##########Call Libreary
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
library(zoo)

##########Load Functions
#Lag Function for previous race
lg <- function(x,y)c(y, x[1:(length(x)-1)])

#Calculating Moving Average
get.mav <- function(bp,n){
  require(zoo)
  rollapply(bp, width=n,mean,align="right",partial=TRUE,na.rm=TRUE)
}

##########Loading Data
data<-data.table(read_excel("Database 2008-2009.xlsx"))
df<-data
df<-df[with(df, order(Run)), ]

##########Class calibration
#Original: Elite / International - 7, New Horse - 6, Class 1 - 5
#Calibrated: Elite / Inter => 1, Class 1-5 => 2 - 6, New Horse => 7
df$Class.Cal<-ifelse(df$Class==7,1,ifelse(df$Class==1,2,ifelse(df$Class==2,3,ifelse(df$Class==3,4,ifelse(df$Class==4,5,ifelse(df$Class==5,6,ifelse(df$Class==6,7,NA)))))))

##########Upgrade of Downgrade of Class
df[,LagClass := lg(Class.Cal,NA), by = c("Name")]
df$L1Class<-ifelse(is.na(df$LagClass),0,df$Class.Cal-df$LagClass)

##########Finishing Position from last race
df[,L1FinPos := lg(FinPos,NA), by = c("Name")]
Avg.FP<-mean(df$L1FinPos,na.rm=T)
df[,AVG4FinPos:=as.numeric(na.fill(get.mav(L1FinPos,4),Avg.FP)),by=c("Name")]

##########Difference from previous rating
df[,L1Rating := ifelse(lg(Rating,0)==0|Rating==0,0,Rating-lg(Rating,0)), by = c("Name")]

##########Difference from previous horse weight
df[,L1HrWt := ifelse(is.na(HrWt-lg(HrWt,NA)),0,HrWt-lg(HrWt,NA)), by = c("Name")]

##########Day difference from previous race
df$date1<-as.Date(df$Date,origin = "1899-12-30")
df[,date2:=lg(date1,NA),by=c("Name")]
df$date3<-na.fill(as.numeric(df$date1-df$date2),365)
df$LastRun<-ifelse(df$date3>365,365,df$date3)

##########Transform Finishing Position to Binary result
df[,FO:=ifelse(FinPos==1,1,0)]

##########Variable to pick and subset data
var<-c(
"Run","Date","HrNO","Age","HrWt","WtCr",
"L1Class","AVG4FinPos","L1Rating","L1HrWt","LastRun","FO","FinPos","FinOdd")
data.pick<-select(df,one_of(var))

##########Using Conditional Logistic Regression##########
##########predicting the probability of winning##########
#Call Library
library(survival)

##########Convert feature to factor
data.pick$HrNO<-as.factor(data.pick$HrNO)

##########Splitting Train and Test
train.cl<-data.pick[data.pick$Date<="2009-12-31", ]
test.cl<-data.pick[data.pick$Date>"2009-12-31", ]

##########Modelling Conditional Logistic Regression
fit<-clogit(FO~Age+HrWt+WtCr+L1Class+AVG4FinPos+L1Rating+L1HrWt+LastRun
+strata(Run),train.cl)

##########Form a data matrix for prediction
m<-model.matrix(~Age+HrWt+WtCr+L1Class+AVG4FinPos+L1Rating+L1HrWt+LastRun
-1, data=test.cl)

##########Coeficient times data matrix and predicted probability
pp<-exp(m %*% coef(fit))
pps<-ave(pp, test.cl$Run, FUN=sum)
pred.cl<-pp/pps

##########Combining data and predicted probability
result.cl<-cbind(test.cl,pred.cl)
colnames(result.cl)[colnames(result.cl)=="V1"] <- "Prob"

##########Calculating ranking for predicted first place
result.cl<-ddply(result.cl,.(Run),transform,rank=rank(-Prob,ties.method="min"))
result.cl$Cou<-ifelse(result.cl$FinPos<=1 & result.cl$rank<=1,1,0)
Ttl<-ddply(result.cl, .(Run), summarise, Sum1 = sum(Cou))

##########Accuracy for prediction
sum(Ttl$Sum1 == 1)/nrow(Ttl)

##########Return if based on prediction
result.cl$Return<-result.cl$Cou*result.cl$FinOdd
sum(result.cl$Return)

##########Calculating ranking for first place based on final odds
result.cl<-ddply(result.cl,.(Run),transform,rankO=rank(FinOdd,ties.method="min"))
result.cl$Cou2<-ifelse(result.cl$FinPos<=1 & result.cl$rankO<=1,1,0)
Ttl2<-ddply(result.cl, .(Run), summarise, Sum1 = sum(Cou2))

##########Accuracy for favorite odds
sum(Ttl2$Sum1 == 1)/nrow(Ttl2)

##########Return if based on favorable odds
result.cl$Return2<-result.cl$Cou2*result.cl$FinOdd
sum(result.cl$Return2)


##########Using Extreme Gradient Bossting##########
##########predicting the probability of winning##########
#Call Library
require(xgboost)

##########Splitting data
train.xgb<-data.pick[data.pick$Date<="2009-12-31", ]
test.xgb<-data.pick[data.pick$Date>"2009-12-31", ]

##########Transform data frame to matrix
mtrain.xgb<-data.matrix(train.xgb[,-c(1:3,12,13,14)])
mtest.xgb<-data.matrix(test.xgb[,-c(1:3,12,13,14)])

##########Take out final odds
trainodd.xgb<-data.matrix(train.xgb[,14])
testodd.xgb<-data.matrix(test.xgb[,14])

##########Vectorized output
output_vector1 = train.xgb[,"FO"]==1
output_vector2 = test.xgb[,"FO"]==1

##########Extreme gradient boosting training
bst<-xgboost(
data = mtrain.xgb, label = output_vector1,
nround = 1,objective = "binary:logistic")#max.depth = 10,eta = 0.01, nthread = 8, gamma=1)

##########Predict probability
pred.xgb<-predict(bst,mtest.xgb)

##########Combine data
result.xgb<-as.data.frame(cbind(test.xgb,pred.xgb))

##########Ranking for probability
result.xgb<-ddply(result.xgb,.(Run),transform,rank2=rank(-pred.xgb,ties.method="min"))
result.xgb<-ddply(result.xgb,.(Run),transform,rankO=rank(FinOdd,ties.method="min"))
result.xgb$Cou2<-ifelse(result.xgb$FO==1 & result.xgb$rank2==1,1,0)
result.xgb$Couf<-ifelse(result.xgb$FO==1 & result.xgb$rankf==1,1,0)

##########Accuracy for Prediction and final odds
sum(result.xgb$Cou2)/sum(result.xgb$FinPos==1)
sum(result.xgb$Couf)/sum(result.xgb$FinPos==1)

##########Return from prediction and final odds
result.xgb$MR<-ifelse(result.xgb$Cou2==1,result.xgb$FinOdd,0)
result.xgb$FR<-ifelse(result.xgb$Couf==1,result.xgb$FinOdd,0)
sum(result.xgb$MR)
sum(result.xgb$FR)

##########Variable Importance
name.xgb<-colnames(mtrain.xgb)
importance.xgb<- xgb.importance(feature_names = name.xgb, model = bst)
head(importance.xgb,10)
xgb.plot.importance(importance_matrix = importance)

##########5 Fold Cross Validation
bstcv<-xgb.cv(data = mtrain.xgb, max.depth = 3,
eta = 0.1, nthread = 8, nround = 10,label = output_vector1,
objective = "binary:logistic",nfold=5,prediction = T)
bstcv

xgb.plot.tree(model=bst, trees=0:1, render=T)





##########Using Random Forest##########
##########predicting the probability of winning##########
tune.mtry<-function (mtry, n = 1, ...) 
{result <- sapply(1:n, function(x) {
    sapply(mtry, function(y) {
      rf <- ranger(mtry = y, ...)
      rf$prediction.error})})
  return(result)
}

tune.nodesize<-function (min.node.size, n = 1, ...) 
{result <- sapply(1:n, function(x) {
    sapply(min.node.size, function(y) {
      rf <- ranger(min.node.size = y, ...)
      rf$prediction.error})
  })
  return(result)
}





library(ranger)
library(randomForest)

train.rf<-data.pick[data.pick$Date<="2009-12-31", ]
test.rf<-data.pick[data.pick$Date>"2009-12-31", ]

##########Transform data frame to matrix
dtrain.rf<-train.rf[,-c(1:3,13,14)]
dtest.rf<-test.rf[,-c(1:3,13,14)]

trainodd.rf<-train.rf[,14]
testodd.rf<-test.rf[,14]


# 最適なmtry数の推定
num<-ncol(dtrain.rf)-1
mtry_result<-NULL
for (i in 1:num){
  # 10回繰り返した平均を保存
  res<-mean(tune.mtry(mtry=i,n=10,FO~.,data=dtrain.rf))
  names(res)<-i
  mtry_result<-c(mtry_result,res)
}
# 最適なmtry数の確認
barplot(mtry_result,xlab="mtry")
best_mtry<-which.min(mtry_result)
# 最適なノード数の推定
num<-100
node_result<-NULL
for (i in 1:num){
  # 10回繰り返した計算した平均を保存
  res<-mean(tune.nodesize(min.node.size=i,n=10,FO~.,data=dtrain.rf))
  names(res)<-i
  node_result<-c(node_result,res)
}
# 最適なノード数の確認
barplot(node_result,xlab="nodes")
best_nodesize<-which.min(node_result)

rf.model<-ranger(FO~.,dtrain.rf,num.trees = 100,mtry = best_mtry,
write.forest = TRUE,min.node.size = best_nodesize,importance="impurity")

pbrf.model<-ranger(FO~.,dtrain.rf,probability = TRUE,num.trees = 100,mtry = best_mtry,
write.forest = TRUE,min.node.size = best_nodesize,importance="impurity")

importance(rf.model)

rf.predictions <- predict(rf.model, dat=test.org)
pbrf.predictions <- predict(pbrf.model, dat=test.org)
table(test.org$FO, rf.predictions$predictions)
rf.model[9]

