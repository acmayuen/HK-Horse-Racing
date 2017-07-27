##########Call Libreary
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(corrplot)

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


##########Data Visualization
##########Boxplot for variables
par(mfrow=c(2,4))#set margin
boxplot(data.pick$Age,col="red")
boxplot(data.pick$HrWt,col="blue")
boxplot(data.pick$WtCr,col="yellow")
boxplot(data.pick$L1Class,col="green")
boxplot(data.pick$AVG4FinPos,col="red")
boxplot(data.pick$L1Rating,col="blue")
boxplot(data.pick$L1HrWt,col="yellow")
boxplot(data.pick$LastRun,col="green")

##########Correlation Plot
par(mfrow=c(1,1))
correlations <- cor(data.pick[,4:14])# calculate correlations
corrplot(correlations, method="circle")# create correlation plot


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
result.xgb$Couf<-ifelse(result.xgb$FO==1 & result.xgb$rankO==1,1,0)

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
xgb.plot.importance(importance_matrix = importance.xgb)

##########5 Fold Cross Validation
bstcv<-xgb.cv(data = mtrain.xgb, max.depth = 3,
eta = 0.1, nthread = 8, nround = 10,label = output_vector1,
objective = "binary:logistic",nfold=5,prediction = T)
bstcv

xgb.plot.tree(model=bst, trees=0:1, render=T)


##########Using Random Forest##########
##########predicting the probability of winning##########

##########Call Library
library(ranger)
library(randomForest)

##########Call Function for tuning parameter
tune.mtry<-function (mtry, n = 1, ...) 
{result <- sapply(1:n, function(x) {
    sapply(mtry, function(y) {
      rf <- ranger(mtry = y, ...)
      rf$prediction.error})})
  return(result)}

tune.nodesize<-function (min.node.size, n = 1, ...) 
{result <- sapply(1:n, function(x) {
    sapply(min.node.size, function(y) {
      rf <- ranger(min.node.size = y, ...)
      rf$prediction.error})
  })
  return(result)}

##########Splitting Data
train.rf<-data.pick[data.pick$Date<="2009-12-31", ]
test.rf<-data.pick[data.pick$Date>"2009-12-31", ]

##########Transform data frame to matrix
dtrain.rf<-train.rf[,-c(1:3,13,14)]
dtest.rf<-test.rf[,-c(1:3,13,14)]

dtrain.rf$FO<-as.factor(dtrain.rf$FO)
dtest.rf$FO<-as.factor(dtest.rf$FO)

trainodd.rf<-train.rf[,14]
testodd.rf<-test.rf[,14]

##########Optimized mtry
num<-ncol(dtrain.rf)-1
mtry_result<-NULL
for (i in 1:num){
  res<-mean(tune.mtry(mtry=i,n=10,FO~.,data=dtrain.rf))
  names(res)<-i
  mtry_result<-c(mtry_result,res)
}
barplot(mtry_result,xlab="mtry")
best_mtry<-which.min(mtry_result)

##########Optimized node size
num<-10
node_result<-NULL
for (i in 1:num){
  res<-mean(tune.nodesize(min.node.size=i,n=10,FO~.,data=dtrain.rf))
  names(res)<-i
  node_result<-c(node_result,res)
}
barplot(node_result,xlab="nodes")
best_nodesize<-which.min(node_result)

##########Building Random Forrest with Ranger library
pbrf.model<-ranger(FO~.,dtrain.rf,probability = TRUE,num.trees = 10000,mtry = best_mtry,
write.forest = TRUE,min.node.size = best_nodesize,importance="impurity")

##########Prediction
pred.rf<-predict(pbrf.model, dat=dtest.rf)

##########Variable Importance
importance(rf.model)

##########Combine prediction
result.rf<-cbind(test.rf,pred.rf$predictions)
colnames(result.rf)[colnames(result.rf)=="1"] <- "Prob"

##########Ranking for probability
result.rf<-ddply(result.rf,.(Run),transform,rank3=rank(-Prob,ties.method="min"))
result.rf<-ddply(result.rf,.(Run),transform,rankO=rank(FinOdd,ties.method="min"))

result.rf$Cou3<-ifelse(result.rf$FO==1 & result.rf$rank3==1,1,0)
result.rf$Couf<-ifelse(result.rf$FO==1 & result.rf$rankO==1,1,0)

##########Accuracy for Prediction and final odds
sum(result.rf$Cou3)/sum(result.rf$FinPos==1)
sum(result.rf$Couf)/sum(result.rf$FinPos==1)

##########Return from prediction and final odds
result.rf$MR<-ifelse(result.rf$Cou3==1,result.rf$FinOdd,0)
result.rf$FR<-ifelse(result.rf$Couf==1,result.rf$FinOdd,0)
sum(result.rf$MR)
sum(result.rf$FR)


##########Using Support Vector Machine##########
##########predicting the probability of winning##########

##########Call Library
library("Rgtsvm", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")


train.svm<-data.pick[data.pick$Date<="2009-12-31", ]
test.svm<-data.pick[data.pick$Date>"2009-12-31", ]

#train.svm$FO<-ifelse(train.svm$FO==1,"W","L")
#test.svm$FO<-ifelse(test.svm$FO==1,"W","L")

#train.svm$FO<-as.factor(train.svm$FO)
#test.svm$FO<-as.factor(test.svm$FO)

##########Transform data frame to matrix
dtrain.svm<-train.svm[,-c(1:3,12,14)]
dtest.svm<-test.svm[,-c(1:3,12,14)]

dtrainx.svm<-train.svm[,-c(1:3,12,13,14)]
dtrainy.svm<-train.svm[,c(13)]

dtestx.svm<-test.svm[,-c(1:3,12,13,14)]
dtesty.svm<-test.svm[,c(13)]

svm.model<-svm(FinPos~.,data=dtrain.svm,
type = "eps-regression",probaility=T)
#cost = 1,gamma=1)
#,kernel="polynomial")#sigmoid")#cost = 1),
#type = "eps-regression" / "C-classification"

pred.svm<-predict(svm.model,dtestx.svm)
result.svm<-cbind(test.svm,pred.svm)
result.svm<-ddply(result.svm,.(Run),transform,rank4=rank(pred.svm,ties.method="min"))
result.svm<-ddply(result.svm,.(Run),transform,rankO=rank(FinOdd,ties.method="min"))

result.svm$Cou4<-ifelse(result.svm$FinPos==1 & result.svm$rank4==1,1,0)
result.svm$Couf<-ifelse(result.svm$FinPos==1 & result.svm$rankO==1,1,0)

##########Accuracy for Prediction and final odds
sum(result.svm$Cou4)/sum(result.svm$FinPos==1)
sum(result.svm$Couf)/sum(result.svm$FinPos==1)

##########Return from prediction and final odds
result.svm$MR<-ifelse(result.svm$Cou4==1,result.svm$FinOdd,0)
result.svm$FR<-ifelse(result.svm$Couf==1,result.svm$FinOdd,0)
sum(result.svm$MR)
sum(result.svm$FR)


##########Using Support Vector Machine##########
##########predicting the probability of winning##########

##########Stacking Result from 4 models
test.stack<-cbind(test.svm,pred.cl,pred.svm,pred.xgb,pred.rf$predictions[,2])

##########Data conversion
dstackx<-data.matrix(test.stack[,c(15:18)])
dstacky<-test.stack[,c(12)]==1

##########Stacking model using XGBoost
bst.stack<-xgboost(data = dstackx, label = dstacky,nround = 5,objective = "binary:logistic"
,max.depth = 5,eta = 0.8, nthread = 8, gamma=1)

pred.bst.stack<-predict(bst.stack,dstackx)

