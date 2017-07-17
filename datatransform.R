library(readxl)
library(data.table)
#horse <-read.csv("Database 2006-2016 (as at 23 Oct 2016).csv"
#                    ,head=TRUE,stringsAsFactors=FALSE,sep=","
#                    ,na.strings=" ",colClasses=c("character"))
horse<-read_excel("Database 2006-2016 (as at 23 Oct 2016).xlsx")
track<-read_excel("Tables (13 Nov 2016).xlsx",sheet = 1)
time<-read_excel("Tables (13 Nov 2016).xlsx",sheet = 2)
srf<-read_excel("Tables (13 Nov 2016).xlsx",sheet = 3)

lg <- function(x)c(NA, x[1:(length(x)-1)])

ddt <- data.table(data)
ddt[,lvar := lg(Lane), by = c("Name","JC")]

get.mav <- function(bp,n=2){
require(zoo)
if(is.na(bp[1])) bp[1] <- mean(bp,na.rm=TRUE)
bp <- na.locf(bp,na.rm=FALSE)
if(length(bp)<n) return(bp)
c(bp[1:(n-1)],rollapply(bp,width=n,mean,align="right"))  
}

movave<-function(x,)
setDT(ddt)     # converts test to a data.table in place
setkey(ddt,Name,JC)
ddt[,AVGFinPos:=as.numeric(get.mav(FinPos,2)),by=Name]

write.csv(ddt, file = "datatrans1.csv")




############################################

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
outlierKD(data, variable)
