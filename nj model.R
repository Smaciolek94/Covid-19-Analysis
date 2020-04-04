#Data as of 4/3

raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\US_confirmed_4-3.csv",header=TRUE)
raw <- raw[,-c(1:5,8:11)] #removing nonimportant columns
rawnj <- raw[which(raw$Province_State=="New Jersey"),]
daynj <- rep(0,(ncol(rawnj)-2))
daynewnj <- rep(0,(ncol(rawnj)-2))
for (i in 1:(ncol(rawnj)-2)){
  daynj[i] <- sum(rawnj[,i+2])
  if (i>2){
    daynewnj[i] <- daynj[i] - daynj[i-1]
  }
}
daynewnj[1] <- daynj[1]

date <- 1:(ncol(raw)-2)
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

#preliminary plots:
plot(date,daynewnj,main="New Daily Cases in NJ",ylab="cases",type="o")
plot(date,daynj,main="Total Daily Cases in NJ",ylab="cases",type="o")

plot(date,log(daynewnj),main="log plot of New Daily Cases in NJ",ylab="cases",type="o")
plot(date,log(daynj),main="log plot of Total Daily Cases in NJ",ylab="cases",type="o")

model <- function(degree,log){
  if (log == "no"){
    p <- lm(daynj~poly(date,degree))
    print(summary(p))
    main = paste("observed vs predicted, poly order: ",i)
    plot(date,daynj,main = main,ylab="cases",col="black")
    lines(date,predict(p),col="blue")
    plot(date,p$residuals,main="residual plot")
    qqnorm(p$residuals,main="qqplot")
  }
  if (log == "yes"){
    logdaynj <- log(.1*daynj+1)
    p <- lm(logdaynj~poly(date,degree))
    print(summary(p))
    main = paste("observed vs predicted, log order :",i)
    plot(date,logdaynj,main = "observed vs predicted",ylab="cases",col="black")
    lines(date,predict(p),col="blue")
    plot(date,p$residuals,main="residual plot")
    qqnorm(p$residuals,main="qqplot")
  }
}
for (i in 1:10){
  model(i,"yes")
}