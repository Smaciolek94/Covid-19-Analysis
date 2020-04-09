#data as of 4/1/2020

#importing
raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\covid_cases.csv",header= TRUE)
raw <- as.data.frame(raw)
#converting case counts to intergers
raw[,12:ncol(raw)] <- as.integer(unlist(raw[,12:ncol(raw)]))
daysums <- rep(0,(ncol(raw)-11))
#converting lat/long values to intergers
raw[,8:9] <- as.numeric(unlist(raw[,8:9]))
#summing case counts by day
for (i in 12:ncol(raw)){
  daysums[(i-11)] <- sum(raw[,i])
}
#creating a date vector
date <- 1:(ncol(raw)-11)
date <- as.Date(date,origin = "2020-01-20")
format(date,format = "%b %d %y")
#time plot of total cases by day
plot(date,daysums,main="total US cases as of 3/30",ylab = "new cases",xlab = "date",type="o")
placesum <- rep(0,nrow(raw))
#summing the cases by place
for (i in 1:nrow(raw)){
  placesum[i] <- sum(raw[i,12:ncol(raw)])
}
#attaching place sum to raw data
raw <- cbind(raw,placesum)
#creating a new data frame and sorting by number of infections by place
placelist <- data.frame(as.character(raw$Admin2),as.character(raw$Province_State),raw$placesum)
placelist <- placelist[order(-placesum),]
rank <- 1:nrow(placelist)
placelist <- data.frame(rank,placelist)
#list of case counts by place
colnames(placelist)=c("Rank","City","State","Total cases as of 3/30")
#printing 10 worse areas
#print("Top 10 worst affected areas")
#print(placelist[1:10,],row.names=FALSE,class=FALSE,right=TRUE)
write.csv(placelist,"C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\Ranked_Places.csv",row.names=FALSE)
#creating a new dataframe by number of cases
rawbycase <- raw[order(-placesum),]
#time plots of the 10 worst places
for (i in 1:10){
  main = paste("Cases in",rawbycase$Admin2[i],",",rawbycase$Province_State[i])
  trimmedcase <- as.numeric(rawbycase[i,12:80])
  plot(date,trimmedcase,main = main,type="o",xlab="date",ylab="cases")
}
plot(rawbycase$placesum[1:10],main="Chart of infected places",ylab="cases",xlab="rank (#1 means worst hit)",type="o")

ntrys <- 200
ratio <- rep(0,ntrys)
for (i in 1:ntrys){
  ratio[i] <- rawbycase$placesum[1] / rawbycase$placesum[i]
}
plot(ratio,main="ratio of number of cases to worst",type="o",xlab="rank (#1 means worst hit)",ylab="ratio")
rank <- 1:ntrys
#for (i in 1:10){
#ratiomodel <- lm(ratio~poly(rank,i))
#plot(ratiomodel$residuals)
#qqnorm(ratiomodel$fitted.values)
#plot(ratio,main="ratio of number of cases to worst",type="l",col="black",xlab="rank (#1 means worst hit)",ylab="ratio")
#lines(ratiomodel$fitted.values,type="l",col="blue")
}

ratiomodel <- lm(ratio~poly(rank,5))
summary(ratiomodel)

plot(ratiomodel$residuals,main="residual plot, 5th order poly",ylab="residuals",xlab="rank")
qqnorm(ratiomodel$fitted.values,main = "qqplot of 5th order poly fit")
plot(ratio,main="ratio of number of cases to worst",type="l",col="black",xlab="rank (#1 means worst hit)",ylab="ratio")
lines(ratiomodel$fitted.values,type="l",col="blue")


ratioprev <- rep(1,100)
for (i in 2:100){
  ratioprev[i] <- rawbycase$placesum[i-1] / rawbycase$placesum[i]
}
plot(ratioprev[1:100],main="ratio of number of cases to previous",type="o",xlab="rank (#1 means worst hit)",ylab="ratio")
lines(ratio)

#raw ploynomial fits
lm1 <- lm(daysums~poly(date,1))
lm1 <- lm(daysums~poly(date,2))
lm3 <- lm(daysums~poly(date,3))
lm4 <- lm(daysums~poly(date,4))
plot(lm1$residuals)
plot(lm2$residuals)
plot(lm3$residuals)
plot(lm4$residuals)

#polynomal log transformed fits
lm1 <- lm(log(daysums)~poly(date,1))
lm2 <- lm(log(daysums)~poly(date,2))
lm3 <- lm(log(daysums)~poly(date,3))
lm4 <- lm(log(daysums)~poly(date,4))
plot(lm1$residuals)
plot(lm2$residuals)
plot(lm3$residuals)
plot(lm4$residuals)

#cubic spline fit
library(splines)
spline <- lm(daysums ~ bs(date,knots=40))
plot(spline$residuals)
logspline <- lm(log(daysums) ~ bs(date,knots=40))
plot(logspline$residuals)

#fit with a smoothing spline
smoothspline <- smooth.spline(date,daysums,df=16)
plot(date,unlist(smoothspline$y),type="b",col="green",ylab = "number of cases",main="Cubic Smooth Spline Fit")
lines(date,daysums,type="l",col="red")

#fit with nonparametric regression
kernel <- ksmooth(date,daysums,kernel = "normal",n.points=length(date))
plot(date,kernel$y,type="b",col="blue",main = "Kernel Regression Fit",ylab="number of cases")
lines(date,daysums,type="l",col="red")

smoothkern <- function(place){
  main1 = paste("Smooth cubic spline fit of cases in",rawbycase$Admin2[place],",",rawbycase$Province_State[place])
  main2 = paste("Kernel Regression fit of cases in",rawbycase$Admin2[place],",",rawbycase$Province_State[place])
  smoothspline <- smooth.spline(date,rawbycase[place,12:80],df=16)
  plot(date,unlist(smoothspline$y),type="l",col="green",ylab = "number of cases",main=main1)
  lines(date,rawbycase[place,12:80],type="b",col="red")
  
  kernel <- ksmooth(date,rawbycase[place,12:80],kernel = "normal",n.points=length(date))
  plot(date,kernel$y,type="l",col="blue",main = main2,ylab="number of cases")
  lines(date,rawbycase[place,12:80],type="b",col="red")
}
for (i in 1:10){smoothkern(i)}