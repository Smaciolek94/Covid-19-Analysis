p<-rnorm(1000)
hist(p)

plot(dnorm,xlim=c(-3,3),main="Sample Normal Distribution",xlab="",ylab="")
abline(v=0)


plot(dnorm(seq(-8,8,.01),0,4),xlim=c(-8,8),main="Sample Normal Distributions", xlab="",ylab="")
lines(dnorm)

uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]

n <- ncol(uscases)-2

date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

totcase <- colSums(uscases[,3:(n+2)])
newcase <- rep(0,n)
newcase[1] <- totcase[1]
newcase[2:n] <- diff(totcase)

rollingcase <- rep(0,n)
for (i in 7:n){
  rollingcase[i] <- mean(newcase[(i-6):i])
}

plot(date,newcase,type="o",xlab="Date",ylab="New Daily Cases",
     main="Daily New Cases in the United States")

plot(date,rollingcase,type="o",xlab="Date",ylab="New Daily Cases",
     main="7-Day Moving Average Daily New Cases in the United States")

plot(date,totcase,main="Cumulative Covid-19 Cases in the United States",
     ylab="Cumulative Cases",xlab="Date",type="o")

plot(date[50:80],rollingcase[50:80],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")

mean1 = max(rollingcase[50:80])
mean1 = max(rollingcase)
library(radiant.data)
wsd1 <- weighted.sd(50:80,rollingcase[50:80])
pred1 <- rep(0,n)
for (i in 1:n){
  pred1[i] <- mean1*dnorm(i,80,2*wsd1) / dnorm(80,80,2*wsd1)
}
plot(date[50:80],rollingcase[50:80],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:80],pred1[50:80],type="l",col="red")


plot(date[50:n],rollingcase[50:n],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:n],pred1[50:n],type="l",col="red")

begin<- function(end){
  mean1 = max(rollingcase[50:end])
  wsd1 <- weighted.sd(50:end,rollingcase[50:end])
  pred1 <- rep(0,n)
  for (i in 1:n){
    pred1[i] <- mean1*dnorm(i,end,2*wsd1) / dnorm(end,end,2*wsd1)
  }
  plot(date[50:end],rollingcase[50:end],type="o",ylab="New Daily Cases",xlab="Date",
       main="New Daily Covid-19 Cases in the United States")
  lines(date[50:end],pred1[50:end],type="l",col="red")
}

begin2 <- function(end){
  wsd1 <- weighted.sd(50:end,rollingcase[50:end])
  pred1 <- rep(0,n)
  for (i in 1:n){
    pred1[i] <- mean1*dnorm(i,80,2*wsd1) / dnorm(80,80,2*wsd1)
  }
  plot(date[50:end],rollingcase[50:end],type="o",ylab="New Daily Cases",xlab="Date",
       main="New Daily Covid-19 Cases in the United States")
  lines(date[50:end],pred1[50:end],type="l",col="red")
}

plot(date[50:n],rollingcase[50:n],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:n],pred1[50:n],type="l",col="red")

mean = max(rocase[50:n])
library(radiant.data)
wsd <- weighted.sd(50:n,newcase[50:n])
pred <- rep(0,n)
for (i in 1:n){
  pred[i] <- mean*dnorm(i,94,2*wsd) / dnorm(94,94,2*wsd)
}

plot(date[50:n],newcase[50:n],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:n],pred[50:n],type="l",col="red")



mean = max(newcase)
library(radiant.data)
wsd <- weighted.sd(1:n,newcase)
pred <- rep(0,n)
for (i in 1:n){
  pred[i] <- mean*dnorm(i,94,2*wsd) / dnorm(94,94,2*wsd)
}

plot(date,newcase,type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date,pred,type="l",col="red")

mean = max(rollingcase[80:n])
library(radiant.data)
wsd <- weighted.sd(1:n,rollingcase)
pred <- rep(0,n)
for (i in 1:n){
  pred[i] <- mean*dnorm(i,80,2*wsd) / dnorm(80,80,2*wsd)
}

wsd <- weighted.sd(1:n,rollingcase[80:n])
pred <- rep(0,n)
for (i in 1:n){
  pred[i] <- mean*dnorm(i,80,2*wsd) / dnorm(80,80,2*wsd)
}

plot(date[50:n],rollingcase[50:n],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:n],pred[50:n],type="l",col="red")

plot(date,newcase,type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date,pred,type="l",col="red")

plot(date,pred1,type="l",col="blue",ylab="New Daily Cases",xlab="Date",
     main="Predicted Models for New Covid-19 Cases in the US")
lines(date,pred,type="l",col="red")

pred2<- dnorm(1:n,100,wsd1) * sum(pred)

plot(date,pred2,type="l",col="blue",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date,pred,type="l",col="red")

mean2 = max(rollingcase[96:n])
library(radiant.data)
wsd2 <- weighted.sd(1:n,rollingcase[1:n])
pred2 <- rep(0,n)
for (i in 1:n){
  pred2[i] <- mean2*dnorm(i,96,2*wsd2) / dnorm(96,96,2*wsd2)
}

plot(date[50:n],rollingcase[50:n],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:n],pred2[50:n],type="l",col="red")