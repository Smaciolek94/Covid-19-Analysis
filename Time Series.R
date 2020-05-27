#importing raw files and trimming them down
uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]
n <- ncol(uscases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")
library(TSA)

names <- unique(uscases$Province_State)

#this function creates a time series of the selected state
cases <- function(state){
  totcase <- colSums(uscases[which(uscases$Province_State == state),3:(n+2)])
  newcase <- rep(0,n)
  newcase[1] <- totcase[1]
  newcase[2:n] <- diff(totcase)
  newcase <- ts(data=newcase,start=c(2020,01,22),frequency = 365)
  return(newcase)
}

#test
plot(cases("New York"))

#this function performs EDA on the state time series to inform model building
prelim <- function(state){
  series <- cases(state)
  dif <- diff(series)
  weekdif <- diff(series,7)
  plot(series,main="undifferenced time series")
  plot(dif,main="first difference of time series")
  plot(weekdif,main="first weekly difference of time series")
  acf(as.vector(series),main=paste(state,"ACF"))
  pacf(as.vector(series),main=paste(state,"PACF"))
  acf(as.vector(dif),main=paste(state,"1st difference ACF"))
  pacf(as.vector(dif),main=paste(state,"1st difference PACF"))
  acf(as.vector(weekdif),main=paste(state,"1st weekly difference ACF"))
  pacf(as.vector(weekdif),main=paste(state,"1st weekly difference PACF"))
  periodogram(series,main="Periodogram of time series")
}
prelim("New York")

model<-arima(cases(state),order=c(1,0,0),seasonal=list(order=c(0,0,0),period=7))
coefupper <- model$coef + diag(sqrt(model$sigma2))
coeflower <- model$coef - diag(sqrt(model$sigma2))
model("New York")

time <- sapply(names,cases)

model<-arima(cases("New York"),order=c(1,1,1),seasonal=list(order=c(0,0,0),period=7))

diag<-function(model){
  resid <- model$residuals
  plot(resid,main="Final Model Residuals")
  qqnorm(resid,main="QQplot of Residuals")
  hist(resid,main="Histogram of Residuals")
  acf(as.vector(resid),main="ACF of Residuals")
}
diag(model)