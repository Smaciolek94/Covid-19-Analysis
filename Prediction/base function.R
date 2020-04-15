uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(uscases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

statenames <- unique(uscases$Province_State)

model <- function(state,nahead){
  statecase <- uscases[which(uscases$Province_State==state),]
  cases <- rep(0,n)
  for (i in 1:n){
    cases[i]<- sum(statecase[,i+2])
  }
  model <- lm(cases~poly(date,5))
  pred <- rep(0,nahead)
  for (i in 1:nahead){
    pred[i] <- model$coefficients[1] + model$coefficients[2]*i +model$coefficients[3]*i^2
      +model$coefficients[4]*i^3 + model$coefficients[5]*i^4+model$coefficients[6]*i^5
  }
  main = paste("Predicted Cases in",state,nahead,"days out")
  datenew <- 1:(n+nahead)
  datenew <- as.Date(datenew,origin = "2020-01-21")
  format(datenew,format = "%b %d %y")
  cases = c(cases,pred)
  plot(datenew,cases,main=main,type="p",col=c(rep("black",n),rep("red",nahead)))
}

library(shiny)

#cubic spline fit
library(splines)
spline <- lm(cases ~ bs(date,knots=n))


plot(spline$residuals)
logspline <- lm(log(daysums) ~ bs(date,knots=40))
plot(logspline$residuals)