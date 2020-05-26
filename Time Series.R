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

#total cases:
cases <- function(state){
  totcase <- colSums(uscases[which(uscases$Province_State == state),3:(n+2)])
  newcase <- rep(0,n)
  newcase[1] <- totcase[1]
  newcase[2:n] <- diff(totcase)
  newcase <- ts(data=newcase,start=c(2020,01,22),frequency = 365)
  return(newcase)
}

plot(cases("New York"))

time <- sapply(names,cases)

model<-arima(cases("New York"),order=c(1,0,0),seasonal=list(order=c(1,0,1),period=7))
