uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(uscases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

totcase <- colSums(uscases[,3:(n+2)])
totdeath <- colSums(usdeaths[,3:(n+2)])
newcase <- rep(0,n)
newdeath <- rep(0,n)
newcase[1] <- totcase[1]
newdeath[1] <- totdeath[1]
newcase[2:n] <- diff(totcase)
newdeath[2:n] <- diff(totdeath)

rollingcase <- rep(0,n)
rollingdeath <- rep(0,n)
for (i in 7:n){
  rollingcase[i] <- mean(newcase[(i-6):i])
  rollingdeath[i] <- mean(newdeath[(i-6):i])
}

wkday <- c("W","T","F","S","S","M","T")

plot(date,newcase,type="o",ylab="New Cases",xlab="Date",pch=wkday,
     main="New Cases in the United States")


plot(date,newdeath,type="o",ylab="New Deaths",xlab="Date",pch=wkday,
     main="New Deaths in the United States")

plot(date,rollingcase,type="o",ylab="New Cases",xlab="Date",
     main="New Cases in the United States")

plot(date,rollingdeath,type="o",ylab="New Deaths",xlab="Date",
     main="New Deaths in the United States")

rollingcase <- rep(0,n)
rollingdeath <- rep(0,n)
for (i in 30:n){
  rollingcase[i] <- mean(newcase[(i-29):i])
  rollingdeath[i] <- mean(newdeath[(i-29):i])
}

plot(date,rollingcase,type="o",ylab="New Cases",xlab="Date",
     main="New Cases in the United States")

plot(date,rollingdeath,type="o",ylab="New Deaths",xlab="Date",
     main="New Deaths in the United States")