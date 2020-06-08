rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]

n <- ncol(rawcases)

rate <- function(country){
  case <- sum(rawcases[which(rawcases$Country.Region==country),n])
  death <- sum(rawdeaths[which(rawdeaths$Country.Region==country),n])
  rate <- round((death/case * 100),4)
  return(rate)
}

country <- unique(rawcases$Country.Region)

rates <- sapply(country,rate)

hist(rates,main="Histogram of Fatality Rate by Country",ylab="Number of Countries",
     xlab="Fatality Rate",breaks=50)

rates <- data.frame(country,rates)
colnames(rates) <- c("Country","Fatality_Rate")
print(rates[order(rates)])



order(rates,-rates$Fatality_Rate)
print(rates[1:5])

x <- -30:30
cubic <- function(x){
  -x^3-4*x^2-10*x+10
}
plot(x,cubic(x),type="l",main="Sample Cubic Function",ylab="y")
x<- seq(-5,5,.01)
plot(x,dnorm(x),type="l",main="Sample Guassian Function",ylab="y")

y <- function(x){
  x + 10*rnorm(1)
}
x<- -200:200
plot(x,y(x))

cubicexample <- function(x){
  .01*x^3-.15*x^2+3
}

x<- seq(1,5,.01)
x<- seq(-100,150)
plot(x,cubicexample(x),type="l",ylab="Number of Deaths",
     main = "Cubic Polynomial Fit")
abline(h=0)