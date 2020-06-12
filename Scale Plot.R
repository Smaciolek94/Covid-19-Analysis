uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]

n <- ncol(uscases) - 2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

location <- unique(uscases$Province_State)

stateplot <- function(state){
  cases <- colSums(uscases[which(uscases$Province_State==state),3:(n+2)])
  newcases <- rep(0,n)
  newcases[1] <- cases[1]
  newcases[2:n] <- diff(cases)
  smoothcases <- rep(0,n)
  for (i in 7:n){
    smoothcases[i] <- mean(newcases[(i-6):i])
  }
  #mx <- max(smoothcases)
  #smoothnorm <- smoothcases / mx
  #if (sum(is.na(smoothnorm)) > 0){
  #  smoothnorm <- rep(0,n)
  #}
  #return(smoothnorm)
  return(smoothcases)
}

plot(date,stateplot("California"),main="California and Texas New cases",
     xlab="Date",ylab="7day MA New Cases",type="l",col="blue")
lines(date,stateplot("Texas"),type="l",col="green")
legend("topleft",legend=c("California","Texas"),col=c("blue","green"),
       pch=c(1,1),lty=c(.5,.5))

plot(date,stateplot("California"),main="New Cases in the 5 Largest States",
     xlab="Date",ylab="7day MA New Cases",type="l",col="blue",ylim=c(0,9500))
lines(date,stateplot("Texas"),type="l",col="green")
lines(date,stateplot("Florida"),type="l",col="red")
lines(date,stateplot("New York"),type="l",col="orange")
lines(date,stateplot("Illinois"),type="l",col="purple")
legend("topleft",legend=c("California","Texas","Florida","New York",
                          "Illinois"),col=c("blue","green","red","orange","purple"),
       pch=c(1,1),lty=c(.5,.5))


plot(date,stateplot("Pennsylvania"),main="New Cases in the next 5 Largest States",
     xlab="Date",ylab="7day MA New Cases",type="l",col="blue")
lines(date,stateplot("Ohio"),type="l",col="green")
lines(date,stateplot("Georgia"),type="l",col="red")
lines(date,stateplot("North Carolina"),type="l",col="orange")
lines(date,stateplot("Michigan"),type="l",col="purple")
legend("topleft",legend=c("Pennsylvania","Ohio","Georgia","North Carolina",
                          "Michigan"),col=c("blue","green","red","orange","purple"),
       pch=c(1,1),lty=c(.5,.5))

plot(date,stateplot("New Jersey"),main="New Cases in the next 5 Largest States",
     xlab="Date",ylab="7day MA New Cases",type="l",col="blue")
lines(date,stateplot("Virginia"),type="l",col="green")
lines(date,stateplot("Washington"),type="l",col="red")
lines(date,stateplot("Arizona"),type="l",col="orange")
lines(date,stateplot("Massachusetts"),type="l",col="purple")
legend("topleft",legend=c("New Jersey","Virginia","Washington","Arizona",
                          "Massachusetts"),col=c("blue","green","red","orange","purple"),
       pch=c(1,1),lty=c(.5,.5))
maxdate <- rep(0,58)

for (i in 1:58){
  temp <- data.frame(date,stateplot(location[i]))
  temp <- temp[(temp[,2]==max(temp[,2])),]
  maxdate[i] <- temp[1,1]
}

for (i in 1:58){
  if(maxdate[i]=="2020-01-22"){
    maxdate <- maxdate[-i]
  }
}

maxdate <- as.Date(maxdate[2:58],origin = "1970-01-01")
hist(maxdate,main="Histogram of States by Their Worst Days"
     ,breaks=20,xlab="Date",ylab="Number of States",freq=TRUE)