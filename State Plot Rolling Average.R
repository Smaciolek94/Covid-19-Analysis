uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]

n <- ncol(uscases) - 2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

stateplot <- function(state){
  cases <- colSums(uscases[which(uscases$Province_State==state),3:(n+2)])
  newcases <- rep(0,n)
  newcases[1] <- cases[1]
  newcases[2:n] <- diff(cases)
  smoothcases <- rep(0,n)
  for (i in 7:n){
    smoothcases[i] <- mean(newcases[(i-6):i])
  }
  mn = paste("New Cases In",state)
  plot(date,smoothcases,main=mn,xlab="Date",ylab="New Cases",type="o")
}

stateplot("California")
stateplot("Texas")
stateplot("Florida")
stateplot("New York")
stateplot("Illinois")