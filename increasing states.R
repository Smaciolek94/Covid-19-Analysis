uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(uscases) - 2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

states <- unique(uscases$Province_State)

stateplot <- function(state){
  cases <- colSums(uscases[which(uscases$Province_State==state),3:(n+2)])
  newcases <- rep(0,n)
  newcases[1] <- cases[1]
  newcases[2:n] <- diff(cases)
  smoothcases <- rep(0,n)
  for (i in 7:n){
    smoothcases[i] <- mean(newcases[(i-6):i])
  }
  return(smoothcases)
}

stateplotdeaths <- function(state){
  deaths <- colSums(usdeaths[which(usdeaths$Province_State==state),3:(n+2)])
  newdeaths <- rep(0,n)
  newdeaths[1] <- deaths[1]
  newdeaths[2:n] <- diff(deaths)
  smoothdeaths <- rep(0,n)
  for (i in 7:n){
    smoothdeaths[i] <- mean(newdeaths[(i-6):i])
  }
  return(smoothdeaths)
}

library(dplyr)

increase <- function(state){
  indicator = "Null"
  if(stateplot(state)[n]==0){
    indicator = "No_Cases"
  }
  else if(max(stateplot(state)) %in% stateplot(state)[(n-6):n]){
    indicator = "Increase"
  }
  else{
    indicator = "Decrease"
  }
  return(indicator)
}

increasedeaths <- function(state){
  indicator = "Null"
  if(stateplotdeaths(state)[n]==0){
    indicator = "No_Deaths"
  }
  else if(max(stateplotdeaths(state)) %in% stateplotdeaths(state)[(n-6):n]){
    indicator = "Increase"
  }
  else{
    indicator = "Decrease"
  }
  return(indicator)
}

numbercases = sapply(states,increase)
numberdeaths = sapply(states,increasedeaths)

statenum = data.frame(states,numbercases,numberdeaths)

barplot(table(statenum$numbercases),main="Number of States/Territories with Increasing/Decreasing Cases",
        ylab = "Number of States/Territories")

increasing <- statenum[which(statenum$numbercases=="Increase"),]
decreasing <- statenum[which(statenum$numbercases=="Decrease"),]
up <- rowSums(sapply(increasing$states,stateplot))
down <- rowSums(sapply(decreasing$states,stateplot))

plot(date,down,main="Increasing Vs Decreasing States Cases Total",
     xlab="Date",ylab="Cases",type="l",col="blue")
lines(date,up,type="l",col="red")
legend("topleft",legend=c("Increasing","Decreasing"),col=c("red","blue"),pch=c(1,1))

barplot(table(statenum$numberdeaths),main="Number of States/Territories with Increasing/Decreasing Deaths",
        ylab = "Number of States/Territories")

increasing1 <- statenum[which(statenum$numberdeaths=="Increase"),]
decreasing1 <- statenum[which(statenum$numberdeaths=="Decrease"),]
up1 <- rowSums(sapply(increasing1$states,stateplotdeaths))
down1 <- rowSums(sapply(decreasing1$states,stateplotdeaths))

plot(date,down1,main="Increasing Vs Decreasing States Deaths Total",
     xlab="Date",ylab="Deaths",type="l",col="blue")
lines(date,up1,type="l",col="red")
legend("topleft",legend=c("Increasing","Decreasing"),col=c("red","blue"),pch=c(1,1))