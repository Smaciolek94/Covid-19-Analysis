uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:10)]
usdeaths <- usdeaths[,-c(1:10)]

n <- ncol(uscases) - 1
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")
first <- rep(0,nrow(uscases))
for (i in 1:nrow(uscases)){
  temp <- uscases[i,1:n+1]
  temp <- t(temp)
  temp <- as.data.frame(temp)
  temp <- cbind(date,temp)
  tempcut <- temp[which(temp[,2]!=0),]
  first[i] <- tempcut[1,1]
}
first <- as.Date(first,origin = "1970-01-01")
first <- data.frame(uscases$Combined_Key,first)
hist(first$first,breaks=n,main="Histogram of the date of first cases in the US",ylab="number of counties/cities",xlab="Date of First Case",freq=TRUE)

max <- rep(0,nrow(uscases))
for (i in 1:nrow(uscases)){
  newcases <- rep(0,n)
  cases <- as.numeric(uscases[i,1:n+1])
  for (j in 2:n){
    newcases[j] <- cases[j] - cases[j-1]
  }
  temp <- data.frame(date,newcases)
  temp <- temp[order(-newcases),]
  if (temp[1,n] == "2020-01-22"){
    max[i] = 0
  }
  else{
    max[i] <- temp[1,1]
  }
}
max <- as.Date(max,origin = "1970-01-01")
max <- data.frame(uscases$Combined_Key,max)
max <- max[which(max$max!="1970-01-01"),]
hist(max$max,breaks=n/2,main="Histogram of the date of peak new daily cases in the US",ylab="number of counties/cities",xlab="Date of Peak New Cases",freq=TRUE)