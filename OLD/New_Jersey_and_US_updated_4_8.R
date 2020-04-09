#data as of 4/8/2020

raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\Copy of time_series_covid_19_confirmed_US_Updated_4_8.csv",header=TRUE)
raw[,12:ncol(raw)] <- as.integer(unlist(raw[,12:ncol(raw)]))
#summing case counts by day
daysums <- rep(0,(ncol(raw)-11))
for (i in 12:ncol(raw)){
  daysums[(i-11)] <- sum(raw[,i])
}

#creating a date vector
date <- 1:(ncol(raw)-11)
date <- as.Date(date,origin = "2020-01-20")
format(date,format = "%b %d %y")
#time plot of total cases by day
plot(date,daysums,main="total US cases as of 4/7",ylab = "new cases",xlab = "date",type="o")

rawnj <- raw[which(raw$Province_State == "New Jersey"),]
#summing nj case counts by day
daysumsnj <- rep(0,(ncol(rawnj)-11))
for (i in 12:ncol(rawnj)){
  daysumsnj[(i-11)] <- sum(rawnj[,i])
}
plot(date,daysumsnj,main="total NJ cases as of 4/7",ylab = "new cases",xlab = "date",type="o")

ratio <- daysumsnj / daysums
plot(date,ratio,main="proportion of total US cases in NJ",type="o")

rawny <- raw[which(raw$Province_State == "New York"),]
#summing nj case counts by day
daysumsny <- rep(0,(ncol(rawny)-11))
for (i in 12:ncol(rawny)){
  daysumsny[(i-11)] <- sum(rawny[,i])
}
plot(date,daysumsny,main="total NY cases as of 4/2",ylab = "new cases",xlab = "date",type="o")

rationy <- daysumsny / daysums
plot(date,rationy,main="proportion of total US cases in NY",type="o")

plot(date,daysums,main = "Total Cases: US, NJ, and NY",sub="Black = US total, Red = New York State, Blue = New Jersey",ylab="cases",type="o",col="black")
lines(date,daysumsnj,type="o",col="blue")
lines(date,daysumsny,type="o",col="red")

casesnyc <- as.vector(raw[which(raw$Admin2 == "New York"),12:ncol(raw)])
casesphilly <- as.vector(raw[which(raw$Admin2 == "Philadelphia"),12:ncol(raw)])
casesla <- as.vector(raw[which(raw$Admin2 == "Los Angeles"),12:ncol(raw)])
casesSF <- as.vector(raw[which(raw$Admin2 == "San Francisco"),12:ncol(raw)])
casesdallas <- as.vector(raw[which(raw$Admin2 == "Dallas"),12:ncol(raw)])
casesdallas <- casesdallas[5,]
casessea <- as.vector(raw[which(raw$Admin2 == "Seatle"),12:ncol(raw)])


plot(date,daysumsny,main = "Total NY State / NYC cases",ylab = "cases",type="o",col="black")
lines(date,casesnyc,type="o",col="blue")

rawinternational <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\covid-cases-worldwide.csv",header=TRUE)
rawchina <- rawinternational[which(rawinternational$Country.Region=="China"),]
daychinatotal <- rep(0,ncol(rawchina)-4)
for (i in 5:ncol(rawchina)){
  daychinatotal[(i-4)] <- sum(rawchina[,i])
}
newdaychinatotal <- rep(0,length(daychinatotal))
newdayustotal <- rep(0,length(daysums)-1)
for (i in 2:length(daychinatotal)){
  newdaychinatotal[i] <- daychinatotal[i] - daychinatotal[i-1]
  newdayustotal[i] <- daysums[i] - daysums[i-1]
}


plot(date[1:71],newdaychinatotal,main="New Daily China vs US cases",xlab="date",ylab="cases",type="o",col="black",ylim=c(0,25000))
lines(date[1:71],newdayustotal,type="o",col="blue")

newdayustotal <- rep(0,length(daysums))
for (i in 2:length(daysums)){
  newdayustotal[i] <- daysums[i] - daysums[i-1]
}
plot(date,newdayustotal,main="Daily New Cases in the US",ylab="cases",type="o")

plot(date,casesdallas,ylim=c(0,25000),main="Cases in selected US cities as of 4/2",sub="Black = NYC, Blue = LA, Red = Phila, Green = San Fran, Yellow = Dallas",type="o",col="yellow")
lines(date,casesla,type="o",col="blue")
lines(date,casesphilly,type="o",col="red")
lines(date,casesSF,type="o",col="green")
lines(date,casesnyc,type="o",col="black")
#data not available for Phoenix, Chicago, Houston

propinfected <- casesnyc / 8623000
plot(date,propinfected,main="proportion of total NYC population infected",ylab="proportion infected",type="o")