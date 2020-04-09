#data from 4/2/2020
raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\covid-cases-worldwide.csv",header=TRUE)
rawchina <- raw[which(raw$Country.Region=="China"),]
daytotal <- rep(0,ncol(rawchina)-4)
for (i in 5:ncol(rawchina)){
  daytotal[(i-4)] <- sum(rawchina[,i])
}
date <- 1:(ncol(raw)-4)
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")
plot(date,daytotal,main="Total Cases in China by Day",ylab = "cases",type="o")

newdaytotal <- rep(0,length(daytotal))
for (i in 2:length(daytotal)){
  newdaytotal[i] <- daytotal[i] - daytotal[i-1]
}
plot(date,newdaytotal,main="New Cases in China by Day",ylab = "cases",type="o")

rawitaly <- raw[which(raw$Country.Region=="Italy"),]
totitaly <- rawitaly[1,5:ncol(rawitaly)]
totitaly <- as.numeric(totitaly)
plot(date,totitaly,main = "Total Cases in Italy by Day",ylab = "cases",type="o")
newdayitaly <- rep(0,length(totitaly)) 
for (i in 2:length(totitaly)){
  newdayitaly[i] <- totitaly[i] - totitaly[i-1]
}
plot(date,newdayitaly,main="New Cases in Italy by Day",ylab="cases",type="o")

#South Korea and Japan
rawkorea <- raw[which(raw$Country.Region=="South Korea"),]
rawjapan <- raw[which(raw$Country.Region=="Japan"),]
daytotalkorea <- rep(0,ncol(rawkorea)-4)
daytotaljapan <- rep(0,ncol(rawjapan)-4)
newdaykorea <- rep(0,ncol(rawkorea)-4)
newdayjapan <- rep(0,ncol(rawjapan)-4)
for (i in 5:ncol(rawchina)){
  daytotalkorea[(i-4)] <- sum(rawkorea[,i])
  daytotaljapan[(i-4)] <- sum(rawjapan[,i])
}
plot(date,daytotalkorea,main="South Korea Total Cases")
plot(date,daytotaljapan,main="Japan Total Cases")

#just one obsercation for japan, none for SK
plot(date,rawjapan[5:75],main="cases in Japan as of 4/2",ylab="cases",type="o")