#data as of 4/3

raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\US_confirmed_4-3.csv",header=TRUE)
raw <- raw[,-c(1:5,8:11)] #removing nonimportant columns
rawwash <- raw[which(raw$Province_State == "Washington"),]
washsum <- rep(0,(ncol(raw)-2))
washdaysum <- rep(0,(ncol(raw)-2))
for (i in 1:(ncol(raw)-2)){
  washsum[i] <- sum(rawwash[,i+2])
  if (i > 1){
    washdaysum[i] <- washsum[i] - washsum[i-1]
  }
}

date <- 1:(ncol(raw)-2)
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

plot(date,washsum,ylab="cases",main="Washington State Cases as of 4/3",sub="new day cases in blue",type="o",col="black")
lines(date,washdaysum,type="o",col="blue")

plot(date,washdaysum,main="New cases by day Washington State",ylab="cases",type="o")

rawsea <- raw[which(raw$Admin2=="King"&raw$Province_State=="Washington"),]
rawnyc <- raw[which(raw$Admin2=="New York"),]
plot(date,rawnyc[3:ncol(rawnyc)],main="Seattle and New York Cases",sub="black = NYC, blue = Seattle",ylab="cases",type="o")
plot(date,rawsea[3:ncol(rawsea)],main="Seattle Cases",type="o",col="blue")
