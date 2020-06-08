nyccases <- uscases[which(uscases$Admin2=="New York"),3:(n+2)]

nycases <- as.numeric(nyccases)

newcase <- rep(0,n)
newcase[1] <- nycases[1]
newcase[2:n] <- diff(nycases)

rollingcase <- rep(0,n)
for (i in 7:n){
  rollingcase[i] <- mean(newcase[(i-6):i])
}

plot(date,rollingcase,xlab="Date",ylab="New Cases",type="o",
     main="7-Day Moving Averagre - New Cases In NYC")


mean1 = max(rollingcase)
library(radiant.data)
wsd1 <- weighted.sd(1:85,rollingcase[1:85])
pred1 <- rep(0,n)
for (i in 1:n){
  pred1[i] <- mean1*dnorm(i,80,2*wsd1) / dnorm(80,80,2*wsd1)
}
plot(date[50:80],rollingcase[50:80],type="o",ylab="New Daily Cases",xlab="Date",
     main="New Daily Covid-19 Cases in the United States")
lines(date[50:80],pred1[50:80],type="l",col="red")