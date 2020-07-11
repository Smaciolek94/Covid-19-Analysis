tests <- read.csv("C:\\Users\\smaci\\Documents\\GitHub\\Covid-19-Analysis\\daily.csv",header=T)
index <- 1:n
tests <- data.frame(tests,index)
tests <- tests[(order(-tests$index)),]

newpositive <- rep(0,n)
newnegative <- rep(0,n)
newpending <- rep(0,n)
newpositive[1] <- tests$positive[1]
newnegative[1] <- tests$negative[1]
newpending[1] <- tests$pending[1]
newpositive[2:n] <- diff(tests$positive)
newnegative[2:n] <- diff(tests$negative)
newpending[2:n] <- diff(tests$pending)

total <- newpositive + newnegative

plot(date,total,main="Daily Covid-19 tests in the United States",
     xlab = "Date",ylab = "Daily Tests",type="l")

percentpositive <- newpositive / total * 100

plot(date[36:n],percentpositive[36:n],main="Percentage of positive Covid-19 tests in the United States",
     xlab = "Date",ylab="Percentage of Positive Tests",type="l")