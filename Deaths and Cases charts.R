raw <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\time_series_covid_19_deaths.csv",header=TRUE)
raw <- raw[,-c(3:4)]
raw[,3:ncol(raw)] <- as.integer(raw[,3:ncol(raw)])

date <- 1:(ncol(raw)-2)
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

cplot <- function(countr){
  country <- raw[which(raw$Country.Region==countr),]
  daysum <- rep(0,ncol(country)-2)
  daysum <- as.integer(daysum)
  for (i in 3:ncol(country)){
    daysum[i-2] = sum(as.numeric(country[,i]))
  }
  main = paste("Daily Deaths in:",countr)
  plot(date,daysum,main=main,ylab="deaths",type="o")
}
cplot("United States")