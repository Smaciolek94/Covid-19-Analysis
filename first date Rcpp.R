uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:10)]
usdeaths <- usdeaths[,-c(1:10)] #use these for the R code

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

########################################################################################
####################################################################################################
################################################################################################################

uscases <- uscases[,-c(1)]
usdeaths <- usdeaths[,-c(1,2)] #use these for the c++ code
casemat <- as.matrix(uscases)

n<-ncol(uscases)
#trying to make a c++ funtion to speed it up
library(Rcpp)

cppFunction('int first(NumericMatrix uscases, int row){
  int n = uscases.ncol();
  NumericVector temp(n);
  temp = uscases(row,_);
  NumericVector date(n);
  date[0] = 1;
  for (int i=0; i<n; ++i){
    date[i] += i;
  }
  int first = 0;
  for (int i =0;i<n;++i){
    if (temp[i] != 0){
      first = date(i);
      // return first;
    }
    if (temp[i] != 0){break;}
  }
  //int len = first.length();
  return first;
}')

firstdate <- rep(0,nrow(uscases))
for(i in 1:nrow(uscases)-1){
  firstdate[i] <- first(casemat,i)
}
firstdate <- firstdate[which(firstdate!=0)]
firstdate <- as.Date(firstdate,origin = "2020-01-22")
#first <- data.frame(uscases$Combined_Key,first)
hist(firstdate,breaks=n,main="Histogram of the date of first cases in the US",ylab="number of counties/cities",xlab="Date of First Case",freq=TRUE)
