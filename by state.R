uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:11)]

state <- unique(as.character(uscases$Province_State))
n <- ncol(uscases) - 2
m <- length(state)

sum <- rep(0,m*n)
dim(sum) <- c(m,n)
for (i in 1:m){
  for (j in 3:n+2){
    temp <- uscases[which(uscases$Province_State==state[i]),]
    sum[i,j] <- sum(temp[,j])
  }
}