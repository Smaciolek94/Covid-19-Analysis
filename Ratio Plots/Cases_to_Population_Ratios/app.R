uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:10)]
usdeaths <- usdeaths[,-c(1:10)]

n <- ncol(usdeaths) - 2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

totcases <- sum(uscases[,2:n])
totdeaths <- sum(usdeaths[,3:n])

new <- data.frame(uscases$Combined_Key,usdeaths$Population,totcases,totdeaths)

ratio <- function(location,cd){
    ratiocases <- rep(0,n)
    ratiodeaths <- rep(0,n)
    for (i in 1:n){
        deaths[i] <- 
    }
}