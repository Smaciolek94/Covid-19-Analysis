#updating to make it easier for shiny
#importing data from Johns Hopkins Github
library(httr)
rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
#old csv import as a backup
#rawcases <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\Copy of time_series_covid_19_confirmed_4_8.csv",header=TRUE)
#rawdeaths <- read.csv("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\Copy of time_series_covid_19_deaths_4_8_update.csv",header=TRUE)
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]
#creating date vector and n dates for simplicity
n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

countryplot <- function(region){
  cases <- rep(0,n)
  newcases <- rep(0,n)
  deaths <- rep(0,n)
  newdeaths <- rep(0,n)
  cases <- as.numeric(cases)
  newcases <- as.numeric(newcases)
  deaths <- as.numeric(deaths)
  newdeaths <- as.numeric(newdeaths)
  regioncase <- rawcases[which(rawcases$Country.Region==region),]
  regiondeath <- rawdeaths[which(rawdeaths$Country.Region==region),]
  regioncase <- as.numeric(regioncase)
  regiondeath <- as.numeric(regiondeath)
  for (i in 1:n){
    cases[i] <- sum(regioncase[i+2])
    deaths[i] <- sum(regiondeath[i+2])
    if (i > 1){
      newcases[i] <- cases[i] - cases[i-1]
      newdeaths[i] <- deaths[i] - deaths[i-1]
    }
  }
  newcases[1] <- cases[1]
  newdeaths[1] <- deaths[1]
  main1 <- paste("Total Cases in:",region,date[n])
  main2 <- paste("Total Deaths in:",region,date[n])
  main3 <- paste("New Cases in:",region,date[n])
  main4 <- paste("New Deaths in",region,date[n])
  plot(date,cases,main=main1,ylab="cases",type="o")
  plot(date,deaths,main=main2,ylab="deaths",type="o")
  plot(date,newcases,main=main3,ylab="cases",type="o")
  plot(date,newdeaths,main=main4,ylab="deaths",type="o")
}

stateplot <- function(region){
  cases <- rep(0,n)
  newcases <- rep(0,n)
  deaths <- rep(0,n)
  newdeaths <- rep(0,n)
  cases <- as.numeric(cases)
  newcases <- as.numeric(newcases)
  deaths <- as.numeric(deaths)
  newdeaths <- as.numeric(newdeaths)
  regioncase <- uscases[which(uscases$Province_State==region),]
  regiondeath <- usdeaths[which(usdeaths$Province_State==region),]
  #regioncase <- as.numeric(regioncase)
  #regiondeath <- as.numeric(regiondeath)
  for (i in 1:n){
    cases[i] <- sum(regioncase[i+2])
    deaths[i] <- sum(regiondeath[i+2])
    if (i > 1){
      newcases[i] <- cases[i] - cases[i-1]
      newdeaths[i] <- deaths[i] - deaths[i-1]
    }
  }
  newcases[1] <- cases[1]
  newdeaths[1] <- deaths[1]
  main1 <- paste("Total Cases in:",region,date[n])
  main2 <- paste("Total Deaths in:",region,date[n])
  main3 <- paste("New Cases in:",region,date[n])
  main4 <- paste("New Deaths in",region,date[n])
  plot(date,cases,main=main1,ylab="cases",type="o")
  plot(date,deaths,main=main2,ylab="deaths",type="o")
  plot(date,newcases,main=main3,ylab="cases",type="o")
  plot(date,newdeaths,main=main4,ylab="deaths",type="o")
}

townplot <- function(county,region){
  cases <- rep(0,n)
  newcases <- rep(0,n)
  deaths <- rep(0,n)
  newdeaths <- rep(0,n)
  cases <- as.numeric(cases)
  newcases <- as.numeric(newcases)
  deaths <- as.numeric(deaths)
  newdeaths <- as.numeric(newdeaths)
  towncase <- uscases[which(uscases$Province_State==region),]
  towncase <- towncase[which(towncase$Admin2==county),]
  towndeath <- usdeaths[which(uscases$Province_State==region),]
  towndeath <- towndeath[which(towndeath$Admin2==county),]
  for (i in 1:n){
    cases[i] <- sum(towncase[i+2])
    deaths[i] <- sum(towndeath[i+2])
    if (i > 1){
      newcases[i] <- cases[i] - cases[i-1]
      newdeaths[i] <- deaths[i] - deaths[i-1]
    }
  }
  main1 <- paste("Total Cases in:",county,region,date[n])
  main2 <- paste("Total Deaths in:",county,region,date[n])
  main3 <- paste("New Cases in:",county,region,date[n])
  main4 <- paste("New Deaths in",county,region,date[n])
  plot(date,cases,main=main1,ylab="cases",type="o")
  plot(date,deaths,main=main2,ylab="deaths",type="o")
  plot(date,newcases,main=main3,ylab="cases",type="o")
  plot(date,newdeaths,main=main4,ylab="deaths",type="o")
}
