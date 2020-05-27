rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))

rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

wkday <- c("W","T","F","S","S","M","T")

countryplot <- function(region,cd){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    regioncase <- rawcases[which(rawcases$Country.Region==region),]
    regiondeath <- rawdeaths[which(rawdeaths$Country.Region==region),]
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
    main4 <- paste("New Deaths in:",region,date[n])
    if (cd=="a"){
        plot(date,cases,main=main1,ylab="cases",type="o",pch=wkday)
    }
    if (cd =="b"){
        plot(date,deaths,main=main2,ylab="deaths",type="o",pch=wkday)
    }
    if (cd=="c"){
        plot(date,newcases,main=main3,ylab="cases",type="o",pch=wkday)
    }
    if (cd=="d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o",pch=wkday)
    }
}

stateplot <- function(region,cd){
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
    main4 <- paste("New Deaths in:",region,date[n])
    if (cd == "a"){
        plot(date,cases,main=main1,ylab="cases",type="o",pch=wkday)
    }
    if (cd == "b"){
        plot(date,deaths,main=main2,ylab="deaths",type="o",pch=wkday)
    }
    if (cd == "c"){
        plot(date,newcases,main=main3,ylab="cases",type="o",pch=wkday)
    }
    if (cd == "d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o",pch=wkday)
    }
}


townplot <- function(town,state,cd){
    location <- paste(town,",",state)
    uscases$Combined_Key <- paste(uscases$Admin2,",",uscases$Province_State)
    usdeaths$Combined_Key <- paste(usdeaths$Admin2,",",usdeaths$Province_State)
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    towncase <- as.numeric(uscases[which(uscases$Combined_Key==location),])
    towndeath <- as.numeric(usdeaths[which(usdeaths$Combined_Key==location),])
    for (i in 1:n){
        cases[i] <- towncase[i+2]
        deaths[i] <- towndeath[i+2]
        if (i > 1){
            newcases[i] <- cases[i] - cases[i-1]
            newdeaths[i] <- deaths[i] - deaths[i-1]
        }
    }
    main1 <- paste("Total Cases in:",location,date[n])
    main2 <- paste("Total Deaths in:",location,date[n])
    main3 <- paste("New Cases in:",location,date[n])
    main4 <- paste("New Deaths in:",location,date[n])
    if (cd=="a"){
        plot(date,cases,main=main1,ylab="cases",type="o",pch=wkday)
    }
    if (cd=="b"){
        plot(date,deaths,main=main2,ylab="deaths",type="o",pch=wkday)
    }
    if (cd=="c"){
        plot(date,newcases,main=main3,ylab="cases",type="o",pch=wkday)
    }
    if (cd=="d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o",pch=wkday)
    }
}

library(shiny)

ui <- fluidPage(
    titlePanel("Personal Use Plots"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4"),
    plotOutput("plot5"),
    plotOutput("plot6")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        countryplot("US","c")
    })
    output$plot2<-renderPlot({
        countryplot("US","d")
    })
    output$plot3<-renderPlot({
        stateplot("New Jersey","c")
    })
    output$plot4<-renderPlot({
        stateplot("New Jersey","d")
    })
    output$plot5<-renderPlot({
        townplot("Mercer","New Jersey","c")
    })
    output$plot6<-renderPlot({
        townplot("Mercer","New Jersey","d")
    })
}

shinyApp(ui = ui,server = server)