#file importer:
rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]

countrynames <- unique(rawcases$Country.Region)  

n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

countryplot <- function(region,cd){
    cases <- rep(0,n)
    deaths <- rep(0,n)
    newcases <- rep(0,n)
    newdeaths <- rep(0,n)
    rollingcase <- rep(0,n)
    rollingdeath <- rep(0,n)
    regioncase <- rawcases[which(rawcases$Country.Region==region),]
    regiondeath <- rawdeaths[which(rawdeaths$Country.Region==region),]
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
    for (i in 7:n){
        rollingcase[i] <- mean(newcases[(i-6):i])
        rollingdeath[i] <- mean(newdeaths[(i-6):i])
    }
    rollingcase[1:6] <- newcases[1:6]
    rollingdeath[1:6] <- newdeaths[1:6]
    main1 <- paste("Total Cases in:",region,date[n])
    main2 <- paste("Total Deaths in:",region,date[n])
    main3 <- paste("7 Day MA New Cases in:",region,date[n])
    main4 <- paste("7 Day MA New Deaths in:",region,date[n])
    if (cd=="a"){
        plot(date,cases,main=main1,ylab="cases",type="o")
    }
    if (cd =="b"){
        plot(date,deaths,main=main2,ylab="deaths",type="o")
    }
    if (cd=="c"){
        plot(date,rollingcase,main=main3,ylab="cases",type="o")
    }
    if (cd=="d"){
        plot(date,rollingdeath,main=main4,ylab="deaths",type="o")
    }
}

library(shiny)

ui <- fluidPage(
    titlePanel("Case and Death Plots by Country"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    selectInput(inputId = "country",label = "Choose Your Country",choices = countrynames),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        countryplot(input$country,"a")
    })
    output$plot2<-renderPlot({
        countryplot(input$country,"b")
    })
    output$plot3<-renderPlot({
        countryplot(input$country,"c")
    })
    output$plot4<-renderPlot({
        countryplot(input$country,"d")
    })
}

shinyApp(ui = ui,server = server)