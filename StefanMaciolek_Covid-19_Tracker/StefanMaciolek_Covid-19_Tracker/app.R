##total app
##this has been updated to include testing data

library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)

#import for countryplot
rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]

#import for state and town plots
uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
#stateplots
usstatecases <- uscases[,-c(1:5,8:11)]
usstatedeaths <- usdeaths[,-c(1:5,8:12)]
#townplots
ustowncases <- uscases[,-c(1:10)]
ustowndeaths <- usdeaths[,-c(1:10,12)]

#import for testing plots
testdata <- read.csv(url("https://covidtracking.com/api/v1/states/daily.csv"))

#countryplot
countrynames <- unique(rawcases$Country.Region) 

#stateplot
statenames <- unique(usstatecases$Province_State)
statenames <- statenames[!statenames %in% c("Diamond Princess","Grand Princess")]

#townplot
location <- unique(ustowncases$Combined_Key)

#test plots
testinputs <- c("AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FL","GA","GU","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","VI","WA","WV","WI","WY")
testkey <- data.frame(testinputs,statenames)

n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

datefirst <- date[which(day(date)==1)]

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
    main3 <- paste("New Cases in:",region,date[n])
    main4 <- paste("New Deaths in:",region,date[n])
    library(scales)
    df<-data.frame(date,cases,deaths,rollingcase,rollingdeath)
    if (cd=="A"){
        #it's getting confused by the conditional
        a <- ggplot(df)
        a <- a + geom_area(aes_string(x=date,y=cases),fill="steelblue")
        a <- a + geom_line(aes_string(x=date,y=cases))
        a <- a + ggtitle(main1)
        a <- a + xlab("Date") + ylab("Cases")
        a <- a + scale_x_continuous(breaks = datefirst)
        a <- a + scale_y_continuous(labels=comma)
        return(a)
    }
    if (cd =="B"){
        b <- ggplot(df)
        b <- b + geom_area(aes_string(x=date,y=deaths),fill="red4")
        b <- b + geom_line(aes_string(x=date,y=deaths))
        b <- b + ggtitle(main2)
        b <- b + xlab("Date") + ylab("Deaths")
        b <- b + scale_x_continuous(breaks = datefirst)
        b <- b + scale_y_continuous(labels=comma)
        return(b)
    }
    if (cd=="C"){
        c <- ggplot(df)
        c <- c + geom_area(aes_string(x=date,y=rollingcase),fill="steelblue")
        c <- c + geom_line(aes_string(x=date,y=rollingcase))
        c <- c + ggtitle(main3,subtitle="Seven Day Rolling Average")
        c <- c + xlab("Date") + ylab("Cases")
        c <- c + scale_x_continuous(breaks = datefirst)
        c <- c + scale_y_continuous(labels=comma)
        return(c)
    }
    if (cd=="D"){
        d <- ggplot(df)
        d <- d + geom_area(aes_string(x=date,y=rollingdeath),fill="red4")
        d <- d + geom_line(aes_string(x=date,y=rollingdeath))
        d <- d + ggtitle(main4,subtitle="Seven Day Rolling Average")
        d <- d + scale_x_continuous(breaks = datefirst)
        d <- d + xlab("Date") + ylab("Deaths")
        d <- d + scale_y_continuous(labels=comma)
        return(d)
    }
}

stateplot <- function(region,cd){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    rollingcase <- rep(0,n)
    rollingdeath <- rep(0,n)
    regioncase <- usstatecases[which(usstatecases$Province_State==region),]
    regiondeath <- usstatedeaths[which(usstatedeaths$Province_State==region),]
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
    for (i in 6:n){
        rollingcase[i] <- mean(newcases[(i-6):i])
        rollingdeath[i] <- mean(newdeaths[(i-6):i])
    }
    rollingcase[1:6] <- newcases[1:6]
    rollingdeath[1:6] <- newdeaths[1:6]
    main1 <- paste("Total Cases in:",region,date[n])
    main2 <- paste("Total Deaths in:",region,date[n])
    main3 <- paste("New Cases in:",region,date[n])
    main4 <- paste("New Deaths in:",region,date[n])
    df<-data.frame(date,cases,deaths,rollingcase,rollingdeath)
    if (cd=="A"){
        a <- ggplot(df)
        a <- a + geom_area(aes_string(x=date,y=cases),fill="steelblue")
        a <- a + geom_line(aes_string(x=date,y=cases))
        a <- a + ggtitle(main1)
        a <- a + xlab("Date") + ylab("Cases")
        a <- a + scale_x_continuous(breaks = datefirst)
        a <- a + scale_y_continuous(labels=comma)
        return(a)
    }
    if (cd =="B"){
        b <- ggplot(df)
        b <- b + geom_area(aes_string(x=date,y=deaths),fill="red4")
        b <- b + geom_line(aes_string(x=date,y=deaths))
        b <- b + ggtitle(main2)
        b <- b + xlab("Date") + ylab("Deaths")
        b <- b + scale_x_continuous(breaks = datefirst)
        b <- b + scale_y_continuous(labels=comma)
        return(b)
    }
    if (cd=="C"){
        c <- ggplot(df)
        c <- c + geom_area(aes_string(x=date,y=rollingcase),fill="steelblue")
        c <- c + geom_line(aes_string(x=date,y=rollingcase))
        c <- c + ggtitle(main3,subtitle="Seven Day Rolling Average")
        c <- c + xlab("Date") + ylab("Cases")
        c <- c + scale_x_continuous(breaks = datefirst)
        c <- c + scale_y_continuous(labels=comma)
        return(c)
    }
    if (cd=="D"){
        d <- ggplot(df)
        d <- d + geom_area(aes_string(x=date,y=rollingdeath),fill="red4")
        d <- d + geom_line(aes_string(x=date,y=rollingdeath))
        d <- d + ggtitle(main4,subtitle="Seven Day Rolling Average")
        d <- d + xlab("Date") + ylab("Deaths")
        d <- d + scale_x_continuous(breaks = datefirst)
        d <- d + scale_y_continuous(labels=comma)
        return(d)
    }
}

townplot <- function(location,cd){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    rollingcase <- rep(0,n)
    rollingdeath <- rep(0,n)
    towncase <- as.numeric(ustowncases[which(ustowncases$Combined_Key==location),])
    towndeath <- as.numeric(ustowndeaths[which(ustowndeaths$Combined_Key==location),])
    towncase[is.na(towncase)] <- 0
    towndeath[is.na(towndeath)] <- 0
    for (i in 1:n){
        cases[i] <- towncase[i+1]
        deaths[i] <- towndeath[i+1]
        if (i > 1){
            newcases[i] <- cases[i] - cases[i-1]
            newdeaths[i] <- deaths[i] - deaths[i-1]
        }
    }
    newcases[is.na(newcases)] <- 0
    newdeaths[is.na(newdeaths)] <- 0
    newcases[1] <- cases[1]
    newdeaths[1] <- deaths[1]
    for (i in 6:n){
        rollingcase[i] <- mean(newcases[(i-6):i])
        rollingdeath[i] <- mean(newdeaths[(i-6):i])
    }
    rollingcase[1:6] <- newcases[1:6]
    rollingdeath[1:6] <- newdeaths[1:6]
    rollingcase[is.na(rollingcase)] <- 0
    rollingdeath[is.na(rollingdeath)] <- 0
    main1 <- paste("Total Cases in:",location,date[n])
    main2 <- paste("Total Deaths in:",location,date[n])
    main3 <- paste("New Cases in:",location,date[n])
    main4 <- paste("New Deaths in:",location,date[n])
    df<-data.frame(date,cases,deaths,rollingcase,rollingdeath)
    if (cd=="A"){
        a <- ggplot(df)
        a <- a + geom_area(aes_string(x=date,y=cases),fill="steelblue")
        a <- a + geom_line(aes_string(x=date,y=cases))
        a <- a + ggtitle(main1)
        a <- a + xlab("Date") + ylab("Cases")
        a <- a + scale_x_continuous(breaks = datefirst)
        a <- a + scale_y_continuous(labels=comma)
        return(a)
    }
    if (cd =="B"){
        b <- ggplot(df)
        b <- b + geom_area(aes_string(x=date,y=deaths),fill="red4")
        b <- b + geom_line(aes_string(x=date,y=deaths))
        b <- b + ggtitle(main2)
        b <- b + xlab("Date") + ylab("Deaths")
        b <- b + scale_x_continuous(breaks = datefirst)
        b <- b + scale_y_continuous(labels=comma)
        return(b)
    }
    if (cd=="C"){
        c <- ggplot(df)
        c <- c + geom_area(aes_string(x=date,y=rollingcase),fill="steelblue")
        c <- c + geom_line(aes_string(x=date,y=rollingcase))
        c <- c + ggtitle(main3,subtitle="Seven Day Rolling Average")
        c <- c + xlab("Date") + ylab("Cases")
        c <- c + scale_x_continuous(breaks = datefirst)
        c <- c + scale_y_continuous(labels=comma)
        return(c)
    }
    if (cd=="D"){
        d <- ggplot(df)
        d <- d + geom_area(aes_string(x=date,y=rollingdeath),fill="red4")
        d <- d + geom_line(aes_string(x=date,y=rollingdeath))
        d <- d + ggtitle(main4,subtitle="Seven Day Rolling Average")
        d <- d + xlab("Date") + ylab("Deaths")
        d <- d + scale_x_continuous(breaks = datefirst)
        d <- d + scale_y_continuous(labels=comma)
        return(d)
    }
}

testplots <- function(statefull,ab){
    state <- testkey[testkey$statenames==statefull,1]
    statedata <- data[which(testdata$state==state),]
    n <- nrow(statedata)
    index <- 1:n
    statedata <- data.frame(statedata,index)
    statedata <- statedata[order(-statedata$index),]
    statedata[is.na(statedata)] <- 0
    positive <- rep(0,n)
    #negative <- rep(0,n)
    positive[1] <- statedata$positive[1]
    #negative[1] <- statedata$negative[1]
    positive[2:n] <- diff(statedata$positive)
    #negative[2:n] <- diff(statedata$negative)
    total <- rep(0,n) #new
    total[1] <- statedata$totalTestResults[1]
    total[2:n] <- diff(statedata$totalTestResults) #new
    #pending <- statedata$pending
    #total<-positive+negative+pending
    percentpositive <- positive / total * 100
    origin <- Sys.Date() - n - 1
    date <- 1:n
    date <- as.Date(date,origin = origin)
    format(date,format = "%b %d %y")
    df <- data.frame(date,total,percentpositive)
    if(ab==1){
        main = paste("Daily Tests in",statefull)
        a <- ggplot(df)
        a <- a + geom_area(aes_string(x=date,y=total),fill="seagreen4")
        a <- a + geom_line(aes_string(x=date,y=total))
        a <- a + ggtitle(main) + xlab("Date") + ylab("Number of Tests")
        a <- a + scale_x_continuous(breaks = datefirst)
        a <- a + scale_y_continuous(labels=comma)
        return(a)
    }
    if(ab==2){
        main = paste("Pecent Positive/Negative Test Results in",statefull)
        b <- ggplot(df)
        b <- b + geom_area(aes_string(x=date,y=percentpositive),fill="red")
        b <- b + geom_line(aes_string(x=date,y=percentpositive))
        b <- b + ggtitle(main) + xlab("Date") + ylab("Percentage of Tests Positive")
        b <- b + scale_x_continuous(breaks = datefirst)
        b <- b + scale_y_continuous(labels=comma)
        return(b)
    }
}

ui <- fluidPage(
    titlePanel("Covid-19 Case and Death Tracker"),
    mainPanel("Stefan Maciolek, MS"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    tabsetPanel(
        #id = "plots1",
        tabPanel(
            id = "plots1",
            title = "By Country",
            titlePanel("By Country"),
                        selectInput(inputId = "country",label = "Choose Your Country",choices = countrynames),
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4")
        ),
        tabPanel(
            id = "plots2",
            title = "By State",
            titlePanel("By State (US only)"),
            selectInput(inputId = "state",label = "Choose Your State",choices = statenames),
            plotOutput("plot5"),
            plotOutput("plot6"),
            plotOutput("plot7"),
            plotOutput("plot8"),
            plotOutput("plot9"),
            plotOutput("plot10")
        ),
        tabPanel(
            id = "plots3",
            title = "By City/County",
            titlePanel("By City/County (US only)"),
            selectizeInput(inputId = "location",label = "Choose Your Location",choices = location,options= list(maxOptions = 3253)),
            plotOutput("plot11"),
            plotOutput("plot12"),
            plotOutput("plot13"),
            plotOutput("plot14")
        )
    )
)

server <- function(input, output, session) {
    output$plot1<-renderPlot({
        countryplot(input$country,"C")
    })
    output$plot2<-renderPlot({
        countryplot(input$country,"D")
    })
    output$plot3<-renderPlot({
        countryplot(input$country,"A")
    })
    output$plot4<-renderPlot({
        countryplot(input$country,"B")
    })
    output$plot5<-renderPlot({
        stateplot(input$state,"C")
    })
    output$plot6<-renderPlot({
        stateplot(input$state,"D")
    })
    output$plot7<-renderPlot({
        stateplot(input$state,"A")
    })
    output$plot8<-renderPlot({
        stateplot(input$state,"B")
    })
    output$plot9<-renderPlot({
        testplots(input$state,1)
    })
    output$plot10<-renderPlot({
        testplots(input$state,2)
    })
    output$plot11<-renderPlot({
        townplot(input$location,"C")
    })
    output$plot12<-renderPlot({
        townplot(input$location,"D")
    })
    output$plot13<-renderPlot({
        townplot(input$location,"A")
    })
    output$plot14<-renderPlot({
        townplot(input$location,"B")
    })
}

shinyApp(ui = ui, server = server)