uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

statenames <- unique(uscases$Province_State)

#n <- ncol(uscases)-2
n <- 50
date <- 1:n

predictedcases <- function(nahead) {
    cases <- rep(0,n)
    newcases <- rep(0,n)
    for (i in 1:n){
        cases[i]<- sum(uscases[,i+2])
        if (i>2){
            newcases[i] <- cases[i] - cases[i-1]
        }
    }
    newcases[1] <- cases[1]
    cleandata <- data.frame(date,cases,newcases)
    wmean <- weighted.mean(date,cases)
    max <- cleandata[which(cleandata$newcases==max(cleandata$newcases)),]
    maxmean <- max[1,1]
    
    library(radiant.data)
    wsd <- weighted.sd(date,cases)
    
    predcases <- rep(0,nahead)
    for (i in 1:nahead){
        predcases[i] <- max[1,3]*dnorm(n+i,maxmean,2*wsd)/dnorm(max[1,1],maxmean,2*wsd)
    }
    
    totcases <- c(newcases,predcases)
    newdate <- 1:(n+nahead)
    newdate <- as.Date(newdate,origin = "2020-01-21")
    format(newdate,format = "%b %d %y")
    
    main = paste("Predicted New Cases: Total US",nahead,"Days Out, in Red")
    plot(newdate,totcases,col=c(rep("black",n),rep("red",nahead)),type="l",main=main,ylab="New Daily Cases",xlab="Date")
}


predictedstatecases <- function(state,nahead) {
    cases <- rep(0,n)
    newcases <- rep(0,n)
    statecases <- uscases[which(uscases$Province_State==state),]
    for (i in 1:n){
        cases[i]<- sum(statecases[,i+2])
        if (i>2){
            newcases[i] <- cases[i] - cases[i-1]
        }
    }
    newcases[1] <- cases[1]
    cleandata <- data.frame(date,cases,newcases)
    wmean <- weighted.mean(date,cases)
    max <- cleandata[which(cleandata$newcases==max(cleandata$newcases)),]
    maxmean <- max[1,1]
    
    library(radiant.data)
    wsd <- weighted.sd(date,cases)
    
    predcases <- rep(0,nahead)
    for (i in 1:nahead){
        predcases[i] <- max[1,3]*dnorm(n+i,maxmean,2*wsd)/dnorm(max[1,1],maxmean,2*wsd)
    }
    
    totcases <- c(newcases,predcases)
    newdate <- 1:(n+nahead)
    newdate <- as.Date(newdate,origin = "2020-01-21")
    format(newdate,format = "%b %d %y")
    
    main = paste("Predicted New Cases:",state,nahead,"Days Out, in Red")
    plot(newdate,totcases,col=c(rep("black",n),rep("red",nahead)),type="p",main=main,ylab="New Daily Cases",xlab="Date")
}

library(shiny)

ui <- fluidPage(
    titlePanel("Predicted New Cases - THIS IS ONLY SPECULATION - NOT FOR MEDICAL OR PLANNING USE"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    selectInput(inputId = "state",label = "Choose Your State",choices = statenames),
    sliderInput(inputId = "nahead",
                label= "Number of Days ahead to Predict",
                min = 1,
                max = 60,
                value = 1),
    plotOutput("plot1"),
    plotOutput("plot2")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        predictedstatecases(input$state,input$nahead)
    })
    output$plot2<-renderPlot({
        predictedcases(input$nahead)
    })
}

shinyApp(ui = ui,server = server)