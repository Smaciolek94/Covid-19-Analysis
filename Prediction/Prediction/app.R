uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(uscases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

statenames <- unique(uscases$Province_State)

model <- function(state,nahead){
    statecase <- uscases[which(uscases$Province_State==state),]
    cases <- rep(0,n)
    for (i in 1:n){
        cases[i]<- sum(statecase[,i+2])
    }
    model <- lm(cases~poly(date,5))
    pred <- rep(0,nahead)
    for (i in 1:nahead){
        pred[i] <- model$coefficients[1] + model$coefficients[2]*i +model$coefficients[3]*i^2
        +model$coefficients[4]*i^3 + model$coefficients[5]*i^4+model$coefficients[6]*i^5
    }
    main = paste("Predicted Cases in",state,nahead,"days out")
    datenew <- 1:(n+nahead)
    datenew <- as.Date(datenew,origin = "2020-01-21")
    format(datenew,format = "%b %d %y")
    cases = c(cases,pred)
    plot(datenew,cases,main=main,type="p",col=c(rep("black",n),rep("red",nahead)),xlab="date")
}

library(shiny)

ui <- fluidPage(
    titlePanel("Predicted Plots"),
    selectInput(inputId = "state",label = "Choose Your State",choices = statenames),
    sliderInput(inputId = "nahead",
                label= "Number of Days ahead to Predict",
                min = 1,
                max = 20,
                value = 1),
    plotOutput("plot1")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        model(input$state,input$nahead)
    })
}

shinyApp(ui = ui,server = server)