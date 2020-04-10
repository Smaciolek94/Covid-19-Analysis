uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:10)]
usdeaths <- usdeaths[,-c(1:10,12)]

location <- as.character(uscases$Combined_Key)

n <- ncol(uscases)-1
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

townplot <- function(location,cd,start){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    towncase <- as.numeric(uscases[which(uscases$Combined_Key==location),])
    towndeath <- as.numeric(usdeaths[which(usdeaths$Combined_Key==location),])
    for (i in 1:n){
        cases[i] <- towncase[i+1]
        deaths[i] <- towndeath[i+1]
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
        plot(date[start:n],cases[start:n],main=main1,ylab="cases",ylim = c(date[start],date[n]),type="o")
    }
    if (cd=="b"){
        plot(date[start:n],deaths[start:n],main=main2,ylab="deaths",ylim = c(date[start],date[n]),type="o")
    }
    if (cd=="c"){
        plot(date[start:n],newcases[start:n],main=main3,ylab="cases",ylim = c(date[start],date[n]),type="o")
    }
    if (cd=="d"){
        plot(date[start:n],newdeaths[start:n],main=main4,ylab="deaths",ylim = c(date[start],date[n]),type="o")
    }
}

library(shiny)

ui <- fluidPage(
    titlePanel("Case and Death Plots by County/City"),
    selectizeInput(inputId = "location",label = "Choose Your Location",choices = location,options= list(maxOptions = 3253)),
    sliderInput(inputId = "Start",
                label= "Choose your start date: ",
                min = date[1],
                max = date[n],
                value = date[1]),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        townplot(input$location,"a",input$Start)
    })
    output$plot2<-renderPlot({
        townplot(input$location,"b",input$Start)
    })
    output$plot3<-renderPlot({
        townplot(input$location,"c",input$Start)
    })
    output$plot4<-renderPlot({
        townplot(input$location,"d",input$Start)
    })
}

shinyApp(ui = ui,server = server)