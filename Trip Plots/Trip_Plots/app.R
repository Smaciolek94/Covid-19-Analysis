uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:5,8:11)]
usdeaths <- usdeaths[,-c(1:5,8:12)]

n <- ncol(uscases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

stateplot <- function(region,cd){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    rollingcase <- rep(0,n)
    rollingdeath <- rep(0,n)
    regioncase <- uscases[which(uscases$Province_State==region),]
    regiondeath <- usdeaths[which(usdeaths$Province_State==region),]
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
    #for (i in 6:n){
     #   rollingcase[i] <- mean(newcases[(i-6):i])
     #   rollingdeath[i] <- mean(newdeaths[(i-6):i])
    #}
    #rollingcase[1:6] <- newcases[1:6]
    #rollingdeath[1:6] <- newdeaths[1:6]
    main3 <- paste("New Cases in:",region,date[n])
    main4 <- paste("New Deaths in:",region,date[n])
    if (cd == "c"){
        plot(date,newcases,main=main3,ylab="cases",type="o")
    }
    if (cd == "d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o")
    }
}

townplot <- function(location,state,cd){
    cases <- rep(0,n)
    newcases <- rep(0,n)
    deaths <- rep(0,n)
    newdeaths <- rep(0,n)
    rollingcase <- rep(0,n)
    rollingdeath <- rep(0,n)
    towncase <- as.numeric(uscases[which((uscases$Admin2==location) & (uscases$Province_State == state)),])
    towndeath <- as.numeric(usdeaths[which((usdeaths$Admin2==location) & (usdeaths$Province_State == state)),])
    for (i in 1:n){
        cases[i] <- towncase[i+1]
        deaths[i] <- towndeath[i+1]
        if (i > 1){
            newcases[i] <- cases[i] - cases[i-1]
            newdeaths[i] <- deaths[i] - deaths[i-1]
        }
    }
    newcases[1] <- cases[1]
    newdeaths[1] <- deaths[1]
    #for (i in 6:n){
    #    rollingcase[i] <- mean(newcases[(i-6):i])
    #    rollingdeath[i] <- mean(newdeaths[(i-6):i])
    #}
    #rollingcase[1:6] <- newcases[1:6]
    #rollingdeath[1:6] <- newdeaths[1:6]
    main3 <- paste("New Cases in:",location,state,date[n])
    main4 <- paste("New Deaths in:",location,state,date[n])
    if (cd=="c"){
        plot(date,newcases,main=main3,ylab="cases",type="o")
    }
    if (cd=="d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o")
    }
}

library(shiny)

ui <- fluidPage(
    titlePanel("Personal Plots for Trip Planning"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4"),
    plotOutput("plot5"),
    plotOutput("plot6"),
    plotOutput("plot7"),
    plotOutput("plot8"),
    plotOutput("plot9"),
    plotOutput("plot10"),
    plotOutput("plot11"),
    plotOutput("plot12"),
    plotOutput("plot13"),
    plotOutput("plot14"),
    plotOutput("plot15"),
    plotOutput("plot16"),
    plotOutput("plot17"),
    plotOutput("plot18")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        stateplot("New Jersey","c")
    })
    output$plot2<-renderPlot({
        stateplot("New Jersey","d")
    })
    output$plot3<-renderPlot({
        stateplot("Pennsylvania","c")
    })
    output$plot4<-renderPlot({
        stateplot("Pennsylvania","d")
    })
    output$plot5<-renderPlot({
        stateplot("Delaware","c")
    })
    output$plot6<-renderPlot({
        stateplot("Delaware","d")
    })
    output$plot7<-renderPlot({
        stateplot("Maryland","c")
    })
    output$plot8<-renderPlot({
        stateplot("Maryland","d")
    })
    output$plot9<-renderPlot({
        stateplot("New York","c")
    })
    output$plot10<-renderPlot({
        stateplot("New York","d")
    })
    output$plot11<-renderPlot({
        townplot("Baltimore","Maryland","c")
    })
    output$plot12<-renderPlot({
        townplot("Baltimore","Maryland","d")
    })
    output$plot13<-renderPlot({
        townplot("Allegheny","Pennsylvania","c")
    })
    output$plot14<-renderPlot({
        townplot("Allegheny","Pennsylvania","d")
    })
    output$plot15<-renderPlot({
        townplot("Erie","Pennsylvania","c")
    })
    output$plot16<-renderPlot({
        townplot("Erie","Pennsylvania","d")
    })
    output$plot17<-renderPlot({
        townplot("Lackawanna","Pennsylvania","c")
    })
    output$plot18<-renderPlot({
        townplot("Lackawanna","Pennsylvania","d")
    })
}

shinyApp(ui = ui,server = server)