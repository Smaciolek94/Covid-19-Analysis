statenames <- c("Alabama","Alaska","American Samoa","Arizona",
                "Arkansas",
                "California",
                "Colorado",
                "Connecticut",
                "Delaware",
                "Diamond Princess",
                "District of Columbia",
                "Florida",
                "Georgia",
                "Grand Princess",
                "Guam",
                "Hawaii",
                "Idaho",
                "Illinois",
                "Indiana",
                "Iowa",
                "Kansas",
                "Kentucky",
                "Louisiana",
                "Maine",
                "Maryland",
                "Massachusetts",
                "Michigan",
                "Minnesota",
                "Mississippi",
                "Missouri",
                "Montana",
                "Nebraska",
                "Nevada",
                "New Hampshire",
                "New Jersey",
                "New Mexico",
                "New York",
                "North Carolina",
                "North Dakota",
                "Northern Mariana Islands",
                "Ohio",
                "Oklahoma",
                "Oregon",
                "Pennsylvania",
                "Puerto Rico",
                "Rhode Island",
                "South Carolina",
                "South Dakota",
                "Tennessee",
                "Texas",
                "Utah",
                "Vermont",
                "Virgin Islands",
                "Virginia",
                "Washington",
                "West Virginia",
                "Wisconsin",
                "Wyoming")

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
        plot(date,cases,main=main1,ylab="cases",type="o")
    }
    if (cd == "b"){
        plot(date,deaths,main=main2,ylab="deaths",type="o")
    }
    if (cd == "c"){
        plot(date,newcases,main=main3,ylab="cases",type="o")
    }
    if (cd == "d"){
        plot(date,newdeaths,main=main4,ylab="deaths",type="o")
    }
}

library(shiny)

ui <- fluidPage(
    titlePanel("Case and Death Plots by State"),
    mainPanel("Data From Johns Hopkins CSSE, for educational/research use only"),
    selectInput(inputId = "state",label = "Choose Your State",choices = statenames),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4")
)

server <- function(input,output){
    output$plot1<-renderPlot({
        stateplot(input$state,"a")
    })
    output$plot2<-renderPlot({
        stateplot(input$state,"b")
    })
    output$plot3<-renderPlot({
        stateplot(input$state,"c")
    })
    output$plot4<-renderPlot({
        stateplot(input$state,"d")
    })
}

shinyApp(ui = ui,server = server)