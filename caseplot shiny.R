library(shiny)

ui <- fluidPage(
    titlePanel("Case and Death Plots"),
    textInput(inputId = "country", 
              label = "Enter Your Country"),
    textInput(inputId = "state",
              label = "Enter Your State"),
    textInput(inputId = "town",
              label = "Enter Your Town/County"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
)

server <- function(input,output){
  output$plot1<-renderPlot({countryplot(input$country)})
  output$plot2<renderPlot({stateplot(input$state)})
  output$plot3<-renderPlot({townplot(input$town,input$state)})
}

shinyApp(ui = ui,server = server)