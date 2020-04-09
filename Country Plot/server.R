
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