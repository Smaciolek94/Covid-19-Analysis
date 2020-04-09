countrynames <- c("Afghanistan","Albania","Algeria","Andorra",                         
                  "Angola","Antigua and Barbuda",             
                  "Argentina", "Armenia",                         
                  "Australia" ,"Austria",                         
                  "Azerbaijan","Bahamas",                         
                  "Bahrain","Bangladesh",                      
                  "Barbados","Belarus",                         
                  "Belgium","Belize",                          
                  "Benin","Bhutan",                          
                  "Bolivia","Bosnia and Herzegovina",          
                  "Botswana","Brazil",                          
                  "Brunei","Bulgaria",                        
                  "Burkina Faso","Burma",                           
                  "Burundi","Cabo Verde",                      
                  "Cambodia","Cameroon",                        
                  "Canada","Central African Republic",        
                  "Chad","Chile",                           
                  "China","Colombia",                        
                  "Congo (Brazzaville)","Congo (Kinshasa)",                
                  "Costa Rica","Cote d'Ivoire",                   
                  "Croatia","Cuba",                            
                  "Cyprus","Czechia",                         
                  "Denmark","Diamond Princess",                
                  "Djibouti","Dominica",                        
                  "Dominican Republic","Ecuador",                         
                  "Egypt","El Salvador",                     
                  "Equatorial Guinea","Eritrea",                         
                  "Estonia", "Eswatini",                        
                  "Ethiopia","Fiji",                            
                  "Finland","France",                          
                  "Gabon","Gambia",                          
                  "Georgia","Germany",                         
                  "Ghana","Greece",                          
                  "Grenada","Guatemala",                       
                  "Guinea","Guinea-Bissau",                  
                  "Guyana","Haiti",                           
                  "Holy See","Honduras",                        
                  "Hungary","Iceland",                         
                  "India","Indonesia",                       
                  "Iran","Iraq",                           
                  "Ireland","Israel",                          
                  "Italy","Jamaica",                         
                  "Japan","Jordan",                          
                  "Kazakhstan","Kenya",                           
                  "Korea, South","Kosovo",                          
                  "Kuwait","Kyrgyzstan",                      
                  "Laos","Latvia",                          
                  "Lebanon","Liberia",                         
                  "Libya","Liechtenstein",                   
                  "Lithuania","Luxembourg",                      
                  "Madagascar","Malawi",                          
                  "Malaysia","Maldives",                        
                  "Mali","Malta",                           
                  "Mauritania","Mauritius",                       
                  "Mexico","Moldova",                         
                  "Monaco","Mongolia",                        
                  "Montenegro","Morocco",                         
                  "Mozambique","MS Zaandam",                      
                  "Namibia","Nepal",                           
                  "Netherlands","New Zealand",                     
                  "Nicaragua","Niger",                           
                  "Nigeria","North Macedonia",                 
                  "Norway","Oman",                            
                  "Pakistan","Panama",                          
                  "Papua New Guinea","Paraguay",                        
                  "Peru","Philippines",                     
                  "Poland","Portugal",                        
                  "Qatar","Romania",                         
                  "Russia","Rwanda",                          
                  "Saint Kitts and Nevis","Saint Lucia",                    
                  "Saint Vincent and the Grenadines","San Marino",                      
                  "Sao Tome and Principe","Saudi Arabia",                    
                  "Senegal","Serbia",                          
                  "Seychelles","Sierra Leone",                    
                  "Singapore","Slovakia",                        
                  "Slovenia","Somalia",                         
                  "South Africa","South Sudan",                     
                  "Spain","Sri Lanka",                       
                  "Sudan","Suriname",                        
                  "Sweden","Switzerland",                     
                  "Syria","Taiwan*",                         
                  "Tanzania","Thailand",                        
                  "Timor-Leste","Togo",                            
                  "Trinidad and Tobago","Tunisia",                         
                  "Turkey", "Uganda",                          
                  "Ukraine","United Arab Emirates",            
                  "United Kingdom","Uruguay",                         
                  "US","Uzbekistan",                      
                  "Venezuela","Vietnam",                         
                  "West Bank and Gaza","Western Sahara",                  
                  "Zambia","Zimbabwe")    

#file importer:
rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]

n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

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
    plot(date,cases,main=main1,ylab="cases",type="o")
  }
  if (cd =="b"){
    plot(date,deaths,main=main2,ylab="deaths",type="o")
  }
  if (cd=="c"){
    plot(date,newcases,main=main3,ylab="cases",type="o")
  }
  if (cd=="d"){
    plot(date,newdeaths,main=main4,ylab="deaths",type="o")
  }
}

library(shiny)

ui <- fluidPage(
  titlePanel("Case and Death Plots"),
  selectInput(inputId = "country",label = "Enter Your Country",choices = countrynames),
  plotOutput("plot1"),
  plotOutput("plot2"),
  plotOutput("plot3"),
  plotOutput("plot4")
)
