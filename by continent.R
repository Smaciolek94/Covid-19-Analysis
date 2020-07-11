rawcases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
rawdeaths <-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
rawcases <- rawcases[,-c(3:4)]
rawdeaths <- rawdeaths[,-c(3:4)]

n <- ncol(rawcases)-2
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

countryplot <- function(region){
  cases <- rep(0,n)
  newcases <- rep(0,n)
  rollingcase <- rep(0,n)
  regioncase <- rawcases[which(rawcases$Country.Region==region),]
  for (i in 1:n){
    cases[i] <- sum(regioncase[i+2])
    if (i > 1){
      newcases[i] <- cases[i] - cases[i-1]
    }
  }
  newcases[1] <- cases[1]
  for (i in 7:n){
    rollingcase[i] <- mean(newcases[(i-6):i])
  }
  return(rollingcase)
}

plot(countryplot("Bolivia"))

NorthAm <- c("Canada","US","Mexico")
CentAm <- c("Belize","Costa Rica","El Salvador","Guatemala","Honduras",
                "Nicaragua","Panama")
SouthAm <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador",
                 "Guyana","Paraguay","Peru","Suriname",
                 "Uruguay","Venezuela")

Northtotal <- sapply(NorthAm,countryplot)
north <- rowSums(Northtotal)
Centraltotal <- sapply(CentAm,countryplot)
central  <- rowSums(Centraltotal)
Southtotal <- sapply(SouthAm,countryplot)
south <- rowSums(Southtotal)

plot(date,north,main="Moving Average - New Daily Cases in North America",
     xlab = "Date", ylab = "New Cases",type = "o")
plot(date,central,main="Moving Average - New Daily Cases in Central America",
     xlab = "Date", ylab = "New Cases",type = "o")
plot(date,south,main="Moving Average - New Daily Cases in South America",
     xlab = "Date", ylab = "New Cases",type = "o")

plot(date,north,main="Moving Average - New Daily Cases in the Americas",
     xlab = "Date", ylab = "New Cases",type = "l",col="blue",ylim=c(0,40000))
lines(date,central,col="green",type="l")
lines(date,south,col="red",type="l")

Africa <- c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
"Cabo Verde","Cameroon","Central African Republic","Chad","Comoros","Congo (Brazzaville)", 
"Congo (Kinshasa)","Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea",
"Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
"Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania",
"Mauritius","Morocco","Mozambique","Namibia","Niger","Nigeria","Rwanda",
"Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia",
"South Africa","South Sudan","Sudan","Tanzania","Togo","Tunisia","Uganda",
"Zambia","Zimbabwe")

i=0
for (i in 1:length(Africa)){countryplot(Africa[i])}

AfricaTotal <- sapply(Africa,countryplot)
africa <- rowSums(AfricaTotal)
plot(date,africa,main="7 Day Moving Average Cases in Africa",
     ylab="New Cases",xlab="Date",type="o")

Europe <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus",
"Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Cyprus","Czechia",
"Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary",
"Iceland","Ireland","Italy","Kazakhstan","Kosovo","Latvia","Liechtenstein",
"Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro",
"Netherlands","North Macedonia","Norway","Poland","Portugal","Romania",
"Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden",
"Switzerland","Turkey","Ukraine","United Kingdom")

i=0
for (i in 1:length(Europe)){countryplot(Europe[i])}

EuropeTotal <- sapply(Europe,countryplot)
europe <- rowSums(EuropeTotal)

plot(date,europe,main="New Daily Cases in Several Continents",
     xlab="Date",ylab="New Cases",type="l",col="blue")
lines(date,north,type="l",col="green")
lines(date,south,type="l",col="red")
lines(date,africa,type="l",col="orange")

Asia <- c("Afghanistan","Armenia","Azerbaijan","Bahrain","Bangladesh",
"Bhutan","Brunei","Cambodia","China","Cyprus","Georgia",
"India","Indonesia","Iran","Iraq","Israel","Japan","Jordan","Kazakhstan",
"Kuwait","Kyrgyzstan","Laos","Lebanon","Malaysia","Maldives","Mongolia",
"Nepal","Oman","Pakistan","Philippines",
"Qatar","Russia","Saudi Arabia","Singapore","Sri Lanka",
"Syria","Tajikistan","Thailand","Turkey",
"United Arab Emirates","Uzbekistan","Vietnam","Yemen")

i=0
for (i in 1:length(Asia)) {countryplot(Asia[i])}

AsiaTotal <- sapply(Asia,countryplot)
asia<-rowSums(AsiaTotal)

australia <- countryplot("Australia")

plot(date,europe,main="New Daily Cases in Several Continents",
     xlab="Date",ylab="New Cases",type="l",col="blue",ylim=c(0,70000))
lines(date,north,type="l",col="green")
lines(date,(south+central),type="l",col="red")
lines(date,africa,type="l",col="orange")
lines(date,(asia+australia),type="l",col="purple")
legend("topleft",legend=c("Europe","North America","Central and South America",
        "Africa","Asia and Australia"),col=c("blue","green","red","orange","purple"),
       pch=c(1,1),lty=c(.5,.5),bty="n")

total <- europe + north + south + central + asia + australia + africa
plot(date,total,main="New Daily Cases in Several Continents",
     xlab="Date",ylab="New Cases",type="l",col="black")
lines(date,europe,type="l",col="blue")
lines(date,north,type="l",col="green")
lines(date,(south+central),type="l",col="red")
lines(date,africa,type="l",col="orange")
lines(date,(asia+australia),type="l",col="purple")
legend("topleft",legend=c("World Total","Europe","North America","Central and South America",
                          "Africa","Asia and Australia"),col=c("black","blue","green","red","orange","purple"),
       pch=c(1,1),lty=c(.5,.5))