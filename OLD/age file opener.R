#importing Age Census Data files

install.packages("readxl")
library(readxl)

age1 <- rep(NA,10)
age1 <- as.list(age1)
for (i in 1:10){
age1[i] <- read_xls("C:\\Users\\Stvma\\Documents\\GitHub\\Covid-19-Analysis\\Census Data\\Age\\Census Age Data 1.xls",sheet = i)
}