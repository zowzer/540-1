library(readxl)
library(plyr)
library(tidyverse)
library(car)

# calls to see your current directory#
getwd()

setwd("~/Documents/TBANLT540")

CityBikeData <- read_excel("~/Documents/TBANLT540/citibike-tripdata.xlsx")
View(CityBikeData)

#Removes empty cases, if any#
CityBikeData1<-  CityBikeData[complete.cases(CityBikeData), ]

#Returns how many observations have "NA" , if any #
sum(is.na(CityBikeData1))

CityBikeData2 <- CityBikeData %>% select(`start station name`,`end station name`,bikeid, usertype)

#count the values in start station#
StartStationCount <- as.data.frame(table(CityBikeData2$`start station name`))
View(StartStationCount)

#recode the factor to collapse the categories#
#CityBikeData2$SSLoc <- recode(CityBikeData2$`start station name, 'c("PLACEHOLDER","PLACEHOLDER") = "PLACEHOLDER"')
#SSNeigh is Start Station Location

#count the values in end station#
EndStationCount <- as.data.frame(table(CityBikeData2$`end station name`))
View(EndStationCount)

#recode the factor to collapse the categories#
#CityBikeData2$ESLoc <- recode(CityBikeData2$`end station name, 'c("PLACEHOLDER","PLACEHOLDER") = "PLACEHOLDER"')
#SENeigh is End Station Location

#count the values in user type#
UserTypeCount <- as.data.frame(table(CityBikeData2$usertype))
View(UserTypeCount)

#create factor variable for user types#
CityBikeData2$usertypefactor<-factor(CityBikeData2$usertype, levels=c("Subscriber", "Customer"))

#count the values in bike id to get total bikes#
BikeCount <- as.data.frame(table(CityBikeData2$bikeid))
View(BikeCount)

#Get the Min, Median, Mean, Max for all the variables#
summary(CityBikeData2) 








