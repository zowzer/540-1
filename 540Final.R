install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("plyr")
install.packages("car")
install.packages("zoo")

library(tidyverse)
library(corrgram)
library(tseries)
library(readxl)
library(plyr)
library(car)
library(zoo)

# calls to see your current directory#
getwd()

setwd("~/Documents/TBANLT540")

CityBikeData <- read_excel("~/Documents/TBANLT540/citibike-tripdata.xlsx")
View(CityBikeData)


CityBikeData2 <- CityBikeData %>% select(`start station name`,`end station name`,bikeid, usertype,tripduration,starttime,stoptime)

#Removes empty cases, if any#
CityBikeData1<-  CityBikeData[complete.cases(CityBikeData), ]

#Returns how many observations have "NA" , if any #
sum(is.na(CityBikeData1))

#count the values in start station#
StartStationCount <- as.data.frame(table(CityBikeData2$`start station name`))
View(StartStationCount)

#create factor variable for start station#
CityBikeData2$StartStationFactor<-factor(CityBikeData2$`start station name`)

#SSLoc is Start Station Location
CityBikeData2$SSLoc<-factor(CityBikeData2$`start station name`,levels=c('Bayside Park','Columbia Park','Bethune Center','JCBS Depot','Danforth Light Rail','Exchange Place','Paulus Hook','City Hall',
                                                                       'Grove St PATH','Warren St','NJCU','West Side Light Rail','Garfield Ave Station','Union St','Liberty Light Rail','Lincoln Park',
                                                                       'McGinley Square','Sip Ave','Riverview Park','Heights Elevator','Newport Pkwy','Dey St','Newport PATH','Hamilton Park','JC Medical Center',
                                                                       'Hilltop','Oakland Ave','Brunswick St','Pershing Field','Newark Ave','Christ Hospital','Van Vorst Park','Essex Light Rail','5 Corners Library',
                                                                       'Baldwin at Montgomery','Morris Canal','Lafayette Park','Brunswick & 6th','Jersey & 6th St','Jersey & 3rd','Manila & 1st','Columbus Drive',
                                                                       'Marin Light Rail','Communipaw & Berry Lane','Monmouth and 6th','Dixon Mills','Astor Place','Washington St','Leonard Gordon Park','York St',
                                                                       'Harborside','Journal Square'))
                                                               

#recode the factor to collapse the categories#
CityBikeData2$SSLoc <- recode(CityBikeData2$`start station name`, 'c("Bayside Park","Columbia Park","Bethune Center","JCBS Depot","Danforth Light Rail") = "Other"')


#count the values in end station#
EndStationCount <- as.data.frame(table(CityBikeData2$`end station name`))
View(EndStationCount)

#ESLoc is End Station Location
CityBikeData2$ESLoc<-factor(CityBikeData2$`end station name`,levels=c('12 Ave & W 40 St','Bethune Center','Vesey Pl & River Terrace','W 52 St & 5 Ave','Danforth Light Rail','Bayside Park','JCBS Depot','MLK Light Rail','Columbia Park',
                                                                      'NJCU','Communipaw & Berry Lane','5 Corners Library','West Side Light Rail','Garfield Ave Station','Christ Hospital','Riverview Park','Lafayette Park','Union St',
                                                                      'Hilltop','Heights Elevator','Dey St','Leonard Gordon Park','Pershing Field','York St','Lincoln Park','Journal Square','Astor Place','Baldwin at Montgomery','Oakland Ave',
                                                                      'Washington St','JC Medical Center','Liberty Light Rail','Manila & 1st','Dixon Mills','Jersey & 6th St','Essex Light Rail','Brunswick St','McGinley Square','Columbus Drive',
                                                                      'Jersey & 3rd','Monmouth and 6th','Harborside','Newport Pkwy','Marin Light Rail','Brunswick & 6th','City Hall','Warren St','Van Vorst Park','Newark Ave','Paulus Hook','Morris Canal',
                                                                      'Newport PATH','Sip Ave','Hamilton Park','Exchange Place','Grove St PATH'))

#recode the factor to collapse the categories#
CityBikeData2$ESLoc <- recode(CityBikeData2$`end station name`, 'c("12 Ave & W 40 St","Vesey Pl & River Terrace","W 52 St & 5 Ave") = "New York"; c("Danforth Light Rail","Bayside Park","JCBS Depot","MLK Light Rail","Bethune Center") = "Other"')


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

View(CityBikeData2)



#linear regression 
StationData1<-lm(tripduration~bikeid+usertypefactor, data=CityBikeData2)
summary(StationData1)










