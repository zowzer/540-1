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

CityBikeData <- read_excel("~/Documents/TBANLT540/citibike-tripdata_Complete2018_2.xlsx")
View(CityBikeData)


CityBikeData2 <- CityBikeData %>% select(`start station name`,`end station name`,bikeid, usertype,tripduration,starttime,stoptime,month)

#Removes empty cases, if any#
CityBikeData<-  CityBikeData[complete.cases(CityBikeData), ]

#Returns how many observations have "NA" , if any #
sum(is.na(CityBikeData1))

#count the values in start station#
StartStationCount <- as.data.frame(table(CityBikeData2$`start station name`))
View(StartStationCount)

#create factor variable for start station#
CityBikeData2$StartStationFactor<-factor(CityBikeData2$`start station name`)

#count the values in end station#
EndStationCount <- as.data.frame(table(CityBikeData2$`end station name`))
View(EndStationCount)

#ESLoc is End Station Location
CityBikeData2$ESLoc<-factor(CityBikeData2$`end station name`,levels=c('12 Ave & W 40 St','Bethune Center','Vesey Pl & River Terrace','Danforth Light Rail','Bayside Park','JCBS Depot','MLK Light Rail','Columbia Park',
                                                                      'NJCU','Communipaw & Berry Lane','5 Corners Library','West Side Light Rail','Garfield Ave Station','Christ Hospital','Riverview Park','Lafayette Park','Union St',
                                                                      'Hilltop','Heights Elevator','Dey St','Leonard Gordon Park','Pershing Field','York St','Lincoln Park','Journal Square','Astor Place','Baldwin at Montgomery','Oakland Ave',
                                                                      'Washington St','JC Medical Center','Liberty Light Rail','Manila & 1st','Dixon Mills','Jersey & 6th St','Essex Light Rail','Brunswick St','McGinley Square','Columbus Drive',
                                                                      'Jersey & 3rd','Monmouth and 6th','Harborside','Newport Pkwy','Marin Light Rail','Brunswick & 6th','City Hall','Warren St','Van Vorst Park','Newark Ave','Paulus Hook','Morris Canal',
                                                                      'Newport PATH','Sip Ave','Hamilton Park','Exchange Place','Grove St PATH','W 52 St & 5 Ave','6 Ave & Canal St','Amsterdam Ave & W 66 St','Barclay St & Church St',
                                                                      'Barrow St & Hudson St','Broadway & W 32 St','Central Park S & 6 Ave','W 13 St & 6 Ave','W 16 St & The High Line','W 20 St & 11 Ave','W 22 St & 10 Ave','W 22 St & 8 Ave','W 37 St & 10 Ave','W 4 St & 7 Ave S',
                                                                      'W 56 St & 10 Ave','W 59 St & 10 Ave','6 Ave & W 33 St','11 Ave & W 41 St','Cliff St & Fulton St_1','Columbia St & Kane St','Duane St & Greenwich St','E 110 St & Madison Ave','E 32 St & Park Ave','E 33 St & 5 Ave',
                                                                      'E 39 St & 3 Ave','E 47 St & Park Ave','E 55 St & Lexington Ave','FDR Drive & E 35 St','Fulton St & Broadway','John St & William St','Lexington Ave & E 24 St','Pike St & Monroe St','Prospect Pl & 6 Ave','S Portland Ave & Hanson Pl',
                                                                      'South St & Whitehall St','Sullivan St & Washington Sq','W 11 St & 6 Ave','W 113 St & Broadway','W 116 St & Amsterdam Ave',
                                                                      'Amsterdam Ave & W 125 St','Bus Slip & State St','Central Park West & W 68 St','Central Park West & W 72 St',
                                                                      'Maiden Ln & Pearl St','Mercer St & Spring St','Murray St & Greenwich St','Pier 40 - Hudson River Park','W 15 St & 7 Ave','W 34 St & 11 Ave','W Broadway & Spring Street','West St & Chambers St','Broadway & Moylan Pl','Broadway & W 29 St',
                                                                      'Broadway & W 58 St','Hudson St & Reade St','Murray St & West St','Water - Whitehall Plaza','8 Ave & W 52 St','South End Ave & Liberty St','Atlantic Ave & Furman St','Henry St & Atlantic Ave','Broadway & Battery Pl'))
#recode the factor to collapse the categories#
CityBikeData2$ESLoc <- recode(CityBikeData2$`end station name`, 'c("11 Ave & W 41 St","Broadway & W 32 St","E 32 St & Park Ave","E 33 St & 5 Ave","E 39 St & 3 Ave","E 47 St & Park Ave","E 55 St & Lexington Ave","FDR Drive & E 35 St","Lexington Ave & E 24 St","W 16 St & The High Line","W 20 St & 11 Ave","W 22 St & 10 Ave","W 22 St & 8 Ave","W 37 St & 10 Ave","6 Ave & W 33 St","W 34 St & 11 Ave","Broadway & W 29 St","W 46 St & 11 Ave") = "Midtown"; c("6 Ave & Canal St","Barclay St & Church St","Pike St & Monroe St","Cliff St & Fulton St_1","Fulton St & Broadway","John St & William St","South St & Whitehall St","Bus Slip & State St","Maiden Ln & Pearl St","Mercer St & Spring St","W Broadway & Spring Street","West St & Chambers St","Hudson St & Reade St","Murray St & West St","Water - Whitehall Plaza","South End Ave & Liberty St") = "Lower Manhattan"; c("Amsterdam Ave & W 66 St","Central Park S & 6 Ave","E 110 St & Madison Ave","W 113 St & Broadway","W 116 St & Amsterdam Ave","W 52 St & 5 Ave","W 56 St & 10 Ave","W 59 St & 10 Ave","Amsterdam Ave & W 125 St","Central Park West & W 68 St","Central Park West & W 72 St","Broadway & Moylan Pl","Broadway & W 58 St","8 Ave & W 52 St") = "Manhattan"; c("Columbia St & Kane St","Prospect Pl & 6 Ave","S Portland Ave & Hanson Pl","Atlantic Ave & Furman St","Henry St & Atlantic Ave") = "Brooklyn"; c("Barrow St & Hudson St","Duane St & Greenwich St","Sullivan St & Washington Sq","W 11 St & 6 Ave","W 13 St & 6 Ave","W 4 St & 7 Ave S","Murray St & Greenwich St","Pier 40 - Hudson River Park","W 15 St & 7 Ave") = "Greenwich Village"')

#count the values in start station#
EndStationCount2 <- as.data.frame(table(CityBikeData2$ESLoc))
View(EndStationCount2)

#NEED CODE HERE TO DROP end station name#

#create factor variable for end station#
CityBikeData2$EndStationFactor<-factor(CityBikeData2$`end station name`)

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


#tell R this is time series data#
#tsDuration<-zoo(CityBikeData2$tripduration)

#create plots of the timeseries #
#ggplot(data = CityBikeData2, aes(x = CityBikeData2$month, y = CityBikeData2$tripduration))+ geom_line()

#test for stationarity For the adf.test, if p-value < 0.05 => stationary#
#adf.test(tsDuration)

#check the correlogram# 
#acf(tsDuration)




















