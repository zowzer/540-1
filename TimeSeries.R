install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("plyr")
install.packages("car")
install.packages("zoo")
install.packages("reshape2")

library(tidyverse)
library(corrgram)
library(tseries)
library(readxl)
library(plyr)
library(car)
library(zoo)
library(reshape2)

# calls to see your current directory#
getwd()

setwd("~/540 R")

CityBikeData <- read_excel("~/540 R/station_trips.xlsx")
View(CityBikeData)

#Time series conversion

stTS <- ts(data=CityBikeData[, -1], start=min(CityBikeData$Month), end=max(CityBikeData$Month))

plot(stTS, plot.type="single", col=1:59)
#legend("bottom", legend=colnames(stTS), ncol=10, lty=1, col=1:59, cex=.9)

numDiffs <- ndiffs(stTS)

stDiffed <- diff(stTS, difference=numDiffs)
plot(stDiffed, plot.type="single", col=1:59)

install.packages("vars")
install.packages("coefplot")
library(vars)
library(coefplot)

#fit the model

stVar <- VAR(stDiffed, lag.max=1)
#chosen order
stVar$p

names(stVar$varresult)

class(stVar$varresult$X3183)
head(coef(stVar$varresult$X3183))
#plot coefficients
coefplot(stVar$varresult$X3183)
coefplot(stVar$varresult$X3186)

predict(stVar$varresult$X3183, n.ahead=1)
predict(stVar, n.ahead=12)
