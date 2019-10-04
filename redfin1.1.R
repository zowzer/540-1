library(rJava)
library(xlsx)
library(readxl) 
library(tidyverse)

getwd()

#Update file path before /redfin_2019... to reflect your own if getwd() doesn't work for you.
redfin1 <- read_excel("C:/Users/nagem/OneDrive/Documents/R/redfin_2019-09-24-18-15-38.xlsm", sheet = "redfin_2019-09-24-18-15-38") #puts data in Global Environment

view(redfin1)

summary(redfin1)

summary_redfin1 <- summary(redfin1)

price <-(redfin1$`PRICE`)
beds <-(redfin1$`BEDS`)
baths <-(redfin1$`BATHS`)
sq_ft <-(redfin1$`SQUARE FEET`)
lot_size <-(redfin1$`LOT SIZE`)
yr_built <-(redfin1$`YEAR BUILT`)

#Normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Generating and adding normalized yr_built to data frame
redfin1$yr_built_norm<-normalize(redfin1$'YEAR BUILT')
yr_built_norm = redfin1$yr_built_norm
#Scatter Plot of  beds, baths, sq_ft, lot_size, and yr_built by price
ggplot(data=redfin1) + geom_point(mapping=aes(x=beds, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=baths, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=sq_ft, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=lot_size, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=yr_built, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=yr_built_norm, y=price)) #Normalized yr_built

#Scatter plot of beds, baths, sq_ft, lot_size, and yr_built by price with smoothing
qplot(beds, price, data = redfin1, geom = c('point','smooth'))
qplot(baths, price, data = redfin1, geom = c('point','smooth'))
qplot(sq_ft, price, data = redfin1, geom = c('point','smooth'))
qplot(lot_size, price, data = redfin1, geom = c('point','smooth'))
qplot(yr_built, price, data = redfin1, geom = c('point','smooth'))
qplot(yr_built_norm, price, data = redfin1, geom = c('point','smooth')) #Normalized yr_built


#Histograms of beds, baths, sq_ft, lot_size, and yr_built
ggplot(data=redfin1, aes(price)) + geom_histogram(bins = 50, binwidth = 40000)
ggplot(data=redfin1, aes(beds)) + geom_histogram(bins = 7, binwidth = 1)
ggplot(data=redfin1, aes(baths)) + geom_histogram(bins = 10, binwidth = .5)
ggplot(data=redfin1, aes(sq_ft)) + geom_histogram(bins = 6, binwidth = 1000)
ggplot(data=redfin1, aes(lot_size)) + geom_histogram()
ggplot(data=redfin1, aes(yr_built)) + geom_histogram(bins = 24, binwidth = 5)

ggplot(data=redfin1, aes(yr_built_norm)) + geom_histogram() #Normalized yr_built

#Create datframe with DVs and IV
input <- redfin1[,c('PRICE','BEDS','BATHS','SQUARE FEET','LOT SIZE','yr_built_norm')]
str(input)

#Correlation test using Pearson's product-moment correlation
cor.test(x=beds+baths+sq_ft+lot_size+yr_built_norm, y=price)
cor.test(x=beds, y=price)
cor.test(x=baths, y=price)
cor.test(x=sq_ft, y=price)
cor.test(x=lot_size, y=price)
cor.test(x=yr_built_norm, y=price)

#Correlation table
cor(x=input)

#Multiple regression of dataframe
lm_input <- lm(price~beds+baths+sq_ft+lot_size+yr_built_norm, data = input)
print(lm_input)

#Summary Statistic for multiple regression all variables
summary(lm_input)
anova(lm_input)
pairs(input)

#Multiple regression of dataframe with DV year_built_norm removed
lm_input_no_yr <- lm(price~beds+baths+sq_ft+lot_size, data = input)
print(lm_input_no_yr)

#Summary statistics for multiple regression - yr_built_norm
summary(lm_input_no_yr)
anova(lm_input_no_yr)

#Multiple regression of dataframe with DV beds removed
lm_input_no_beds <- lm(price~baths+sq_ft+lot_size+yr_built_norm, data = input)
print(lm_input_no_beds)

#Summary statistics for multiple regression - beds
summary(lm_input_no_beds)
anova(lm_input_no_beds)

#Multiple regression of dataframe with DV beds & yr_built_norm removed
lm_input_no_beds_yr <- lm(price~baths+sq_ft+lot_size, data = input)
print(lm_input_no_beds_yr)

#Summary statistics for multiple regression - beds
summary(lm_input_no_beds_yr)
anova(lm_input_no_beds_yr)
