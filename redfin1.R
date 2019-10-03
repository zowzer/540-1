install.packages("XLConnect") #installs package to read Excel spreadsheets
install.packages("tidyverse")
install.packages("ggplot2")


getwd()

library(rJava)
library(xlsx)
library(readxl) 
library(tidyverse)

#Update file path before /redfin_2019... to reflect your own if getwd() doesn't work for you.
redfin1 <- read_excel("~/MSBA/MSBA Git/540-1/redfin_2019-09-24-18-15-38.xlsm", sheet = "redfin_2019-09-24-18-15-38") #puts data in Global Environment

view(redfin1)

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

#Correlation evals
cor(beds, price)
cor(baths, price)
cor(sq_ft, price) #probably highest correlation
cor(lot_size, price)
cor(yr_built, price)
cor(redfin1$yr_built_norm, price) #Same as yr_built

# OLS plots (once loop between lines 87-97 is validated, this section can be deleted)
redfin1 %>%
  ggplot(aes(x = sqrt(beds), y = sqrt(price))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA) 
redfin1 %>%
  ggplot(aes(x = sqrt(sq_ft), y = sqrt(price))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)
redfin1 %>%
  ggplot(aes(x = sqrt(lot_size), y = sqrt(price))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)
redfin1 %>%
  ggplot(aes(x = sqrt(yr_built), y = sqrt(price))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)

# OLS plots via for loop (verify accuracy by checking against individual plots between lines 69-85)
Y <- sqrt(price)
df <- data.frame(A = sqrt(sq_ft), B = sqrt(lot_size), C = sqrt(yr_built), Y = Y)
colNames <- names(df)[1:3]
for(i in colNames){
  plt <- ggplot(df, aes_string(x=i, y = Y)) +
    geom_point(color="red") +
    geom_smooth(method=lm, fill=NA)
  print(plt)
  Sys.sleep(2)
}


#OLS model
lmodel <- lm(sqrt(price) ~ sqrt(yr_built), data = redfin1)
lmodel$coefficients
summary(lmodel) #statistical info generated here
