install.packages("XLConnect") #installs package to read Excel spreadsheets
install.packages("tidyverse")


getwd()

library(rJava)
library(xlsx)
library(readxl) 
library(tidyverse)

#Update file path before /redfin_2019... to reflect your own if getwd() doesn't work for you.
redfin1 <- read_excel("~/MSBA/MSBA Git/540-1/redfin_2019-09-24-18-15-38.xlsm", sheet = "redfin_2019-09-24-18-15-38") #puts data in Global Environment#

view(redfin1)

summary(redfin1)

summary_redfin1 <- summary(redfin1)

price <-(redfin1$`PRICE`)
beds <-(redfin1$`BEDS`)
baths <-(redfin1$`BATHS`)
sq_ft <-(redfin1$`SQUARE FEET`)
lot_size <-(redfin1$`LOT SIZE`)
yr_built <-(redfin1$`YEAR BUILT`)


#Scatter Plot of  beds, baths, sq_ft, lot_size, and yr_built by price
ggplot(data=redfin1) + geom_point(mapping=aes(x=beds, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=baths, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=sq_ft, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=lot_size, y=price))
ggplot(data=redfin1) + geom_point(mapping=aes(x=yr_built, y=price))

#Scatter plot of beds, baths, sq_ft, lot_size, and yr_built by price with smoothing
qplot(beds, price, data = redfin1, geom = c('point','smooth'))
qplot(baths, price, data = redfin1, geom = c('point','smooth'))
qplot(sq_ft, price, data = redfin1, geom = c('point','smooth'))
qplot(lot_size, price, data = redfin1, geom = c('point','smooth'))
qplot(yr_built, price, data = redfin1, geom = c('point','smooth'))


#Histograms of beds, baths, sq_ft, lot_size, and yr_built
ggplot(data=redfin1, aes(price)) + geom_histogram(bins = 50, binwidth = 40000)
ggplot(data=redfin1, aes(beds)) + geom_histogram(bins = 7, binwidth = 1)
ggplot(data=redfin1, aes(baths)) + geom_histogram(bins = 10, binwidth = .5)
ggplot(data=redfin1, aes(sq_ft)) + geom_histogram(bins = 6, binwidth = 1000)
ggplot(data=redfin1, aes(lot_size)) + geom_histogram()
ggplot(data=redfin1, aes(yr_built)) + geom_histogram(bins = 24, binwidth = 5)

