# XML
#install.packages("XML")
library(XML)
#
library(XLConnect)
#install.packages("xlsx")
library(xlsx)
library(stringr)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)

# SET TO URL or "Google Chrome"
options(browser="/usr/bin/open -a 'Safari'")
# CREATE FILE FOM WEB PAGE
browseURL("http://www.acpafl.org/salessearch.asp")
# READ IN FILE
# SET WD
setwd("~/Documents/Real.Estate/input/nhhd.sales")
# broadmoor Sales201493113927.csv
nhb = read.csv("Sales201493123745.csv",stringsAsFactors = F)
p = ggplot(nhb,aes(x=as.Date(SaleDate))) 
#  scale_x_discrete(breaks=seq(as.Date("2006-01-01"),as.Date("2013-12-31"),91))
# yearly
p +  geom_histogram(binwidth = 365,aes(fill = ..count..)) +  scale_fill_gradient("Count", low = "green", high = "red")
## quartely
p +  geom_histogram(binwidth = (365/4),aes(fill = ..count..)) +  scale_fill_gradient("Count", low = "green", high = "red")

lf0 = lm(SalePrice ~ HtdSqFt, data = nhb)
summary(lf0)
# Scatterplot with confidence interval around the regression line
ggplot(nhb, aes(x = HtdSqFt, y = SalePrice)) + geom_smooth(method = "lm") + geom_point() 
#nhb$bld2 = nhb$BldgVal + nhb$MiscVal
#
lf0 = lm(BldgVal ~ HtdSqFt, data = nhb)
summary(lf0)
# Scatterplot with confidence interval around the regression line
ggplot(nhb, aes(x = HtdSqFt, y = BldgVal)) + geom_smooth(method = "lm") + geom_point() 

# set total value of prop
nhb$Value = nhb$AgLandVal + nhb$AgLandVal + nhb$MrktLandVal + nhb$BldgVal + nhb$MiscVal
# sort descending by value
nhb = nhb[order(-nhb$Value),] 
lf0 = lm(Value ~ HtdSqFt, data = nhb)
summary(lf0)
# Scatterplot with confidence interval around the regression line
ggplot(nhb, aes(x = HtdSqFt, y = Value)) + geom_smooth(method = "lm") + geom_point() 
# sales price/ tax appraisal vale
ggplot(nhb, aes(x = Value, y = (SalePrice/Value))) + geom_smooth(method = "lm") + geom_point() 

