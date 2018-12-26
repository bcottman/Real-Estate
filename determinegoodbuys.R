rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
#unlink("~/.RData")

#
invisible(require(XLConnect))
#install.packages("xlsx")
invisible(require(xlsx))
invisible(require(stringr))
#install.packages("dplyr")
#require(dplyr)
invisible(require(ggplot2))
#install.packages("lubridate")
require(lubridate)
#install.packages("bitops")
require(bitops)
require(RCurl)
#install.packages("scales")
require(scales)

source("~/Dropbox/Public/Real.Estate/code/Zillow/R/zillow2.R")
source("~/Dropbox/Public/Real.Estate/code/Zillow/R/zillowErrors.R")

setwd("~/Documents/Real.Estate/input/DB/")

# kisting DB 
fn = paste0("DB.Listings.Gainesville.21092014")
cat("Loading database file: ",fn,"\n")
load(fn)
valProp = subset(prop,prop$htsf > 0)
valProp$value = valProp$price/valProp$htsf
valProp = valProp[order(valProp$value, valProp$price),]
# look at properties prices under 50,000 anf 50 sf
buyProp = subset(valProp,( (valProp$value < 35) & 
                             (valProp$price < 100000) &
                             (valProp$type == "SINGLE FAMILY HOME")) )

seqi = as.numeric(seq(1,nrow(buyProp)))
#seqi = as.numeric(seq(1,5))
ii=seqi[1]
buyProp$ratio = 0
buyProp$zest = 0
for(ii in seqi){
  
  cat("TargetStreet:",buyProp$street[ii],
      " TargetZip:", buyProp$zip[ii],"\n")

#  nbh$SaleDate = as.Date(buyProp$SaleDate, "%m/%d/%y")
options(show.error.messages = TRUE)
x = try(zestimate(buyProp$street[ii], buyProp$zip[ii], "X1-ZWz1ducn32itqj_4zss3"))
buyProp$zest[ii] = 0
buyProp$zest[ii] = try(x[1,1])
buyProp$ratio[ii] = 0
buyProp$ratio[ii] = try(as.numeric(buyProp$zest[ii])/as.numeric(buyProp$price[ii]))
}


buyProp = buyProp[order(-buyProp$ratio,buyProp$value, buyProp$price),]
