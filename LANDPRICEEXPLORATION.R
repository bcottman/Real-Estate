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


setwd("~/Documents/Real.Estate/input/DB/")

# Listing DB 
fn = paste0("DB.Listings.Newberry29092014")
cat("Loading database file: ",fn,"\n")
load(fn)
#APPRAIER DB
fn = paste0("DB.NewberryByPartionSF.21092014")
cat("Loading database file: ",fn,"\n")
load(fn)

# land only
landProp = subset(prop,prop$type == "LAND")
landProp$PrAcre = landProp$price / (landProp$lsf / 64000)
landProp$acre = landProp$lsf / 64000
# land between 1,5 - 10 acres
landProp =subset(landProp, (landProp$acre > 2.49) & (landProp$acre < 9.9))
ggplot(landProp, aes(x = acre, y = PrAcre)) +
  geom_smooth(method = "lm") + geom_point() +
  scale_x_continuous(limits = c(0, 100)) + 
  scale_y_continuous(limits = c(0, 200000))
lf0 = lm(PrAcre ~ 0 + acre, data = landProp)
summary(lf0)