rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
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
#install.packages("psych")
library(ggplot2)


# SET TO URL
#options(browser="/usr/bin/open -a 'Safari'")
# CREATE FILE FOM WEB PAGE
#browseURL("http://www.realtor.com/realestateandhomes-search/32605?pgsz=50")
## CREATE FILE FOM WEB PAGE
# browseURL("http://www.realtor.com/realestateandhomes-search/Gainesville_FL?pgsz=50")
# browseURL("http://www.realtor.com/realestateandhomes-search/Gainesville_FL/pg-2?pgsz=50")
########
# 0. USE TEXT EDIT ON EXISTING .TXT FILE
#1 cut & paste into textediy
# 2 - copy into clipboard
# get rid of Lake Butler, Chiefland, Jonesville, Earleton,Apulachua,Newberry,Brooker amd Achula listings for gainesvile
realtorSource = read.delim(pipe("pbpaste"))
# change from factors to strings
realtorSource = data.frame(lapply(realtorSource, as.character), stringsAsFactors=FALSE)
######
############
descString = colnames(realtorSource)
descString = unlist(str_split(descString,"\\."))
CityName = descString[2]
ScrapeDate = descString[3]
colnames(realtorSource) = c("Lini")
realtorSource$Lini = str_trim(realtorSource$Lini,side = "both")
# find entries that do not natch or do not hAVE CITY
realtorSource$Lini = sapply(realtorSource$Lini, function(x) 
  if(str_detect(x,"FL") & !(str_detect(x,CityName) ))
  {cat(x,"\n") } 
  else {x}, USE.NAMES=FALSE)