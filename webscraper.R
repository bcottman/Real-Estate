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
page = 29
url_base = "http://www.gainesvillemls.com/gan/main.php"
#url      = paste0(url_base, "pg-", page, "?pgsz=50")
page     = htmlTreeParse(url_base, useInternalNodes = T)
page
