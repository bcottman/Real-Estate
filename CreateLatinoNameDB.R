rm(list = ls(all = TRUE)) #CLEAR WORKSPACE

#install.packages("xlsx")
library(xlsx)

# READ IN FILE
# SET WD
# because on sharable folder
setwd("~/Dropbox/realestate.output/")

LatinoNames = read.xlsx("list of hispanic last names.xlsx",sheetIndex=1,stringsAsFactors = F)
LatinoNames$LastName = toupper(LatinoNames$LastName)
LatinoNames = unique(LatinoNames)

# DB are usually binary and internal use
setwd("~/Documents/Real.Estate/input/DB/")
saveRDS(LatinoNames,file="DB.LatinoNames.21092014.rds")
#
rm(list = ls(all = TRUE)) #CLEAR WORKSPACE

ln = readRDS("DB.LatinoNames.21092014.rds")
