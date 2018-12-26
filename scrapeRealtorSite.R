## scrape realtor ste for listing of the city
## use pevious saved textedit to convert scraping to text
## saveas textedit file as ScrapeRealtorCITYDATE ( this format 29092014 read as Sep 29,2014)
## put desc CITY DATE as 1st line of file and then save again
## run cleanSrcapeOfRealtor.R , fix listing as necessary, and then save again
## run this file 
## creates binary R load file "~/Documents/Real.Estate/input/DB/" "DB.Listings.",CityName,ScrapeDate)
## -- goodluck
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
if(CityName == "High"){CityName = "High Spring"}
if(CityName == "La"){CityName = "La Crosse"}
ScrapeDate = descString[3]
colnames(realtorSource) = c("Lini")
realtorSource$Lini = str_trim(realtorSource$Lini,side = "both")
# sets off rules, remove it
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"A MASTERPIECE")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini, "Feedback")))
#
# land with no folowing size

insert.2elements.to.end = function(v, element1,element2){
  v[(length(v)+1)] = element1
  v[(length(v)+1)] = element2
  return(v)
}
insert.element.to.end = function(v, element1){
  v[(length(v)+1)] = element1
  return(v)
}
#veci = c(1,2)
#veci=insert.2elements.to.end(veci,3,4)
seqi = seq(1,nrow(realtorSource))
v = NULL
z = data.frame(realtorSource$Lini,seqi,stringsAsFactors =F)
vv =  sapply(z$seqi, function(x) if(str_detect(z[(x),1],"Land") &
                                                                !(str_detect(z[(x),1]," Land") |
                                                                  str_detect(z[(x+1),1],"Acres") |
                                                                     str_detect(z[(x+1),1],"Sq Ft"))) 
{insert.2elements.to.end(v,"1 Acre",z[(x+1),1])} 
else {insert.element.to.end(v,z[(x+1),1])}

,USE.NAMES=FALSE)
vv = unlist(vv)
vvv=NULL
vvv[1]=realtorSource$Lini[1]
vvv[2:length(vv)] = vv[1:length(vv)-1]
realtorSource=data.frame(vvv)
colnames(realtorSource) = c("Lini")
#
# no address fix
realtorSource$Lini = str_replace(realtorSource$Lini,"No Listed Address", "100 Nw SemiSlimy St No GIVEN Address")
#
#fix slimeballs, who only puts city, state sometimes or state but cany do street and state yet

realtorSource$Lini = sapply(realtorSource$Lini, function(x) 
  if(str_detect(x,"FL ") & !str_detect(x,CityName) & (str_count(x,",") == 0))
  {paste0("100 SLIME ST NO CITY , ",CityName,", ",x) } 
  else {x}, USE.NAMES=FALSE)

realtorSource$Lini = sapply(realtorSource$Lini, function(x) 
  if(str_detect(x,"FL ") & str_detect(x,CityName)  &  (str_count(x,",") == 1)) 
   {paste0("100 SLIME ST NO STREET ADDRESS, ",x) } 
   else {x}, USE.NAMES=FALSE)
#
# fix missing address problem
# change photo line for a listing with one address
seqi = seq(1,nrow(realtorSource))
z = data.frame(realtorSource$Lini,seqi,stringsAsFactors =F)
ldf = nrow(z)
realtorSource$Lini =  sapply(z$seqi, function(x) if((x > 1) & (x < ldf )) { if(str_detect(z[(x),1],"Photo") &
                                                                               (z[(x-1),1] != z[(x+1),1]) &
                                                                                 str_detect(z[(x+1),1],"FL ") & 
                                                                                 str_detect(z[(x+1),1],CityName) &
                                                                                 (str_count(z[(x+1),1],",") == 2))
                                                                          {z[(x+1),1]} 
                                                                        else {z[x,1]}
                                                 } else {z[x,1]}
                        ,USE.NAMES=FALSE)
# 1st spurious=s comment elimination
seqi = seq(1,nrow(realtorSource))
z = data.frame(realtorSource$Lini,seqi,stringsAsFactors =F)
ldf = nrow(z)
realtorSource$Lini = sapply(z$seqi, function(x) if((x > 1) & (x < ldf )) { if(str_detect(z[(x-1),1],"Save Listing") &
                                                                 ( z[(x),1] == z[(x+1),1]) &  # spurious comments come in pairs
                                                                   !(str_detect(z[x,1],"FL ") | 
                                                                   str_detect(z[x,1],CityName) |
                                                                   (str_count(z[x,1],",") == 2)))    
                                                                  {" "} 
                                                            else {z[x,1]}
                                                  } else {z[x,1]}
                             ,USE.NAMES=FALSE)
# 2nd spurious=s comment elimination
seqi = seq(1,nrow(realtorSource))
z = data.frame(realtorSource$Lini,seqi,stringsAsFactors =F)

realtorSource$Lini = sapply(z$seqi, function(x) if(x > 2) { if(str_detect(z[(x-2),1],"Save Listing") &
                                                                 ( z[(x-1),1] == " ") &   # dealt with rule before
                                                                 !(str_detect(z[x,1],"FL ") |
                                                                 str_detect(z[x,1],CityName) |
                                                                 (str_count(z[x,1],",") == 2))) 
                                                                  {" "} 
                                                            else {z[x,1]}
                                                } else {z[x,1]}
                                  ,USE.NAMES=FALSE)
#eliminate blank lines
realtorSource = subset(realtorSource,!(realtorSource$Lini == "" | realtorSource$Lini == " " | realtorSource$Lini == "?" ))
seqi = seq(1,nrow(realtorSource))
z = data.frame(realtorSource$Lini,seqi,stringsAsFactors =F)
# spurious comment after photo
realtorSource$Lini = sapply(z$seqi, function(x) if(x > 1) { if(str_detect(z[(x-1),1],"Photo") &
                                                !(str_detect(z[x,1],"FL ") |
                                                    str_detect(z[x,1],CityName) |
                                                    (str_count(z[x,1],",") == 2)))
                                                              {" "} 
                                                            else {z[x,1]}
                                                  } else {z[x,1]}
                      ,USE.NAMES=FALSE)

colnames(realtorSource) = c("Lini")
# remove nonsense
###
realtorSource$Lini = str_trim(realtorSource$Lini,side = "both")

realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"listings")))

#most extra descriptions use a !
realtorSource$Lini = str_replace(realtorSource$Lini,"[0-9]{0,} Full Ba Land","Land")
# #"Top Rated Schools!!"
# #str_detect("Top Rated Schools!!","\\!")
 realtorSource = subset(realtorSource,!str_detect(realtorSource$Lini,"\\!"))
# # "Stunning LIKE NEW home in Turnberry Lake" 
 realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Sorting")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Stunning")))
# # "Open House 
 realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Open House")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Save Listing")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Find Your Way Home.")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Large Enough")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Where Customers are Family")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Expertise close to home")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Check Home Values")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Luxurious urban living in")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Click Here")))


#realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"St. Augustine")))

realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"SHORT SALE")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"FORECLOSURE")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Bronson Florida")))

realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Photos")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Photo")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Community:")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Great Home")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Plantation at")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini," Call ")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini," Car Ready  ")))
realtorSource = subset(realtorSource,is.na(str_match(realtorSource$Lini,"Classic Properties "))
                        
realtorSource$Lini = str_replace(realtorSource$Lini,"From \\$","\\$")


realtorSource$Lini = str_replace(realtorSource$Lini,"C-21 ","CENTURY 21 ")
realtorSource$Lini = str_replace(realtorSource$Lini,"Brokered by: ","")
###
realtorSource$Lini = str_trim(realtorSource$Lini,side = "right")
#eliminate blank lines
realtorSource = subset(realtorSource,(realtorSource$Lini != "" & realtorSource$Lini != "?" ))
#

addresses = realtorSource$Lini[as.numeric(seq(1,nrow(realtorSource),6))]
#NO STREET ADDRESS ADD ONE
mini = min(999,length(addresses))
addresses[1:mini]
realtorSource$Lini[(250*6):(250*6+50)]
mini = min(1999,length(addresses))
if( mini > 999) addresses[1000:mini] else 0
# Put in Cir, when non
la = paste0(", ",CityName,", FL ")
addresses = str_replace(addresses , " [A-Z][a-z], FL | [A-Z][a-z][a-z], FL " , la)
#addresses = str_replace(addresses , "[a-z] Gainesville" , ", Gainesville")
##split addresses
l=strsplit(addresses, ",")
l=unlist(l)
ln = length(l)
#l[1:999]
#l[1000:1999]
street = l[as.numeric(seq(1,ln,3))]
street
city = l[as.numeric(seq(2,ln,3))]
city
state = l[as.numeric(seq(3,ln,3))]
state = str_trim(state,side = "both")
state
l=strsplit(state, " ")
l=unlist(l)
ln = length(l)
state = l[as.numeric(seq(1,ln,2))]
state
zip = l[as.numeric(seq(2,ln,2))]
#
zip
##transform val into price
val = realtorSource$Lini[as.numeric(seq(3,nrow(realtorSource),6))]
val
val = str_sub(val,2)
price = as.numeric(str_replace_all(val,",",""))
price[1:999]
##desc
desc = realtorSource$Lini[as.numeric(seq(4,nrow(realtorSource),6))]
descS = desc
desc
## split up desc

## property type
#desc = unlist(desc)
descS[1:mini]
typi = sapply(descS, function(x) if(x == "Land")
  {"Land"}
  else if(str_detect(x," Single Family Home"))
{ "Single Family Home"}
 else if(str_detect(x,"Multi-Family Home"))
{ "Multi-Family Home"}
  else if(str_detect(x," Condo/Townhome/Row Home/Co-Op"))
{"Condo/Townhome/Row Home/Co-Op"}
else if(str_detect(x,"Mfd/Mobile Home"))
{"Mfd/Mobile Home"}
else if(str_detect(x,"Farms/Ranches"))
{"Farms/Ranches"}
else
{"XXUNKXX"}
  ,USE.NAMES=FALSE)
typi = unlist(typi)
typi
desc = str_replace(desc,"Land","")
desc = str_replace(desc," Single Family Home","")
desc = str_replace(desc,"[0-9]{1,} Units Multi-Family Home","")
desc = str_replace(desc," Condo/Townhome/Row Home/Co-Op","")
desc = str_replace(desc," Mfd/Mobile Home","")
desc = str_replace(desc," Farms/Ranches","")
desc = str_replace(desc,"Farms/Ranches","")
desc
 ## number of bedrooms
bdrm = str_sub(desc,1,1)
bdrm = str_replace(bdrm,"","0")
bdrm = as.numeric(bdrm)
bdrm
desc = str_replace(desc,"[0-9]{1} Bd ","")
#desc
## number of bathrooms
bthrm = str_sub(desc,1,1)
bthrm = str_replace(bthrm,"","0")
bthrm = as.numeric(bthrm)
bthrm
desc = str_replace(desc,"[0-9]{1} Ba","")
desc = str_replace(desc,"[0-9]{1} Full Ba","")
desc = str_replace(desc,"[0-9]{1} Full, ","")
#
hbthrm = str_sub(desc,1,1)
hbthrm = str_replace(hbthrm,"","0")
hbthrm = as.numeric(hbthrm)
hbthrm
bthrm = bthrm + 0.5*hbthrm
sss = data.frame(descS,bdrm,bthrm,stringsAsFactors =F)
#size
size = realtorSource$Lini[as.numeric(seq(5,nrow(realtorSource),6))]
size = str_replace(size,"Sq Ft [0-9]{1,} Units","Sq Ft")
#Acres 4 Units
size = str_replace(size,"Acres [0-9]{1,} Units","Sq Ft")
#
size

htt = c(
  !str_detect(typi,"Land")  & 
  (str_detect(typi,"Single Family Home") | 
  str_detect(typi,"Multi-Family Home") | 
  str_detect(typi,"Mobile Home") |
  str_detect(typi,"Farms/Ranches") |
  str_detect(typi,"Condo/Townhome/Row Home/Co-Op")) )



seqi = seq(1,length(size))
z = data.frame(size,htt,!htt,seqi,stringsAsFactors =F)
htsf = sapply(z$seqi, function(x) if(z[x,2]){
  str_sub(z[x,1],1,(str_locate(z[x,1]," ")[2]-1))
} else if(z[x,3]) 
{ "0"}
,USE.NAMES=FALSE)
#htsf = unlist(htsf)
htsf = str_replace(htsf,",","")
htsf = as.numeric(htsf)
acreMax = 100
htsf = sapply(htsf, function(x) if(x < acreMax){
  x * 64000
} else if(x >= acreMax) 
{ x }
else if(is.na(x)) 
{ 0 }
,USE.NAMES=FALSE)
htsf
# lot sf
lsf =sapply(z$seqi, function(x) if(z[x,3]){
  str_sub(z[x,1],1,(str_locate(z[x,1]," ")[2]-1))
} else if(z[x,2]) 
{ str_replace(z[x,1],"[0-9]{0,}[,]{0,}[0-9]{1,} Sq Ft | [0-9]{1,} Sq Ft |[0-9]{0,}[,]{0,}[0-9]{1,} Sq Ft","")}
,USE.NAMES=FALSE)
lsf

lsf = str_replace(lsf," Sq Ft Lot","")
lsf = str_replace(lsf," Sq Ft","")
lsf = str_replace(lsf," Acres","")
lsf = str_replace(lsf,"Lot","")
lsf = str_replace(lsf,"","0")
lsf
lsf = str_replace(lsf,",","")
lsf = as.numeric(lsf)
# convert acres to sf
lsf = sapply(lsf, function(x) if((x < acreMax & x > 0))
  {x * 64000}
 else if(x >= acreMax) 
{ x }
else if(x <= 0 ) 
{ x }
,USE.NAMES=FALSE)
lsf = if(is.list(lsf)) unlist(lsf) else lsf

sss = data.frame(typi,size,htsf,lsf,stringsAsFactors =F)

## still have to handle residences ##
#brokers
brokers = realtorSource$Lini[as.numeric(seq(6,nrow(realtorSource),6))]
brokers
#realtorSource$Lini[as.numeric(seq(1,nrow(realtorSource),1))]
# create poperty df and save it

prop = data.frame(toupper(street),toupper(city),toupper(state),  zip,  price,  toupper(typi),bdrm,  bthrm,  htsf, lsf,   toupper(brokers),stringsAsFactors =F)
colnames(prop) = c("street",      "city",       "state",        "zip","price", "type",      "bdrm","bthrm","htsf","lsf","broker")
setwd("~/Documents/Real.Estate/input/DB/")
fn = paste0("DB.Listings.",CityName,".",ScrapeDate,".rds")
# create USE rules
prop$use = str_replace(prop$type,"SINGLE FAMILY HOME","Single Family")
prop$use = str_replace(prop$use,"MFD/MOBILE HOME","Mobile Home")
prop$use = str_replace(prop$use,"MULTI-FAMILY HOME","Multifamily")
prop$use = str_replace(prop$use,"FARMS/RANCHES","Misc. Residence")
prop$use = str_replace(prop$use,"CONDO/TOWNHOME/ROW HOME/CO-OP","Condominium")
prop$use = str_replace(prop$use,"LAND","Vacant")


#fn
prop=unique(prop)
saveRDS(prop,file = fn)
### cleaning
table(prop$city)
table(prop$state)
pt=table(prop$zip)
pt
round(prop.table(pt)*100,1)
#
### graphs
#theme
theme_slanted_axis = function(){
  theme(plot.title=element_text(size=16,face="bold", vjust=2)) +
    theme(axis.text.x = element_text(angle = 50, vjust=1.0, hjust = 1,face="bold",colour = "black")) +
    theme(axis.text.y = element_text(face="bold",colour = "black")) + 
    theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
}
setwd("~/Documents/Real.Estate/output/graphs/")
fnn = paste0(CityName,".Zip.Histogram.png")
png(filename=fnn)
bw=1
p = ggplot(prop,aes(x=zip)) 
p +  geom_histogram(binwidth = bw,aes(fill = ..count..)) +  
       labs(title = CityName) +
        scale_fill_gradient("Count", low = "red", high = "green") +
        stat_bin(binwidth=bw, geom="text",size=3, aes(label=..count..), vjust=-.5) + 
          theme_slanted_axis() 
dev.off()
 #prop[37,4]=32618
table(prop$type)
table(prop$bdrm)
table(prop$bthrm)
table(prop$broker)
prop[prop$type=="XXUNKXX",]
