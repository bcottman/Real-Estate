Scape zillow for neighborhood tables
========================================================

general method to clear workspacd


```r
# Setup by clearing all objects from work space
rm(list = ls(all = TRUE))  #CLEAR WORKSPACE
```

general way to scape out tables, if any


```r
library(XML)

theurl = seq(to = 10)
theurl[1] = "http://www.zillow.com/homedetails/10106-NW-13th-Ave-Gainesville-FL-32606/42712542_zpid/"
theurl[2] = "http://www.zillow.com/homes/for_sale/house,townhouse_type/3-_beds/1-_baths/200000-400000_price/761-1521_mp/built_sort/29.678732,-82.441921,29.653263,-82.471747_rect/14_zm/3ecae538b6X1-CRzncwymrdu7jh_10veyp_crid/"
# theurl <- 'http://en.wikipedia.org/wiki/Brazil_national_football_team'
tables <- readHTMLTable(theurl[1])
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
d = as.data.frame(tables[1])
```

Another way




