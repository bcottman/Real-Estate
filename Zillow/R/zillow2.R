# getForm("http://www.zillow.com/webservice/GetSearchResults.htm",
#         'zws-id' = zillowId, 
#         address = "1292 Monterey Avenue",
#         citystatezip = citystatezip,
#         ...)
library(RCurl)
library(XML)
zestimate =
  #
  # v = zestimate("1292 Monterey Avenue", "94707", zillowId)
  #
  #
function(address, citystatezip, zillowId = getOption("ZillowId", stop("need zillow id")), ...)  
{
  reply = getForm("http://www.zillow.com/webservice/GetSearchResults.htm",
                  'zws-id' = zillowId, 
                  address = address,
                  citystatezip = citystatezip,
                  ...)

  doc = xmlParse(reply, asText = TRUE)

  checkStatus(doc)
  
  zpid = xmlValue(doc[["//result/zpid"]])
  est = doc[["//result/zestimate"]]  
  data.frame(amount = as.numeric(xmlValue(est[["amount"]])),
             low = as.numeric(xmlValue(est[["valuationRange"]][["low"]])),
             high = as.numeric(xmlValue(est[["valuationRange"]][["high"]])),
             valueChange30Day = as.numeric(xmlValue(est[["valueChange"]])),             
             row.names = zpid )
}

checkStatus =
function(doc)
{
  msg = doc[["//message"]]
  code = xmlValue(msg[["code"]])

  if(!is.null(msg[["limit-warning"]]))
    warning("reaching the call limit of your Zillow Id")
  
  if(code == "0")
    return(TRUE)

  i = match(code, zillowErrorTable[, 1])
  e = simpleError(zillowErrorTable[i, 3])
  class(e) = c(zillowErrorTable[i, 2], class(e))
  stop(e)
}

getCompComps =
  function(id, zillowId, count = 30, verbose = FALSE, info = getComps(id, zillowId, count))
  {
    if(is.data.frame(id))
      info = id
    
    info$zid = rownames(info)
    
    tmp = lapply(rownames(info)[-1],
                 function(x) {
                   if(verbose)
                     cat(x, "\n")
                   try(getComps(x, zillowId))
                 })
    tmp = tmp[ sapply(tmp, inherits, "data.frame") ]
    
    tmp = lapply(tmp, function(x) { x$zid = rownames(x) ; x})
    other = do.call("rbind", tmp)
    ans = rbind(info, other)
    structure(ans[!duplicated(ans$zid), ], class = c("ExtendedZillowComparables", "ZillowComparables", class(ans)))
  }


getComps =
function(id, zillowId = getOption("ZillowId", stop("need zillow id")), count = 30, ...)
{
     # if the caller gives us an address rather than a zpid, find the zpid.
  if(!is.numeric(id) && length(grep("[^[:digit:]]", id))) {
    id = rownames(zestimate(id[1], id[2], zillowId))[1]
  }

  txt = getForm("http://www.zillow.com/webservice/GetDeepComps.htm",
                 zpid = id, 'zws-id' = zillowId,  count = count, ...)

  doc = xmlParse(txt, asText = TRUE)

  checkStatus(doc)
  
  comps = doc["//response//comparables[.//links]"][[1]]

  principal = doc[["//response/properties/principal"]]
  ans = compToDataFrame(principal)  
  ans = rbind(ans, do.call("rbind",  c(xmlApply(comps, compToDataFrame))))
  rownames(ans) = c(xmlValue(principal[["zpid"]]), xmlSApply(comps, function(x) xmlValue(x[["zpid"]])))

  class(ans$lastSold) = c("POSIXt", "POSIXct")
  class(ans) = c("ZillowComparables", class(ans))
  
  ans
}

compToDataFrame =
function(node, deep = TRUE)
{
  est = node[["zestimate"]]
  add = xmlToList(node[["address"]])
  if(length(add$latitude) == 0)
    add$latitude = NA
  if(length(add$longitude) == 0)
    add$longitude = NA  
     
  ans =
   data.frame(amount = as.numeric(xmlValue(est[["amount"]])),
             low = as.numeric(xmlValue(est[["valuationRange"]][["low"]])),
             high = as.numeric(xmlValue(est[["valuationRange"]][["high"]])),
             valueChange30Day = as.numeric(xmlValue(est[["valueChange"]])),
             street = add[["street"]],
             zip = add$zipcode,
             latitude = as.numeric(add$latitude),
             longitude = as.numeric(add$longitude),             
             row.names = xmlValue(node[["zpid"]] ))
  if(deep) {
    sapply(c("taxAssessmentYear", "taxAssessment", "yearBuilt", "lotSizeSqFt", "finishedSqFt",
              "bathrooms", "bedrooms", "lastSoldPrice"),
            function(id) {
                   ans[1, id] <<- as.numeric(if(!is.null(node[[id]])) xmlValue(node[[id]]) else NA)
            })
    
    ans[1, "lastSold"] = if(!is.null(node[["lastSoldDate"]]))
                            as.POSIXct(strptime(xmlValue(node[["lastSoldDate"]]), "%m/%d/%Y"))
                         else
                            NA
  }
  ans[1,"score"] =  xmlGetAttr(node, "score", NA, as.numeric)

  ans
}
          


plot.ZillowComparables =
function(x, threshold = NA, ...)
{
  opts = par(no.readonly = TRUE)
#  i = match(c("cin", "cra", "csi", "cxy", "din"), names(opts))
  on.exit(par(opts))
#  par(mfrow = c(1, 1))
#  par(mfrow = c(1, 2))

#  boxplot(v$amount, horizontal = TRUE)
#  points(v$amount[1], 1, col = "red")

  bedrooms = ordered(x$bedrooms)
  bathrooms = ordered(x$bathrooms)  
  plot(x$finishedSqFt, x$amount, col = rainbow(length(levels(bathrooms)))[bathrooms],
        ylim = c(min(x$low), max(x$high)),
        xlab = "Sq Ft.", ylab = "Price")
  text(x$finishedSqFt, x$amount, as.character(bedrooms))
  x$score[1] = 1.0
  score = cut(x$score, 6)
  cols = rainbow(length(levels(score)))
  sapply(seq(length = nrow(x)),
          function(i) lines(rep(x$finishedSqFt[i], 2), c(x$low[i], x$high[i]),
                            col = if(i == 1) "blue" else cols[score[i]],
                            lwd = if(i == 1) 2 else 1))


  if(!is.na(threshold))
    abline(h = threshold, col = "yellow", lty = 2)

  plot(amount/finishedSqFt ~ lotSizeSqFt, x,
            col = c("red", rep("blue", nrow(x) - 1)))
  
  invisible(x)
}
#zestimate("10106 NW 13TH AVE", "32606", "X1-ZWz1ducn32itqj_4zss3")
#remove(df)
#
#df=getComps(c("10106 NW 13TH Avenue", "32606"), "X1-ZWz1ducn32itqj_4zss3", count = 10)


compute.more.variables = function(nf)
{
  nf$cpsf = nf$amount/nf$finishedSqFt
  nf$lcpsf = nf$lastSoldPrice/nf$finishedSqFt
  nf$iolp = (nf$cpsf/nf$lcpsf - 1)
  return(nf)
}


#
#calc distance in miles
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# input df, return df wuih variable addes,unit miles
compute.miles = function(nf)
{
  nf$z=0
  for (i in 2:nrow(nf) ) {
    nf$z[i]=earth.dist(nf$longitude[1],nf$latitude[1],nf$longitude[i],nf$latitude[i])*0.62137 #miles
  }
  return(nf)
}

valuation = function(dfx)
{
  print(c("zillow value",signif(dfx[1,2],6),signif(dfx[1,1],6),signif(dfx[1,3],6)))
  mv=mean(dfx[-1,]$lcpsf)
  mev=median(dfx[-1,]$lcpsf)
  sdv=sd(dfx[-1,]$lcpsf)
  lv = mv - sdv
  hv = mv + sdv
  mer = nrow(subset(dfx, (dfx$lcpsf >= mev )))
  lr = nrow(subset(dfx, (dfx$lcpsf >= lv )))
  hr = nrow(subset(dfx, (dfx$lcpsf >= hv )))
  print(c("comparables range",signif((lv*dfx[1,13]),6),signif((mev*dfx[1,13]),6),signif((hv*dfx[1,13]),6)))
  print(c("comparables count",signif(lr,2),signif(mer,2),signif(hr,2)))
#  setwd("~/Dropbox/Public/Real.Estate/figure/")
  
#  par(mfrow=c(1,2))
#plot(dfx$finishedSqFt,dfx$amount)
#  png(filename = "plot2.png",width = 640, height = 480)

#  plot(dfx$finishedSqFt,dfx$amount)
#  boxplot(dfx$lcpsf)
#  dev.off()
}
#
lhcompf = function(address,zip)
{

  dfx=getComps(c(address, zip), "X1-ZWz1ducn32itqj_4zss3", count = 11)
  dfx=compute.more.variables(dfx)
  dfx=compute.miles(dfx)
  return(dfx)
}
lhcomp = function(address,zip)
{
#  remove(df)
#  address="10240 NW 11TH lane"; zip="32606"
  print(c(address,zip))
# remove(zip)
  dfz = lhcompf(address,zip)
  valuation(dfz)
}
# lhcomp("14417 NE CR 1471", "32694")
#dff=lhcompf("14417 NE CR 1471", "32694")
# lhcomp("2513 nw 31st avenue", "32605")
# lhcomp("10240 NW 11TH lane", "32606")
# lhcomp("10106 NW 13th ave", "32606")
#dfc = lhcompf("10106 NW 13th ave", "32606")
# lhcomp("35 battle flagg rd", "01730")
# #501 SW 75th St H-7 Street Unit:H-7 Gainesville, FL 32607
# lhcomp("852 SW 58th Ter", "32607")
# lhcomp("4031 NW 17th Ter,", "32605")
# lhcomp("501 SW 75th Street", "32607")
# lhcomp("2408 nw 26th street", "33431")
# lhcomp("3221 SE 22 Place", "32641")
# lhcomp("10050 NE 88th Ln", "32621")

