require(RgoogleMaps)
require(stringr)
require(plyr)
require(useful)

# get the data on villages
villages <- read.csv(file="C:/users/Jared/week2/data/cspr1.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

# get just rows that have valid data for lat/long
villGood <- villages[with(villages, Lat != "" & Long != ""), ]

# correct missing decimal place
villGood[which(villGood$Long == "096° 3899'"), c("Lat", "Long")] <- c("16° 35.72'", "096° 38.99'")

## break lat/long into usable data
# sub out characters
villGood$Lat <- stringr::str_replace_all(string=villGood$Lat, pattern="°|'", replacement="")
villGood$Long <- stringr::str_replace_all(string=villGood$Long, pattern="°|'", replacement="")
# break into two parts: degrees and minutes
villGood[, c("LatDeg", "LatMin")] <- ldply(stringr::str_split(string=villGood$Lat, pattern=" "), .fun=function(x){names(x) <- c("LatDeg", "LatMin"); as.numeric(x)})
villGood[, c("LongDeg", "LongMin")] <- ldply(stringr::str_split(string=villGood$Long, pattern=" "), .fun=function(x){names(x) <- c("LongDeg", "LongMin"); as.numeric(x)})
# combine into one decimal measure
villGood <- transform(villGood, Latitude=LatDeg + LatMin/60, Longitude=LongDeg + LongMin/60)

# get bounded box for map
boundBox <- qbbox(lat=villGood$Latitude, lon=villGood$Longitude, TYPE="all", margin=list(m=rep(0,4), TYPE = c("perc", "abs")[1]))

# get map of that area
deltaMap <- GetMap.bbox(lonR=boundBox$lonR + c(.3, -.5), latR=boundBox$latR + c(.288, -.1), destfile="maps/delta.png")