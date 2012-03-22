### make random subset of data for now
### later this will be areas identified as purposive

require(useful)

# load good village data
# object is villGood
load("C:/users/Jared/week2/data/villGood.rdata")

# load household data
houses <- read.csv(file="C:/users/Jared/week2/data/hhpr1.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# for now take 30 villages at random
set.seed(1028)
rands <- sample(nrow(villGood), size=30, replace=FALSE)

housesPurpose <- houses[houses$VillageName %in% villGood$VillageName[rands], ]
View(houses[houses$VillageName==7216,])
unique(houses$VillageName)
View(houses)
names(houses)

ddply(.data=houses, .variables="VillageName", .fun=function(x) { sum(x$Long != "") } )