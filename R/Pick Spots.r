### make random subset of data for now
### later this will be areas identified as purposive

# load good village data
load("C:/users/Jared/week2/data/villGood.rdata")

# load household data
houses <- read.csv(file="C:/users/Jared/week2/data/hhpr1.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# for now take 30 villages at random
set.seed(1028)
rands <- sample(nrow(villGood), size=30, replace=FALSE)
villPurpose <- villGood[rands,]

houses <- houses[houses$VillageName %in% villPurpose$VillageName, ]
dim(houses)