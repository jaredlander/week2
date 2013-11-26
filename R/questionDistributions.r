## @knitr riceDist
rice <- build.dist(data=pak, group="Province", question="RiceLost")
rice$Size <- "All"

## @knitr waterDist
water <- build.dist(data=pak, group="Province", question="StagnantWater")
water$Size <- "All"

## @knitr accomodationDist
accommodation <- build.dist(data=pak, group="Province", question="Accommodation")
accommodation$Size <- "All"

## @knitr ricePlot
ggplot(rice, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Rice Lost")

## @knitr waterPlot
ggplot(water, aes(x=StagnantWater, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Stagnant Water")

## @knitr accommodationPlot
ggplot(accommodation, aes(x=Accommodation, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Accommodation")

## @knitr setSeedForSampling
set.seed(26734761)

## @knitr sampleSizes
sampleSizes <- c(3, 4, 5, 10, 15, 20)

## @knitr elegantSampling
alply(sampleSizes, .margins=1, function(x){unlist(dlply(pak, .variables="Province", .fun=village.list, num=x, unit="Tehsil"))})

## @knitr buildSampledData
# Choose samples of 3, 4, 5, 10, 15 and 20 tehsils in each province
pak3 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=3, unit="Tehsil")), ]
pak4 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=4, unit="Tehsil")), ]
pak5 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=5, unit="Tehsil")), ]
pak10 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=10, unit="Tehsil")), ]
pak15 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=15, unit="Tehsil")), ]
pak20 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=20, unit="Tehsil")), ]


## @knitr sampledRiceDist
rice3 <- build.dist(data=pak3, group="Province", question="RiceLost")
rice3$Size <- "3"
rice4 <- build.dist(data=pak4, group="Province", question="RiceLost")
rice4$Size <- "4"
rice5 <- build.dist(data=pak5, group="Province", question="RiceLost")
rice5$Size <- "5"
rice10 <- build.dist(data=pak10, group="Province", question="RiceLost")
rice10$Size <- "10"
rice15 <- build.dist(data=pak15, group="Province", question="RiceLost")
rice15$Size <- "15"
rice20 <- build.dist(data=pak20, group="Province", question="RiceLost")
rice20$Size <- "20"

## @knitr sampledWaterDist
water3 <- build.dist(data=pak3, group="Province", question="StagnantWater")
water3$Size <- "3"
water4 <- build.dist(data=pak4, group="Province", question="StagnantWater")
water4$Size <- "4"
water5 <- build.dist(data=pak5, group="Province", question="StagnantWater")
water5$Size <- "5"
water10 <- build.dist(data=pak10, group="Province", question="StagnantWater")
water10$Size <- "10"
water15 <- build.dist(data=pak15, group="Province", question="StagnantWater")
water15$Size <- "15"
water20 <- build.dist(data=pak20, group="Province", question="StagnantWater")
water20$Size <- "20"

## @knitr sampledAccommodationDist
accomodation3 <- build.dist(data=pak3, group="Province", question="Accommodation")
accomodation3$Size <- "3"
accomodation4 <- build.dist(data=pak4, group="Province", question="Accommodation")
accomodation4$Size <- "4"
accomodation5 <- build.dist(data=pak5, group="Province", question="Accommodation")
accomodation5$Size <- "5"
accomodation10 <- build.dist(data=pak10, group="Province", question="Accommodation")
accomodation10$Size <- "10"
accomodation15 <- build.dist(data=pak15, group="Province", question="Accommodation")
accomodation15$Size <- "15"
accomodation20 <- build.dist(data=pak20, group="Province", question="Accommodation")
accomodationDist20$Size <- "20"
