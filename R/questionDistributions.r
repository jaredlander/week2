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

## @knitr buildSampledData
# Choose samples of 3, 4, 5, 10, 15 and 20 tehsils in each province
pak3 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=3, unit="Tehsil")), ]
pak4 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=4, unit="Tehsil")), ]
pak5 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=5, unit="Tehsil")), ]
pak10 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=10, unit="Tehsil")), ]
pak15 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=15, unit="Tehsil")), ]
pak20 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=20, unit="Tehsil")), ]
