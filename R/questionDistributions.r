## @knitr riceDist
rice <- build.dist(data=pak, group="Province", question="RiceLost")
rice$Size <- "All"

## @knitr waterDist
water <- build.dist(data=pak, group="Province", question="StagnantWater")
water$Size <- "All"

## @knitr accommodationDist
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
rice3 <- build.dist(data=pak3, group="Province", question="RiceLost", full.answers=unique(pak$RiceLost))
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
accommodation3 <- build.dist(data=pak3, group="Province", question="Accommodation")
accommodation3$Size <- "3"
accommodation4 <- build.dist(data=pak4, group="Province", question="Accommodation")
accommodation4$Size <- "4"
accommodation5 <- build.dist(data=pak5, group="Province", question="Accommodation")
accommodation5$Size <- "5"
accommodation10 <- build.dist(data=pak10, group="Province", question="Accommodation")
accommodation10$Size <- "10"
accommodation15 <- build.dist(data=pak15, group="Province", question="Accommodation")
accommodation15$Size <- "15"
accommodation20 <- build.dist(data=pak20, group="Province", question="Accommodation")
accommodation20$Size <- "20"

## @knitr plotAllRice
riceAll <- rbind(rice, rice3, rice4, rice5, rice10, rice15, rice20)
riceAll$Size <- factor(x=riceAll$Size, levels=c("3", "4", "5", "10", "15", "20", "All"))
ggplot(riceAll, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Rice Lost")

## @knitr plotAllWater
waterAll <- rbind(water, water3, water4, water5, water10, water15, water20)
waterAll$Size <- factor(x=waterAll$Size, levels=c("3", "4", "5", "10", "15", "20", "All"))
ggplot(waterAll, aes(x=StagnantWater, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Stagnant Water")

## @knitr plotAllAccommodation
accommodationAll <- rbind(accommodation, accommodation3, accommodation4, accommodation5, accommodation10, accommodation15, accommodation20)
accommodationAll$Size <- factor(x=accommodationAll$Size, levels=c("3", "4", "5", "10", "15", "20", "All"))
ggplot(accommodationAll, aes(x=Accommodation, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Accommodation")

## @knitr compareRice
head(rice)
head(riceAll)
tail(riceAll)
riceCast <- dcast(data=riceAll, formula=Province + RiceLost ~ Size, value.var="Percent")
head(riceCast, 10)
View(rice3[rice3$Size == "3", ])

calculate.error <- function(data, total.col, cols, loss=function(true, sampled){})
{
    
}
