## @knitr riceDist
riceAll <- build.dist(data=pak, group="Province", question="RiceLost", label="All")

## @knitr waterDist
waterAll <- build.dist(data=pak, group="Province", question="StagnantWater", label="All")

## @knitr accommodationDist
accommodationAll <- build.dist(data=pak, group="Province", question="Accommodation", label="All")

## @knitr ricePlot
ggplot(riceAll, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Rice Lost")

## @knitr waterPlot
ggplot(waterAll, aes(x=StagnantWater, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Stagnant Water")

## @knitr accommodationPlot
ggplot(accommodationAll, aes(x=Accommodation, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Accommodation")

## @knitr setSeedForSampling
set.seed(26734761)

## @knitr sampleSizes
sampleSizes <- c(3, 4, 5, 10, 15, 20)
sampleNames <- c(sampleSizes, "All")

## @knitr elegantSampling
alply(sampleSizes, .margins=1, function(x){unlist(dlply(pak, .variables="Province", .fun=village.list, num=x, unit="Tehsil"))})

## @knitr buildSampledData
# Choose samples of sampleSizes tehsils in each province
for(a in sampleSizes)
{
    assign(x=sprintf("pak%s", a), value=cut.down.data(data=pak, num=a, unit="Tehsil", group="Province"))
}

## @knitr sampledRiceDist
for(a in sampleSizes)
{
    assign(x=sprintf("rice%s", a), value=build.dist(data=eval(parse(text=sprintf("pak%s", a))), group="Province", question="RiceLost", full.answers=unique(pak$RiceLost), label=a))
}

## @knitr sampledWaterDist
for(a in sampleSizes)
{
    assign(x=sprintf("water%s", a), value=build.dist(data=eval(parse(text=sprintf("pak%s", a))), group="Province", question="StagnantWater", full.answers=unique(pak$StagnantWater), label=a))
}

## @knitr sampledAccommodationDist
for(a in sampleSizes)
{
    assign(x=sprintf("accommodation%s", a), value=build.dist(data=eval(parse(text=sprintf("pak%s", a))), group="Province", question="Accommodation", full.answers=unique(pak$Accommodation), label=a))
}

## @knitr plotAllRice
riceList <- vector("list", length(sampleNames))
names(riceList) <- sampleNames
for(a in sampleNames)
{
    riceList[[a]] <- eval(parse(text=sprintf("rice%s", a)))
}
riceTotal <- Reduce(rbind, riceList)
riceTotal$Size <- factor(x=riceTotal$Size, levels=sampleNames)
ggplot(riceTotal, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Rice Lost")

## @knitr plotAllWater
waterList <- vector("list", length(sampleNames))
names(waterList) <- sampleNames
for(a in sampleNames)
{
    waterList[[a]] <- eval(parse(text=sprintf("water%s", a)))
}
waterTotal <- Reduce(rbind, waterList)
waterTotal$Size <- factor(x=waterTotal$Size, levels=sampleNames)
ggplot(waterTotal, aes(x=StagnantWater, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Stagnant Water")


## @knitr plotAllAccommodation
accommodationList <- vector("list", length(sampleNames))
names(accommodationList) <- sampleNames
for(a in sampleNames)
{
    accommodationList[[a]] <- eval(parse(text=sprintf("accommodation%s", a)))
}
accommodationTotal <- Reduce(rbind, accommodationList)
accommodationTotal$Size <- factor(x=accommodationTotal$Size, levels=sampleNames)
ggplot(accommodationTotal, aes(x=Accommodation, y=Percent)) + geom_bar(stat="identity", aes(color=Size, fill=Size), position=position_dodge()) + facet_wrap(~Province) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + labs(x="Accommodation")

## @knitr compareRice
riceMSE <- compute.mse(riceTotal, index.col=c("Province", "RiceLost"), size.col="Size", value.var="Percent", full.sample="All", sample.sizes=sampleSizes)
riceMSE

## @knitr compareWater
waterMSE <- compute.mse(waterTotal, index.col=c("Province", "StagnantWater"), size.col="Size", value.var="Percent", full.sample="All", sample.sizes=sampleSizes)
waterMSE

## @knitr compareAccommodation
accommodationMSE <- compute.mse(accommodationTotal, index.col=c("Province", "Accommodation"), size.col="Size", value.var="Percent", full.sample="All", sample.sizes=sampleSizes)
accommodationMSE

## @knitr plotMSERice
ggplot(riceMSE, aes(x=Size, y=MSE)) + geom_line(aes(group=1)) + geom_point() + ggtitle("Rice")

## @knitr plotMSEWater
ggplot(waterMSE, aes(x=Size, y=MSE)) + geom_line(aes(group=1)) + geom_point() + ggtitle("Water")

## @knitr plotMSEAccommodation
ggplot(accommodationMSE, aes(x=Size, y=MSE)) + geom_line(aes(group=1)) + geom_point() + ggtitle("Accommodation")

## @knitr plotMSERiceWaterAccommodation
allMSE <- rbind(cbind(riceMSE, Question="Rice Lost"), cbind(waterMSE, Question="Stagnant Water"), cbind(accommodationMSE, Question="Accommodation"))
allMSE
ggplot(allMSE, aes(x=Size, y=MSE, color=Question, group=Question)) + geom_line() + geom_point()
