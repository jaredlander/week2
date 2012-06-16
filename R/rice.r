require(useful)
source("R/distFuncs.r")

load("C:/Users/Jared/week2/data/pakistan/pak.rdata")
pak$RiceLost[pak$RiceLost == ""] <- NA
pak$RiceLost <- factor(pak$RiceLost)

# isolate fields that involve lost (not acre)
lostCols <- names(pak)[grep(pattern="Lost$", x=names(pak))]

## overall distribution
ricePerc <- build.dist(data=pak, lhs="New_ID", group="Province", question="RiceLost")
ricePerc$Size <- "All"
ggplot(ricePerc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90, hjust=1))


# compare 5 tehsils per province to the overall distribution
pak5 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=5, unit="Tehsil")), ]
pak5$Tehsil <- factor(pak5$Tehsil)
rice5Perc <- build.dist(data=pak5, lhs="New_ID", group="Province", question="RiceLost")
rice5Perc$Size <- "5"
compare5 <- compare.dist(ricePerc, rice5Perc, by=c("Province", "RiceLost"))
compare5$Partial.Size <- impute.col(col=compare5$Partial.Size, 5)
ggplot(rice5Perc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))
View(compare5)

# compare 10 tehsils per province to the overall distribution
pak10 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=10, unit="Tehsil")), ]
pak10$Tehsil <- factor(pak10$Tehsil)
rice10Perc <- build.dist(data=pak10, lhs="New_ID", group="Province", question="RiceLost")
rice10Perc$Size <- "10"
compare10 <- compare.dist(ricePerc, rice10Perc, by=c("Province", "RiceLost"))
compare10$Partial.Size <- impute.col(col=compare10$Partial.Size, 10)
ggplot(rice10Perc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))
View(compare10)

# compare 15 tehsils per province to the overall distribution
pak15 <- pak[pak$Tehsil %in% unlist(dlply(pak, .variables="Province", .fun=village.list, num=15, unit="Tehsil")), ]
pak15$Tehsil <- factor(pak15$Tehsil)
rice15Perc <- build.dist(data=pak15, lhs="New_ID", group="Province", question="RiceLost")
rice15Perc$Size <- "15"
compare15 <- compare.dist(ricePerc, rice15Perc, by=c("Province", "RiceLost"))
compare15$Partial.Size <- impute.col(col=compare15$Partial.Size, 15)
ggplot(rice15Perc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))
View(compare15)

# plot distributions for all measurement sizes on smae graph
allT <- rbind(rice5Perc, rice10Perc, rice15Perc, ricePerc)
allT$Size <- ordered(allT$Size, levels=c(5, 10, 15, "All"))
ggplot(allT, aes(x=RiceLost, y=Percent)) + geom_bar(aes(group=Size, fill=Size), stat="identity", position="dodge") + opts(axis.text.x=theme_text(angle=90)) + facet_wrap(~Province)

# plot differences
head(compare5)
allC <- rbind(compare5, compare10, compare15)
allC$Partial.Size <- ordered(allC$Partial.Size, levels=c(5, 10, 15))
head(allC)
## no to the bar
ggplot(allC, aes(x=RiceLost, y=.Diff)) + geom_bar(aes(fill=Partial.Size, colour=Partial.Size, group=Partial.Size), stat="identity", position="dodge") + opts(axis.text.x=theme_text(angle=90)) + facet_wrap(~Province)
ggplot(allC, aes(x=RiceLost, y=.Diff)) + geom_line(aes(fill=Partial.Size, colour=Partial.Size, group=Partial.Size)) + opts(axis.text.x=theme_text(angle=90)) + facet_wrap(~Province) + geom_hline(yintercept=0, colour="grey", linetype=2)
### We like this one below
ggplot(allC, aes(x=RiceLost, y=.Diff)) + geom_line(aes(fill=Province, colour=Province, group=Province)) + opts(axis.text.x=theme_text(angle=90)) + facet_wrap(~Partial.Size) + geom_hline(yintercept=0, colour="grey", linetype=2)

# plot MSE
ggplot(allC, aes(x=Partial.Size, y=.MSE)) + geom_line(aes(group=Province))

# number of villages in each province
vills <- aggregate(Village ~ Province + Tehsil, pak, function(x){length(unique(x))})
pros <- aggregate(Village ~ Province, vills, sum)
ggplot(pros, aes(x=Province)) + geom_bar(aes(y=Village))
ggplot(vills, aes(x=Tehsil)) + geom_point(aes(y=Village)) + opts(axis.text.x=theme_text(angle=90))
ggplot(vills, aes(x=Tehsil)) + geom_bar(aes(y=Village), stat="identity") + opts(axis.text.x=theme_text(angle=270, hjust=0)) + facet_wrap(~Province, scales="free_x")

vills5 <- aggregate(Village ~ Province + Tehsil, pak5, function(x){length(unique(x))})
ggplot(vills5, aes(x=Tehsil)) + geom_bar(aes(y=Village), stat="identity") + opts(axis.text.x=theme_text(angle=270, hjust=0)) + facet_wrap(~Province, scales="free_x")

vills10 <- aggregate(Village ~ Province + Tehsil, pak10, function(x){length(unique(x))})
ggplot(vills10, aes(x=Tehsil)) + geom_bar(aes(y=Village), stat="identity") + opts(axis.text.x=theme_text(angle=270, hjust=0)) + facet_wrap(~Province, scales="free_x")

vills15 <- aggregate(Village ~ Province + Tehsil, pak15, function(x){length(unique(x))})
ggplot(vills15, aes(x=Tehsil)) + geom_bar(aes(y=Village), stat="identity") + opts(axis.text.x=theme_text(angle=270, hjust=0)) + facet_wrap(~Province, scales="free_x")