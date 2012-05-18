## get week 2 data
## used to get data from the QlikView generated CSV into an rdata file.
require(useful)
require(stringr)
pak <- read.table("C:/Users/Jared/week2/data/pakistan/pak week 2 revised.csv", header=TRUE, sep=",")

# get rid of tehsils that are not in list or blank
pak <- pak[pak$Tehsil != "Not in list", ]
pak <- pak[which(pak$Tehsil != ""), ]

# isolate fields that involve lost (not acre)
lostCols <- names(pak)[grep(pattern="Lost$", x=names(pak))]

for(a in lostCols)
{
    pak[pak[, a] == "", a] <- NA
}

save(pak, file="C:/Users/Jared/week2/data/pakistan/pak.rdata")

# histogram for rice lost
#ggplot(pak, aes(x=RiceLost)) + geom_histogram() + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))