# full sample histograms
require(useful)

load("C:/Users/Jared/week2/data/pakistan/pak.rdata")

# isolate fields that involve lost (not acre)
lostCols <- names(pak)[grep(pattern="Lost$", x=names(pak))]

# do this as percent please
pdf(file="C:/Users/Jared/week2/graphics/Pakistan All.pdf")
for(a in lostCols)
{
    name <- sub(pattern="Lost", replacement=" % Lost", x=a)
    print(ggplot(pak, aes_string(x=a)) + geom_histogram() + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90)) + xlab(name) + opts(title="All Villages"))
}
dev.off()


riceAgg <- aggregate(New_ID ~ Province + RiceLost, pak, length)
ricePerc <- ddply(riceAgg, .variables="Province", .fun=function(x){ x$Percent <- x$New_ID / sum(x$New_ID); return(x) })

riceAgg[riceAgg$Province == "KPK", ]

ggplot(pak, aes(x=RiceLost, y=..count..)) + geom_bar() + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))
ggplot(pak, aes(x=RiceLost, y=(..count..)/sum(..count..))) + geom_bar()
ggplot(pak, aes(x=RiceLost, y=(..count..)/sum(..count..))) + geom_bar() + scale_y_continuous(formatter="percent") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))
?geom_histogram