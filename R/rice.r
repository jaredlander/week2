require(useful)

load("C:/Users/Jared/week2/data/pakistan/pak.rdata")

build.dist <- function(data, lhs, group, question)
{
    theFormula <- build.formula(lhs=lhs, rhs=c(group, question))
    agg <- aggregate(theFormula, data, length)
    agg <- ddply(agg, .variables=group, .fun=function(x){ x$Percent <- x[[lhs]] / sum(x[[lhs]]); return(x) })
    agg
}

## get random tehsils from each province
village.list <- function(x, num=5, unit="Tehsil")
{
    # get list of units
    units <- unique(x[, unit])
    
    # sample num of those without replacement
    keepers <- sample(x=units, size=min(num, length(units)), replace=FALSE)
    
    return(as.character(keepers))
}

# isolate fields that involve lost (not acre)
lostCols <- names(pak)[grep(pattern="Lost$", x=names(pak))]

## overall distribution
ricePerc <- build.dist(data=pak, lhs="New_ID", group="Province", question="RiceLost")
ggplot(ricePerc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))

## get 5 Tehsils from each province
pak5Tehsil <- dlply(pak, .variables="Province", .fun=village.list, num=5, unit="Tehsil")
pak5 <- pak[pak$Tehsil %in% unlist(pak5Tehsil), ]
pak5$Tehsil <- factor(pak5$Tehsil)

# 5 village distribution
rice5Perc <- build.dist(data=pak5, lhs="New_ID", group="Province", question="RiceLost")

rice5Perc
ggplot(rice5Perc, aes(x=RiceLost, y=Percent)) + geom_bar(stat="identity") + facet_wrap(~Province) + opts(axis.text.x=theme_text(angle=90))

myJoin <- join(x=ricePerc, y=rice5Perc, by=c("Province", "RiceLost"))
View(myJoin)

# function to make names of dist's better
change.names <- function(names, include=names, prefix="")
{
    theOnes <- which(!names %in% include)
    names[theOnes] <- sprintf("%s.%s", prefix, names[theOnes])
    return(names)
}

compare.dist <- function(full, partial, by=intersect(names(full), names(partial)))
{
    # prepend Pull onto certain names in full
    names(full) <- change.names(names=names(full), include=by, prefix="Full")
    
    # prepend Partial onto certain names in full
    names(partial) <- change.names(names=names(partial), include=by, prefix="Partial")
    
    # join the two together
    both <- join(x=full, y=partial, by=by, type="left")
    
    
    
    return(both)
}
compare5 <- compare.dist(ricePerc, rice5Perc, by=c("Province", "RiceLost"))