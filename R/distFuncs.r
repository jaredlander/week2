# Distribution functions
require(useful)
## builds the distribution for a given question
build.dist <- function(data, lhs, group, question)
{
    theFormula <- build.formula(lhs=lhs, rhs=c(group, question))
    agg <- aggregate(theFormula, data, length)
    agg <- ddply(agg, .variables=group, .fun=function(x){ x$Percent <- x[[lhs]] / sum(x[[lhs]]); return(x) })
    agg
}


## get random tehsils from a province
village.list <- function(x, num=5, unit="Tehsil")
{
    # get list of units
    units <- unique(x[, unit])
    
    # sample num of those without replacement
    keepers <- sample(x=units, size=min(num, length(units)), replace=FALSE)
    
    return(as.character(keepers))
}