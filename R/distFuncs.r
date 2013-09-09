## @knitr needPackages
require(useful)
require(plyr)

## @knitr distFunc
# Distribution functions

#' @title build.dist
#' @description builds the distribution for a given question
#' @author Jared P. Lander
#' @param data data.frame containing information of interest
#' @param group Variable by which to break up the data
#' @param question Question to calculate on
#' @param na.rm If TRUE NA values of question will be removed before calculation
#' @return Data.frame enumerating the group, the question, the count for each and the percent
#' @import plyr
#' @examples
#' load("data/pakistan/pak.rdata")
#' rice1 <- build.dist(pak, group="Province", question="RiceLost", na.rm=TRUE)
#' rice2 <- build.dist(pak, group="Province", question="RiceLost", na.rm=FALSE)
build.dist <- function(data, group, question, na.rm=FALSE)
{    
    # first remove NAs if requested
    if(na.rm)
    {
        data <- data[!is.na(data[, question]), ]
    }
    
    # get row counts for each combination of group and question
    agg <- ddply(data, .variables=c(group, question), NROW)
    # give the returned column a good name
    agg <- rename(agg, c(V1="Count"))
    # create a new column to hold percents
    agg$Percent <- NA
    
    # calculate the percents
    # count divided by total count for that group
    agg <- ddply(agg, group, function(x){ x$Percent <-  x$Count / sum(x$Count); return(x)} )
    agg
}


## get random tehsils from a province
#' @title village.list
#' @description
village.list <- function(x, num=5, unit="Tehsil")
{
    # get list of units
    units <- unique(x[, unit])
    
    # sample num of those without replacement
    keepers <- sample(x=units, size=min(num, length(units)), replace=FALSE)
    
    return(as.character(keepers))
}


# function to make names of dist's better
change.names <- function(names, include=names, prefix="")
{
    theOnes <- which(!names %in% include)
    names[theOnes] <- sprintf("%s.%s", prefix, names[theOnes])
    return(names)
}

## function to impute missing
impute.col <- function(col, value=0)
{
    col[is.na(col)] <- value
    return(col)
}

## this compares two distributions and computes an MSE
compare.dist <- function(full, partial, compare="Percent", by=intersect(names(full), names(partial)))
{
    # prepend Pull onto certain names in full
    names(full) <- change.names(names=names(full), include=by, prefix="Full")
    
    # prepend Partial onto certain names in full
    names(partial) <- change.names(names=names(partial), include=by, prefix="Partial")
    
    full.compare <- sprintf("Full.%s", compare)
    partial.compare <- sprintf("Partial.%s", compare)
    
    # join the two together
    both <- join(x=full, y=partial, by=by, type="left")
    
    rm(full, partial)
    
    ## fill in any NA's with zero
    both[[full.compare]] <- impute.col(col=both[[full.compare]], value=0)
    both[[partial.compare]] <- impute.col(col=both[[partial.compare]], value=0)
    
    both$.Diff <- both[[full.compare]] - both[[partial.compare]]
    
    both$.MSE <- mean(both$.Diff^2)
    
    #attr(x=both, which="MSE") <- mean(both$.Diff^2)
    
    #aggregate(build.formula(lhs=".Diff", rhs=
    
    return(both)
}

