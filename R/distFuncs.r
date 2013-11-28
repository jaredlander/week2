## @knitr needPackages
require(useful)
require(plyr)
require(reshape2)

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
build.dist <- function(data, group, question, na.rm=FALSE, full.answers=unique(data[, question]), full.group=unique(data[, group]))
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
    
    ## build in a step that fills in missing levels
    # build a data.frame holding the possible answers
    allOpts <- expand.grid(full.group, full.answers)
    # make sure it has the appropriate name
    names(allOpts) <- c(group, question)
    # join the tables together, leaving NA's for the empties
    agg <- join(allOpts, agg, by=c(group, question), type="full")
    # replace NA's with 0
    agg$Count <- impute.col(col=agg$Count, value=0)
    agg
}


#' @title village.list
#' @description Randomly chooses units from a larger political unit
#' @author Jared P. Lander
#' @param x Data.frame holding names of villages and other political units
#' @param num Number of units to select in each political unit
#' @param unit The type of unit to be chosen
#' @return Vector of units
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


#' @title impute.col
#' @description Function to impute missing data
#' @author Jared P. Lander
#' @param col Variable of interest
#' @param value Value to impute missing data with
#' @return Vector with missing data imputed by value
impute.col <- function(col, value=0)
{
    col[is.na(col)] <- value
    return(col)
}


#' @title compare.dist
#' @description This compares two distributions and computes an MSE
#' @author Jared P. Lander
#' @param full data.frame of analysis for the full data
#' @param partial data.frame of analysis for the partial data
#' @param compare Unused
#' @param by Column name to join full and partial
compare.dist <- function(full, partial, compare="Percent", by=intersect(names(full), names(partial)))
{
    # prepend Full onto certain names in full
    names(full) <- change.names(names=names(full), include=by, prefix="Full")
    
    # prepend Partial onto certain names in partial
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

