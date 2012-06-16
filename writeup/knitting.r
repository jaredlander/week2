#' do.knit
#' 
#' Wrapper for knitr::knit
#' 
#' This function is merely a vectorized wrapper for knitr::knit.  It knits Rnw files into tex files.
#' 
#' @import knitr::knit
#' @aliases knitVector
#' @export knitVector
#' @author Jared P. Lander
#' @param input Character vector of filenames (with path) to be knited.
#' @param output Character vector of filenames (with path) for the output TeX.
#' @param tangle Whether to tangle the R code from the input file (like Stangle)
#' @param text A character vector as an alternative way to provide the input file
#' @return The compiled documents are written into the output files, and the paths of the output files are returned.
#' @examples
#' # none here
#' 
knitVector <- Vectorize(FUN=knit, vectorize.args=c("input", "output", "tangle", "text")) 
# <- function(file.name)
# {
#     knit(input=rnw, output=tex)
# }


# if I Vectorize doKnit I can get rid of the loop, but I think that's overkill
#' do.knit
#' 
#' Wrapper for knitr::knit
#' 
#' This function is merely an advanced wrapper wrapper for knitr::knit.  It knits the Rnw files into tex only if the Rnw files are newer than the tex files.  Like LaTex, file names should not be provided with extensions as it will added them automatically.
#' 
#' @import knitr::knit
#' @aliases do.knit
#' @export do.knit
#' @author Jared P. Lander
#' @param files Name of files to be knitted, without extensions.
#' @return The compiled documents are written into the output files, and the path of the output files are returned.
#' @examples
#' # none here
do.knit <- function(files)
{
    # create full names of files
    rnw <- sprintf("%s.Rnw", files)
    tex <- sprintf("%s.tex", files)
    
    # get vector of files that need updating based on. . .
    # if the rnw files exists and either the tex file doesn't or the tex file is old
    toRun <- ifelse(file.exists(rnw) & (!file.exists(tex) | file.info(tex)$mtime < file.info(rnw)$mtime), TRUE, FALSE)
    
    knitVector(input=rnw, output=tex)
    ## loop through and knit each chapter file if the tex file is older
#     for(a in files[toRun])
#     {
#         doKnit(file.name=a)
#     }
}