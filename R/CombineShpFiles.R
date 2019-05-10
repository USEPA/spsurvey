################################################################################
# File: CombineShpFiles.r
# Purpose: Combine .shp files in current working directory
# Programmer: Marc Weber
# Date: May 9, 2019
#'
#' 
#'
#' @param newShp  New shapefile to be created
#'
#' @param ids vector of record IDs
#' 
#' @param numIDs number of record ID values
#'
#' @return 1 on success, -1 on error
#'
#' @author Marc Weber  \email{Weber.Marc@epa.gov}
#'
#' @export
#'
################################################################################
mdmpts <- function(mdcaty,n.desired) {

   gsum <- table(mdcaty)
   catmatch <- match(names(n.desired),names(gsum),nomatch=0)
   piden <- n.desired/gsum[catmatch]
   mdmpts <- rep(NA,length(mdcaty))
   for(i in names(n.desired))
       mdmpts[mdcaty == i] <- piden[i]

   mdmpts
}


