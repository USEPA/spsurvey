################################################################################
# File: mdmarea.r
# Programmer: Tom Kincaid
# Date: October 12, 2004
#
#' Internal Function: GRTS Multipliers for Multi-Density Categories for Areas
#'
#' GRTS Multiplieers for Multi-density Categories for areas
#'
#' @param area Vector of polygon areas for each polygon in the sample frame.
#'
#' @param mdcaty Vector of multi-density category names for each polygon in the
#'   sample frame.
#'
#' @param n.desired Expected sample size for each category.  Row names must
#'   match category names in mdcaty.
#'
#' @return Numeric vector of multipliers that is same length as area and mdcaty.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

mdmarea <- function(area, mdcaty, n.desired) {

   catsum <- tapply(area, mdcaty, sum, na.rm=TRUE)
   catmatch <- match(names(n.desired),names(catsum),nomatch=0)
   piden <- n.desired/catsum[catmatch]
   mdmarea <- rep(NA, length(mdcaty))
   for(i in names(n.desired))
      mdmarea[mdcaty == i] <- piden[i]

   mdmarea
}


