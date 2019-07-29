################################################################################
# File: mdmlin.r
# Programmer: Tony Olsen
# Date: April 3, 2003
#
#' Internal Function: GRTS Multipliers for Multi-Density Categories for Linear Network
#'
#' @param len  Vector of segment lengths for each segment in sample frame.
#'
#' @param mdcaty Vector of multi-density category groups for each segment in
#'   sample frame.
#'
#' @param n.desired Expected sample size for each category.  Row names must
#'   match category names in mdcaty.
#'
#' @return Numeric vector of multipliers that is same length as len and mdcaty.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

mdmlin <- function(len, mdcaty, n.desired) {

   catsum <- tapply(len,mdcaty,sum,na.rm=TRUE)
   catmatch <- match(names(n.desired),names(catsum),nomatch=0)
   piden <- n.desired/catsum[catmatch]
   mdmlin <- rep(NA,length(mdcaty))
   for(i in names(n.desired))
      mdmlin[mdcaty == i] <- piden[i]

   mdmlin
}


