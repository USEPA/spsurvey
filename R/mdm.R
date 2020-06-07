################################################################################
# File: mdm.r
# Programmer: Tony Olsen, Jason Law
# Date: June 6, 2020
#
#' Internal Function: GRTS Multipliers for Multi-Density Categories
#' @param mdcaty Vector of multi-density category groups for each segment in
#'   sample frame.
#'
#' @param n.desired Expected sample size for each category.  Row names must
#'   match category names in mdcaty.
#'   
#' @param size  Vector of size measures for each feature in sample frame. Default
#' is `NULL` for features with identical size measures (e.g., point features).
#'
#' @return Numeric vector of multipliers that is same length as size and mdcaty.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

mdm <- function(mdcaty, n.desired, size = NULL){
  if(!is.factor(mdcaty)){
    mdcaty <- as.factor(mdcaty)
  }
  nms       <- levels(mdcaty)
  n.desired <- n.desired[nms]
  if(is.null(size)){
    gsum <- tabulate(mdcaty)
  } else {
    gsum <- vapply(split(size, mdcaty), sum, na.rm = T, FUN.VALUE = numeric(1))
  }
  return(unname(n.desired / gsum)[mdcaty])
}
