################################################################################
# Function: interp_axis (not exported)
# Programmer Tom Kincaid
# Date: May 5, 2021
#
#' Create Right-Side y-Axis Lables for a CDF Plot
#'
#' This function creates right side y-axis labels for a CDF plot.  It assumes
#' that arguments cdfest_l and cdfest_r are strictly increasing.  If argument
#' yl_lab is less than the first cdfest_l vlaue, then the funciton assumes 0 for
#' both cdfest_l and cdfest_r.
#'
#' @param yl_lab Vector of left side y-axis labels, which are the basis for
#'   interpolating cdfest_r values.
#'
#' @param cdfest_l Vector of CDF estimates corresponding to the left side
#'   y-axis.
#'
#' @param cdfest_r Vector of CDF estimates corresponding to the right side
#'   y-axis.
#'
#' @return A numeric vector consisting of the right side y-axis labels.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @noRd
################################################################################

interp_axis <- function(yl_lab, cdfest_l, cdfest_r) {

nvec <- 1:length(cdfest_l)
rslt <- numeric(0)
for (j in 1:length(yl_lab)) {
   high <- ifelse(length(nvec[cdfest_l >= yl_lab[j]]) > 0,
                  min(nvec[cdfest_l >= yl_lab[j]]), nvec[length(nvec)])
   low <- ifelse(length(nvec[cdfest_l <= yl_lab[j]]) >  0,
                 max(nvec[cdfest_l <= yl_lab[j]]), NA)
   if(is.na(low)) {
      ulow <- 0
      plow <- 0
      pdis <- (yl_lab[j] - plow)/(cdfest_l[high] - plow)
    	 rslt[j] <- ulow + pdis * (cdfest_r[high] - ulow)
   } else {
      if( high > low) {
    	    pdis <- (yl_lab[j] - cdfest_l[low])/(cdfest_l[high] - cdfest_l[low])
    	    rslt[j] <- cdfest_r[low] + pdis * (cdfest_r[high] - cdfest_r[low])
      } else {
         rslt[j] <- cdfest_r[high]
      }
   }
}

return(rslt)
}
