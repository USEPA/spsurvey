################################################################################
# Function: interp.cdf
# Programmers: Tony Olsen
#              Tom Kincaid
# Date: March 26, 2007
# Last Revised: April 26, 2007
#
#' Interpolate CDF Values at a Set of Percentiles
#'
#' This function interpolates CDF values at a set of percentiles.  The CDF
#' values can be CDF estimates, CDF confidence bound estimates, or values at
#' which the CDF is estimated (i.e., x-axis values).  It is assumed that
#' arguments cdfest.p and cdf.value are strictly increasing.
#'
#' @param pctval Vector of percentiles (expressed as percents) at which the
#'   CDF values are to be interpolated.
#'
#' @param cdfest.p Vector of CDF estimates in terms of proportions.
#'
#' @param cdf.value Vector of CDF values to be interpolated.
#'
#' @return A numeric vector consisting of the interpolated CDF values.
#'
#' @author
#'   Tony Olsen \email{Olsen.Tony@epa.gov}\cr
#'   Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

interp.cdf <- function(pctval, cdfest.p, cdf.value) {

nvec <- 1:length(cdfest.p)
rslt <- numeric(0)
for (j in 1:length(pctval)) {
   high <- ifelse(length(nvec[cdfest.p >= pctval[j]]) > 0,
                  min(nvec[cdfest.p >= pctval[j]]), NA)
   low <- ifelse(length(nvec[cdfest.p <= pctval[j]]) >  0,
                  max(nvec[cdfest.p <= pctval[j]]), NA)
   if(is.na(high)) {
      rslt[j] <- NA
   } else if(is.na(low)) {
     rslt[j] <- cdf.value[high]
   } else {
      if( high > low) {
    	    pdis <- (pctval[j] - cdfest.p[low])/(cdfest.p[high] - cdfest.p[low])
    	    rslt[j] <- cdf.value[low] + pdis * (cdf.value[high] - cdf.value[low])
      } else {
         rslt[j] <- cdf.value[high]
      }
   }
}

return(rslt)
}

