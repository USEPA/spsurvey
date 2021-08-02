################################################################################
# Function: interp_cdf (not exported)
# Programmer Tom Kincaid
# Date: May 5, 2021
#
#' Interpolate CDF Values at a Set of Percentiles
#'
#' This function interpolates CDF values at a set of percentiles.  The CDF
#' values can be CDF estimates, CDF confidence bound estimates, or values at
#' which the CDF is estimated (i.e., x-axis values).  It is assumed that
#' arguments cdfest_p and cdf_value are strictly increasing.
#'
#' @param pctval Vector of percentiles (expressed as percents) at which the
#'   CDF values are to be interpolated.
#'
#' @param cdfest_p Vector of CDF estimates in terms of proportions.
#'
#' @param cdf_value Vector of CDF values to be interpolated.
#'
#' @return A numeric vector consisting of the interpolated CDF values.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @noRd
################################################################################

interp_cdf <- function(pctval, cdfest_p, cdf_value) {
  nvec <- 1:length(cdfest_p)
  rslt <- numeric(0)
  for (j in 1:length(pctval)) {
    high <- ifelse(length(nvec[cdfest_p >= pctval[j]]) > 0,
      min(nvec[cdfest_p >= pctval[j]]), NA
    )
    low <- ifelse(length(nvec[cdfest_p <= pctval[j]]) > 0,
      max(nvec[cdfest_p <= pctval[j]]), NA
    )
    if (is.na(high)) {
      rslt[j] <- NA
    } else if (is.na(low)) {
      rslt[j] <- cdf_value[high]
    } else {
      if (high > low) {
        pdis <- (pctval[j] - cdfest_p[low]) / (cdfest_p[high] - cdfest_p[low])
        rslt[j] <- cdf_value[low] + pdis * (cdf_value[high] - cdf_value[low])
      } else {
        rslt[j] <- cdf_value[high]
      }
    }
  }

  return(rslt)
}
