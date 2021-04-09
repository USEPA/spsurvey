###############################################################################
# Function: cdf_nresp (not exported)
# Programmer: Tom Kincaid
# Date: June 16, 2020
#'
#' Internal Function: Calculate Number of Response Values Less Than a Set of Values
#'
#' This function calculates the number of response values less than or equal to
#' each of the set of values at which the cumulative distribution function (CDF)
#' is estimated.
#'
#' @param z Vector of the response values.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @return Vector containing the number of response values for each CDF estimation
#'   value.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @noRd
###############################################################################

cdf_nresp <- function(z, val) {

  # Calculate the number of response values for each CDF estimation value

  z <- z[!is.na(z)]
  m <- length(val)
  nresp <- numeric(m)
  for (i in 1:m) {
    nresp[i] <- sum(ifelse(z <= val[i], 1, 0))
  }

  # Return the number of response values for each CDF estimation value

  nresp
}
