################################################################################
# Function: cdf.nresp
# Programmer: Tom Kincaid
# Date: May 2, 2002
# Last Revised: February 6, 2004
#'
#' Internal Function: Count Number of Responses Less Than a Set of Values
#'
#' This function calculates the number of response values less than or equal to
#' each of the set of values at which the cumulative distribution function (CDF)
#' is estimated.
#'
#' @param z Vector of the response values.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @return Output is the number of response values for each CDF estimation
#'   value.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

cdf.nresp <- function(z, val) {

# Calculate the number of response values for each CDF estimation value

   m <- length(val)
   nresp <- numeric(m)
   for(i in 1:m) {
      nresp[i] <- sum(ifelse(z <= val[i], 1, 0))
   }

# Return the number of response values for each CDF estimation value

   nresp
}
