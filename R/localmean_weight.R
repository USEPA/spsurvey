################################################################################
# Function: localmean_weight (not exported)
# Programmers: Don Stevens and Tom Kincaid
# Date: February 6, 2020
# Revised: April 28, 2021 to use the modified object returned by the
#          localmean_weight2 function and to return a NULL object when the ginv
#          function fails to return valid output
# Revised: April 29, 2021 to eliminate use of the while loop to achieve valid
#          output from the ginv function, which means that the vincr argument
#          and the localmean_weight2 function are no longer needed
#
#' Internal Function: Local Mean Variance Neighbors and Weights
#'
#' This function calculates the index values of neighboring points and
#' associated weights required by the local mean variance estimator.
#'
#' @param x Vector of x-coordinates for location of the sample points.
#'
#' @param y Vector of y-coordinates for location of the sample points.
#'
#' @param prb Vector of inclusion probabilities for the sample points (pre calibration, if relevant).
#'
#' @param nbh Number of neighboring points to use in the calculations (including the site of interest).
#'
#' @return If the generalized inverse fails to return valid output, a NULL object.  Otherwise, a
#'   list containing two elements: a matrix named \code{ij} composed of the
#'   index values of neighboring points and a vector named \code{gwt}
#'   composed of weights.
#'
#' @author  Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @export
################################################################################

localmean_weight <- function(x, y, prb, nbh = 15) {
  if (!is.numeric(nbh) || length(nbh) != 1L || !is.finite(nbh) ||
      nbh < 2 || nbh != floor(nbh)) {
    stop("nbh must be one finite integer of at least 2.", call. = FALSE)
  }
  n <- length(x)
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(prb) ||
      length(y) != n || length(prb) != n || any(!is.finite(x)) ||
      any(!is.finite(y))) {
    stop("Coordinates and inverse analysis weights must be aligned numeric vectors with finite coordinates.", call. = FALSE)
  }
  if (n < 2L || any(!is.finite(prb)) || any(prb == 0)) return(NULL)
  tryCatch(localmean_solve(x, y, prb, min(nbh, n)),
    error = function(e) NULL)
}
