################################################################################
# Function: localmean_var
# Programmers: Don Stevens and Tom Kincaid
# Date: October 17, 2000
# Last Revised: October 30, 2019
#
#' Internal Function: Local Mean Variance Estimator
#'
#' This function calculates the local mean variance estimator.
#'
#' @param z Vector of weighted response values or weighted residual values for
#'  the sample points.
#'
#' @param weight.lst List from the local mean weight function containing two
#'  elements: a matrix named ij composed of the index values of neighboring
#'  points and a vector named gwt composed of weights.
#'
#' @return The local mean estimator of the variance.
#'
#'@author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#'@keywords survey
#'
#'@export
################################################################################

localmean_var <- function(z, weight.lst) {

# Calculate local means

   zb <- sapply(split(z[weight.lst$ij[, 2]] * weight.lst$gwt, weight.lst$ij[, 1]), sum)

# Calculate the variance estimate

   lmvar <- sum(weight.lst$gwt * (z[weight.lst$ij[, 2]] - zb[weight.lst$ij[, 1]])^2)

# Return the variance estimate

   lmvar
}
