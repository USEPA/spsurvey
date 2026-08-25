###############################################################################
# Function: localmean_var (not exported)
# Programmers: Don Stevens and Tom Kincaid
# Date: October 17, 2000
#
#' Internal Function: Local Mean Variance Estimator
#'
#' This function calculates the local mean variance estimator.
#'
#' @param z Vector of weighted response values or weighted residual values for
#'  the sample points.
#'
#' @param weight_1st List from the local mean weight function containing two
#'  elements: a matrix named \code{ij} composed of the index values of neighboring
#'  points and a vector named \code{gwt} composed of weights.
#'
#' @return The local mean estimator of the variance.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @export
###############################################################################

localmean_var <- function(z, weight_1st) {
  # This computes the local neighborhood variance estimator itself (Stevens
  # & Olsen 2003, formula for V_NBH): for each site i with neighborhood
  # D(s_i) and weights w_ij from localmean_weight(), the neighborhood mean
  # zbar_D(si) = sum over j in D(i) of w_ij * z_j is computed below (zb),
  # then the variance is the weighted sum, over every neighbor pair (i, j),
  # of w_ij * (z_j - zbar_D(si))^2, i.e. an average of several
  # contrasts between each neighborhood's members and that neighborhood's
  # own weighted mean, rather than a single global contrast to the overall
  # mean, which is what lets this estimator exploit spatial correlation among nearby
  # observations.

  # Calculate local means

  zb <- sapply(split(z[weight_1st$ij[, 2]] * weight_1st$gwt, weight_1st$ij[, 1]), sum)

  # Calculate the variance estimate

  lmvar <- sum(weight_1st$gwt * (z[weight_1st$ij[, 2]] - zb[weight_1st$ij[, 1]])^2)

  # Return the variance estimate

  lmvar
}
