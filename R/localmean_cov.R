###############################################################################
# Function: localmean_cov (exported)
# Programmer: Tom Kincaid
# Date: March 10, 2020
#
#' Internal Function: Variance-Covariance Matrix Based on Local Mean Estimator
#'
#' This function calculates the variance-covariance matrix using the local mean
#' estimator.
#'
#' @param zmat Matrix of weighted response values or weighted residual values
#'   for the sample points.
#'
#' @param weight_1st List from the local mean weight function containing two
#'   elements: a matrix named \code{ij} composed of the index values of neighboring
#'   points and a vector named \code{gwt} composed of weights.
#'
#' @return The local mean estimator of the variance-covariance matrix.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @export
###############################################################################

localmean_cov <- function(zmat, weight_1st) {
  # Generalizes localmean_var() to the m columns of zmat jointly: cell
  # (k, l) of the returned matrix is the same neighborhood-contrast formula
  # as localmean_var(), but with column k's neighborhood-mean-centered
  # values paired against column l's rather than a column against itself,
  # giving Cov(total_k, total_l) for k != l and Var(total_k) on the
  # diagonal (k == l). Used where variables/categories/surveys must be
  # treated jointly (e.g. cdftestvar_prop(), changevar_mean()) so that
  # downstream code can combine variances via a covariance matrix (e.g. the
  # delta method) rather than assuming independence between columns.

  # Calculate additional required values

  temp <- dim(zmat)
  m <- temp[2]

  # Initialize the results matrix

  lmvar <- array(0, c(m, m))

  # Save each column's local means
  local_means <- lapply(seq_len(m), function(k) {
    z <- zmat[, k]
    localmean_site_sums(
      z[weight_1st$ij[, 2]] * weight_1st$gwt,
      weight_1st$ij[, 1], nrow(zmat)
    )
  })

  # Begin loops for variance/covariance calculations
  # (only the upper triangle, k <= l, is computed; the matrix is symmetric
  # so the lower triangle is copied from it below)

  for (k in 1:m) {
    for (l in k:m) {
      z1 <- zmat[, k]
      z2 <- zmat[, l]
      zb1 <- local_means[[k]]
      zb2 <- local_means[[l]]

      # Calculate the variance or covariance estimate

      lmvar[k, l] <- sum(
        weight_1st$gwt *
          (z1[weight_1st$ij[, 2]] - zb1[weight_1st$ij[, 1]]) *
          (z2[weight_1st$ij[, 2]] - zb2[weight_1st$ij[, 1]])
      )
    }

    # Assign estimates that already have been calculated
    # (mirror the upper triangle into the lower triangle)

    if (k > 1) {
      lmvar[k, 1:(k - 1)] <- lmvar[1:(k - 1), k]
    }
  }

  # Return the variance/covariance estimate

  lmvar
}
