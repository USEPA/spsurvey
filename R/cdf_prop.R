###############################################################################
# Function: cdf_prop (not exported)
# Programmer: Tom Kincaid
# Date: March 3, 2020
#
#' Estimate of Cumulative Distribution Function for a Proportion
#'
#' This function calculates an estimate of the cumulative distribution function
#' (CDF) for the proportion of a finite or an extensive resource.  The set of
#' values at which the CDF is estimated is supplied to the function.  The
#' Horvitz-Thompson ratio estimator, i.e., the ratio of two Horvitz-Thompson
#' estimators, is used to calculate the estimate.  The numerator of the ratio
#' estimates the total of the resource equal to or less than a specified value.
#' The denominator of the ratio estimates the size of the resource.  For a
#' finite resource size is the number of units in the resource.  For an
#' extensive resource size is the extent (measure) of the resource, i.e.,
#' length, area, or volume.  The function can accomodate single-stage and
#' two-stage samples.
#'
#' @param z Vector of the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single- stage sample or the stage two weight for a two-stage sample.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two-stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not a
#'   two-stage sample.
#'
#' @param cluster Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.  The default value is \code{NULL}.
#'
#' @param wgt1 = Vector of the final adjusted stage one weight for each site.
#'   The default value is \code{NULL}.
#'
#' @return The CDF estimate.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

cdf_prop <- function(z, wgt, val, cluster_ind, cluster = NULL, wgt1 = NULL) {

  # Calculate additional required values

  m <- length(val)
  if (cluster_ind) {
    cluster <- factor(cluster)
    ncluster <- length(levels(cluster))
    z_1st <- split(z, cluster)
    wgt2_1st <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
  }

  # Calculate the cdf estimate

  cdf <- numeric(m)
  if (cluster_ind) {
    for (i in 1:m) {
      temp <- numeric(ncluster)
      for (j in 1:ncluster) {
        temp[j] <- sum(ifelse(z_1st[[j]] <= val[i], wgt2_1st[[j]], 0))
      }
      cdf[i] <- sum(wgt1_u * temp)
    }
  } else {
    for (i in 1:m) {
      cdf[i] <- sum(ifelse(z <= val[i], wgt, 0))
    }
  }

  if (cluster_ind) {
    cdf <- cdf / sum(wgt1 * wgt)
  } else {
    cdf <- cdf / sum(wgt)
  }

  # Return the estimate

  cdf
}
