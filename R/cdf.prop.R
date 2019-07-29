################################################################################
# Function: cdf.prop
# Programmer: Tom Kincaid
# Date: July 12, 2000
# Last Revised: January 27, 2004
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
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single- stage sample or the stage two weight for a two-stage sample.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @param cluster.ind Logical value that indicates whether the sample is a
#'   two- stage sample, where TRUE = a two-stage sample and FALSE = not a
#'   two-stage sample.
#'
#' @param cluster Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @return The CDF estimate.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

cdf.prop <- function(z, wgt, val, cluster.ind, cluster, wgt1) {

# Calculate additional required values

   m <- length(val)
   if(cluster.ind) {
      cluster <- factor(cluster)
      ncluster <- length(levels(cluster))
      z.lst <- split(z, cluster)
      wgt2.lst <- split(wgt, cluster)
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
   }

# Calculate the cdf estimate

   cdf <- numeric(m)
   if(cluster.ind) {
      for(i in 1:m) {
         temp <- numeric(ncluster)
         for(j in 1:ncluster) {
            temp[j] <- sum(ifelse(z.lst[[j]] <= val[i], wgt2.lst[[j]], 0))
         }
         cdf[i] <- sum(wgt1.u*temp)
      }
   } else {
      for(i in 1:m) {
         cdf[i] <- sum(ifelse(z <= val[i], wgt, 0))
      }
   }

   if(cluster.ind)
      cdf <- cdf/sum(wgt1*wgt)
   else
      cdf <- cdf/sum(wgt)

# Return the estimate

   cdf
}
