################################################################################
# Function: cdf.test.size.prop
# Programmer: Tom Kincaid
# Date: November 5, 2007
# Last Revised: January 11, 2016
#
#' Size-Weighted Estimate of Population Proportion for Classes
#'
#' This function calculates a size-weighted estimate of the population
#' proportions in a set of intervals (classes).  The set of values defining the
#' upper bound of each class is supplied to the function.  The Horvitz-Thompson
#' ratio estimator, i.e., the ratio of two Horvitz-Thompson estimators, is used
#' to calculate the estimate.  The numerator of the ratio estimates the total of
#' the resource within a class.  The denominator of the ratio estimates the size
#' of the resource.  For a finite resource size is the number of units in the
#' resource.  For an extensive resource size is the extent (measure) of the
#' resource, i.e., length, area, or volume.  The function can accomodate single
#' stage and two-stage samples.
#'
#' @param z Vector of the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param bounds Upper bounds for calculating classes for the CDF.
#'
#' @param cluster.ind Logical value that indicates whether the sample is a
#'   two- stage sample, where TRUE = a two-stage sample and FALSE = not a
#'   two-stage sample.
#'
#' @param cluster Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site. a
#'   two-stage sample.
#'
#' @param swgt Vector of the size-weight for each site, which is the stage two
#'   size-weight for a two-stage sample.
#'
#' @param swgt1 Vector of the stage one size-weight for each site.
#'
#' @return The class proportion estimates.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

cdf.test.size.prop <- function(z, wgt, bounds, cluster.ind, cluster, wgt1, swgt,
   swgt1) {

# Calculate additional required values

   m <- length(bounds)
   wgt <- wgt*swgt
   ubound <- bounds
   lbound <- c(-1e10, bounds[-m])
   if(cluster.ind) {
      cluster <- factor(cluster)
      ncluster <- length(levels(cluster))
      z.lst <- split(z, cluster)
      wgt2.lst <- split(wgt, cluster)
      wgt1 <- wgt1*swgt1
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
   }

# Calculate the class proportions estimate

   phat <- numeric(m)
   if(cluster.ind) {
      for(i in 1:m) {
         temp <- numeric(ncluster)
         for(j in 1:ncluster) {
            temp[j] <- sum(ifelse(z.lst[[j]] <= ubound[i], wgt2.lst[[j]], 0) -
               ifelse(z.lst[[j]] <= lbound[i], wgt2.lst[[j]], 0))
         }
         phat[i] <- sum(wgt1.u*temp)
      }
   } else {
      for(i in 1:m) {
         phat[i] <- sum(ifelse(z <= ubound[i], wgt, 0) - ifelse(z <= lbound[i],
            wgt, 0))
      }
   }

   if(cluster.ind)
      phat <- phat/sum(wgt1*wgt)
   else
      phat <- phat/sum(wgt)

# Return the estimate

   phat
}
