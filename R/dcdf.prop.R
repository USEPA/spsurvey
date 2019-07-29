################################################################################
# Function: dcdf.prop
# Programmer: Tom Kincaid
# Date: December 3, 2002
# Last Revised: January 27, 2004
#'
#' Deconvoluted Cumulative Distribution Function Estimate for Proportion
#'
#' This function calculates an estimate of the deconvoluted cumulative
#' distribution function (CDF) for the proportion of a discrete or an extensive
#' resource.  The simulation extrapolation deconvolution method (Stefanski and
#' Bay, 1996) is use to deconvolute measurement error variance from the
#' response.  The Horvitz-Thompson ratio estimator, i.e., the ratio of two
#' Horvitz-Thompson estimators, is used to calculate the estimate.  The
#' numerator of the ratio estimates the total of the resource equal to or less
#' than a specified value.  The denominator of the ratio estimates the size of
#' the resource.  For a discrete resource size is the number of units in the
#' resource.  For an extensive resource size is the extent (measure) of the
#' resource, i.e., length, area, or volume.  The function can accomodate
#' single-stage and two-stage samples.
#'
#' @param g Vector of the values of the deconvolution function g(.) evaluated
#'   at a specified value for the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
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
#' @return The deconvoluted CDF estimate.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

dcdf.prop <- function(g, wgt, cluster.ind, cluster, wgt1) {

# Calculate additional required values

   if (cluster.ind) {
      cluster <- factor(cluster)
      ncluster <- length(levels(cluster))
      wgt2.lst <- split(wgt, cluster)
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
   }

# Calculate the cdf estimate

   if (cluster.ind) {
      temp <- array(0, c(ncluster, dim(g[[1]])[2]))
      for (i in 1:ncluster) {
         temp[i,] <- apply(g[[i]]*wgt2.lst[[i]], 2, sum)
      }
      cdf <- apply(wgt1.u*temp, 2, sum)
   } else {
      cdf <- apply(wgt*g, 2, sum)
   }

   if (cluster.ind)
      cdf <- cdf/sum(wgt1*wgt)
   else
      cdf <- cdf/sum(wgt)

# Return the estimate

   cdf
}
