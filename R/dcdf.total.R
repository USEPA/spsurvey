################################################################################
# Function: dcdf.total
# Programmer: Tom Kincaid
# Date: December 3, 2002
# Last Revised: January 27, 2004
#
#' Deconvoluted Cumulative Distribution Function for Total
#'
#' This function calculates an estimate of the deconvoluted cumulative
#' distribution function (CDF) for the total of a discrete or an extensive
#' resource.  The simulation extrapolation deconvolution method (Stefanski and
#' Bay, 1996) is use to deconvolute measurement error variance from the
#' response.  If the known extent of the resource is provided, the classic ratio
#' estimator is used to calculate the estimate. That estimator is the product of
#' the known extent of the resource and the Horvitz-Thompson ratio estimator,
#' where the latter is the ratio of two Horvitz-Thompson estimators. The
#' numerator of the ratio estimates the total of the resource equal to or less
#' than a specified value.  The denominator of the ratio estimates the extent of
#' the resource.  If the known extent of the resource is not provided, the
#' Horvitz-Thompson estimator of the total of the resource equal to or less than
#' a specified value is used to calculate the estimate. For a discrete resource,
#' size is the number of units in the resource.  For an extensive resource, size
#' is the measure of the resource, i.e., length, area, or volume.  The function
#' can accomodate single-stage and two-stage samples.
#'
#' @param g Vector of the values of the deconvolution function g(.) evaluated
#'   at a specified value for the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single- stage sample or the stage two weight for a two-stage sample.
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
#' @param popsize Known size of the resource, which is used to perform ratio
#'   adjustment to estimators expressed using measurement units for the
#'   resource.  For a finite resource, this argument is either the total number
#'   of sampling units or the known sum of size-weights.  For an extensive
#'   resource, this argument is the measure of the resource, i.e., either known
#'   total length for a linear resource or known total area for an areal
#'   resource.  For a stratified sample this variable must be a vector
#'   containing a value for each stratum and must have the names attribute set
#'   to identify the stratum codes.
#'
#' @return The deconvoluted CDF estimate.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

dcdf.total <- function(g, wgt, cluster.ind, cluster, wgt1, popsize) {

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

# Adjust the estimate when the size of the resource is known

   if (!is.null(popsize)) {
      if (cluster.ind)
         cdf <- popsize*(cdf/sum(wgt1*wgt))
      else
         cdf <- popsize*(cdf/sum(wgt))
   }

# Return the estimate

   cdf
}
