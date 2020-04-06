################################################################################
# Function: attrisk.var
# Programmer: Tom Kincaid
# Date: November 2, 2010
# Revised: March 30, 2020
#
#' Compute the Variance Estimate for Attributable Risk
#'
#' This function calculates the variance-covariance estimate for the cell totals
#' used to calculate the attributable risk estimate.  Either the simple random
#' sampling (SRS) variance estimator or the local mean variance estimator is
#' calculated, which is subject to user control.  The SRS variance estimator
#' uses the independent random sample approximation to calculate joint inclusion
#' probabilities.  The function can  accomodate single-stage and two-stage
#' samples.
#'
#' @param response Vector of the categorical response variable.
#'
#' @param stressor Vector of the categorical stressor variable.
#'
#' @param response.levels Vector of category values (levels) for the
#'   categorical response variable.  If response.levels equals NULL, then values
#'   "Poor" and "Good" are used for the first level and second level of the
#'   response variable, respectively.  The default is NULL.
#'
#' @param stressor.levels Vector of category values (levels) for the
#'   categorical stressor variable.  If stressor.levels equals NULL, then values
#'   "Poor" and "Good" are used for the first level and second level of the
#'   stressor variable, respectively.  The default is NULL.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'   the x- coordinate for a single-stage sample or the stage two x-coordinate
#'   for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'   the y- coordinate for a single-stage sample or the stage two y-coordinate
#'   for a two-stage sample.
#'
#' @param stratum.ind Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = not a stratified
#'   sample.
#'
#' @param stratum.level Vector of the stratum levels for the sites.
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
#' @param x1 Vector of the stage one x-coordinate for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinate for location for each site.
#'
#' @param pcfactor.ind Logical value that indicates whether the population
#'   correction factor is used during variance estimation, where TRUE = use the
#'   population correction factor and FALSE = do not use the factor.  To employ
#'   the correction factor for a single-stage sample, values must be supplied
#'   for arguments pcfsize and support.  To employ the correction factor for a
#'   two-stage sample, values must be supplied for arguments N.cluster,
#'   stage1size, and support.
#'
#' @param pcfsize Size of the resource, which is required for calculation of
#'   finite and continuous population correction factors for a single-stage
#'   sample. For a stratified sample this argument must be a vector containing a
#'   value for each stratum and must have the names attribute set to identify
#'   the stratum codes.
#'
#' @param N.cluster The number of stage one sampling units in the resource,
#'   which is required for calculation of finite and continuous population
#'   correction factors for a two-stage sample.  For a stratified sample this
#'   variable must be a vector containing a value for each stratum and must have
#'   the names attribute set to identify the stratum codes.
#'
#' @param stage1size Size of the stage one sampling units of a two-stage
#'   sample, which is required for calculation of finite and continuous
#'   population correction factors for a two-stage sample and must have the
#'   names attribute set to identify the stage one sampling unit codes.  For a
#'   stratified sample, the names attribute must be set to identify both stratum
#'   codes and stage one sampling unit codes using a convention where the two
#'   codes are separated by the & symbol, e.g., "Stratum 1&Cluster 1".
#'
#' @param support Vector of the support value for each site - the value one
#'   (1) for a site from a finite resource or the measure of the sampling unit
#'   associated with a site from a continuous resource, which is required for
#'   calculation of finite and continuous population correction factors.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param  warn.ind  Logical value that indicates whether warning messages were
#'   generated, where TRUE = warning messages were generated and FALSE = warning
#'   messages were not generated.
#'
#' @param warn.df A data frame for storing warning messages.
#'
#' @param warn.vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return An object in list format composed of a vector named varest, which
#'   contains the variance-covariance estimate, a logical variable named
#'   warn,ind, which is the indicator for warning messges, and a data frame
#'   named warn.df, which contains warning messages.
#'
#' @seealso 
#'   \code{\link{attrisk.est}} for estimating single relative risk and 
#'   \code{\link{attrisk.analysis}} for estimating relative risk for multiple
#'     variables or subpopulations.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{localmean.cov}}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'     \item{\code{\link{localmean.weight}}}{calculate the weighting matrix for 
#'        the local mean variance estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

attrisk.var <- function(response, stressor, response.levels, stressor.levels,
  wgt, x, y, stratum.ind, stratum.level, cluster.ind, cluster, wgt1, x1, y1, pcfactor.ind, pcfsize, N.cluster, stage1size, support, vartype, warn.ind,
  warn.df, warn.vec) {

# Assign the function name
   fname <- "attrisk.var"

#
# Branch to handle two-stage and single-stage samples
#

   if(cluster.ind) {

#
# Begin the section for a two-stage sample
#

# Calculate additional required values
      cluster <- factor(cluster)
      cluster.levels <- levels(cluster)
      ncluster <- length(cluster.levels)
      response.lst <- split(response, cluster)
      stressor.lst <- split(stressor, cluster)
      if(vartype == "Local") {
         x2.lst <- split(x, cluster)
         y2.lst <- split(y, cluster)
         x1.u <- as.vector(tapply(x1, cluster, unique))
         y1.u <- as.vector(tapply(y1, cluster, unique))
      }
      wgt2.lst <- split(wgt, cluster)
      wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
      if(pcfactor.ind) {
         support.lst <- split(support, cluster)
      } else {
         support.lst <- NULL
      }
      var.ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit response
# values or residuals and the variance of those totals for each stage one
# sampling unit
      total2est <- matrix(0, ncluster, 4)
      var2est <- matrix(0, ncluster, 16)
      for(i in 1:ncluster) {

# Calculate the number of response values
         nresp <- length(response.lst[[i]])

# Create indicator variables for required cells and margins
         Ind1 <- (response.lst[[i]] == response.levels[1])*(stressor.lst[[i]] ==
            stressor.levels[1])
         Ind2 <- (response.lst[[i]] == response.levels[2])*(stressor.lst[[i]] ==
            stressor.levels[1])
         Ind3 <- (response.lst[[i]] == response.levels[1])*(stressor.lst[[i]] ==
            stressor.levels[2])
         Ind4 <- (response.lst[[i]] == response.levels[2])*(stressor.lst[[i]] ==
            stressor.levels[2])

# Calculate the matrix of weighted indicator variables
         rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt2.lst[[i]]

# Calculate total estimates for the stage one sampling unit
         total2est[i,] <- apply(rm, 2, sum)

# Adjust the variance-covariance estimator for small sample size
         SRSind <- FALSE
         if(vartype == "Local" && nresp < 4) {
            warn.ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            if(stratum.ind) {
               warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], "\nin stratum ", stratum.level, ", the simple random sampling variance estimator \nwas used to calculate variance of the category proportion estimates.\n", sep="")
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=warn.vec[1], subpop=warn.vec[2],
                  indicator=warn.vec[3], stratum=I(stratum.level),
                  warning=I(warn), action=I(act)))
            } else {
               warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=warn.vec[1], subpop=warn.vec[2],
                  indicator=warn.vec[3], stratum=NA, warning=I(warn),
                  action=I(act)))
            }
            vartype <- "SRS"
            SRSind <- TRUE
         }

# Calculate the population correction factor for the stage two sample
         pcfactor <- ifelse(pcfactor.ind, (stage1size[i] -
            sum(support.lst[[i]]))/stage1size[i], 1)

# Calculate the variance-covariance estimate for the stage one sampling unit
         if(var.ind[i]) {
            if(vartype == "Local") {
               weight.lst <- localmean.weight(x2.lst[[i]], y2.lst[[i]],
                  1/wgt2.lst[[i]])
               var2est[i,] <- as.vector(pcfactor*localmean_cov(rm, weight.lst))
            } else {
               var2est[i,] <- as.vector(pcfactor*nresp*var(rm))
               if(SRSind)
                  vartype <- "Local"
            }
         }
      }

# Adjust the variance-covariance estimator for small sample size
      if(vartype == "Local" && ncluster < 4) {
         warn.ind <- TRUE
         act <- "The simple random sampling variance estimator was used.\n"
         if(stratum.ind) {
            warn <- paste("There are less than four stage one sampling units in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
               action=I(act)))
         } else {
            warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the category proportion \nestimates.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         }
         vartype <- "SRS"
      }

# Calculate the population correction factor for the stage one sample
      pcfactor <- ifelse(pcfactor.ind, (N.cluster - ncluster)/N.cluster, 1)

# Calculate the variance-covariance estimate
      if(vartype == "Local") {
         weight.lst <- localmean.weight(x1.u, y1.u, 1/wgt1.u)
         varest <- pcfactor*localmean_cov(total2est*matrix(rep(wgt1.u, 4),
            nrow=ncluster), weight.lst) + matrix(apply(var2est *
            matrix(rep(wgt1.u, 16), nrow = ncluster), 2, sum), nrow=4)
      } else {
         varest <- pcfactor*ncluster*var(total2est*matrix(rep(wgt1.u, 4),
            nrow=ncluster)) + matrix(apply(var2est*matrix(rep(wgt1.u, 16),
            nrow = ncluster), 2, sum), nrow=4)
      }

#
# End the section for a two-stage sample
#

   } else {

#
# Begin the section for a single-stage sample
#

# Calculate the number of response values
      nresp <- length(response)

# Create indicator variables for cells
      Ind1 <- (response == response.levels[1])*(stressor == stressor.levels[1])
      Ind2 <- (response == response.levels[2])*(stressor == stressor.levels[1])
      Ind3 <- (response == response.levels[1])*(stressor == stressor.levels[2])
      Ind4 <- (response == response.levels[2])*(stressor == stressor.levels[2])

# Calculate the matrix of weighted indicator variables
      rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt

# Calculate the population correction factor
      pcfactor <- ifelse(pcfactor.ind, (pcfsize - sum(support))/pcfsize, 1)

# Adjust the variance-covariance estimator for small sample size
      if(vartype == "Local" && nresp < 4) {
         warn.ind <- TRUE
         act <- "The simple random sampling variance estimator was used.\n"
         if(stratum.ind) {
            warn <- paste("There are less than four response values in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
               action=I(act)))
         } else {
            warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the category proportion \nestimates.\n"
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         }
         vartype <- "SRS"
      }

# Calculate the variance-covariance estimate for the cell totals
      if (vartype == "Local") {
         wgt.lst <- localmean.weight(x=x, y=y, prb=1/wgt)
         varest <- pcfactor*localmean.cov(rm, wgt.lst)
      } else {
         varest <- pcfactor*nresp*var(rm)
      }

#
# End the section for a single-stage sample
#

   }

# Return the variance-covariance estimate, the warning message indicator, and
# the warn.df data frame
   list(varest=varest, warn.ind=warn.ind, warn.df=warn.df)
}
