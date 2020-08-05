################################################################################
# Function: mean_var
# Programmer: Tom Kincaid
# Date: June 24, 2010
#
#' Local Mean Variance Estimate for the Mean
#'
#' This function calculates variance estimates of the estimated population
#' mean of a response variable. Either the simple random sampling (SRS) variance
#' estimator or the local mean variance estimator is calculated.  The SRS
#' variance estimator uses the independent random sample approximation to
#' calculate joint inclusion probabilities.  The function can  accomodate
#' single-stage and two-stage samples.
#'
#' @param z Vector of the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'   the x-coordinate for a single-stage sample or the stage two x-coordinate
#'   for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'   the y-coordinate for a single-stage sample or the stage two y-coordinate
#'   for a two-stage sample.
#'
#' @param mean.est The mean estimate.
#'
#' @param stratum.ind Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = not a stratified
#'   sample.
#'
#' @param stratum.level The stratum level.
#'
#' @param cluster.ind  Logical value that indicates whether the sample is a
#'   two-stage sample, where TRUE = a two-stage sample and FALSE = not a
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
#' @param pcfactor.ind Logical value that indicates whether the finite
#'   population correction factor is used during variance estimation, where TRUE
#'   = use the population correction factor and FALSE = do not use the factor.
#'   To employ the correction factor for a single-stage sample, a value must be
#'   supplied for argument fpcsize.  To employ the correction factor for a
#'   two-stage sample, values must be supplied for arguments Ncluster and
#'   stage1size.
#'
#' @param fpcsize Size of the resource, which is required for calculation of the
#'   finite population correction factor for a single-stage sample.
#'
#' @param Ncluster The number of stage one sampling units in the resource,
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
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param warn.ind LogicLal value that indicates whether warning messages were
#'   generated, where TRUE = warning messages were generated and FALSE = warning
#'   messages were not generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @param warn.vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return Object in list format composed of a vector named varest, which
#'   contains variance estimates, a logical variable named warn,ind, which is
#'   the indicator for warning messges, and a data frame named warn.df, which
#'   contains warning messages.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{localmean_weight}}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{\link{localmean_var}}}{calculate the local mean variance
#'       estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

mean_var <- function(z, wgt, x, y, mean.est, stratum.ind, stratum.level,
  cluster.ind, cluster, wgt1, x1, y1, pcfactor.ind, fpcsize, Ncluster,
  stage1size, vartype, warn.ind, warn.df, warn.vec) {

# Assign the function name

  fname <- "mean_var"

# Branch to handle two-stage and single-stage samples

  if(cluster.ind) {

# Begin the section for a two-stage sample

# Calculate additional required values

    cluster <- factor(cluster)
    cluster.levels <- levels(cluster)
    ncluster <- length(cluster.levels)
    z.lst <- split(z, cluster)
    if(vartype == "Local") {
       x2.lst <- split(x, cluster)
       y2.lst <- split(y, cluster)
       x1.u <- as.vector(tapply(x1, cluster, unique))
       y1.u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt2.lst <- split(wgt, cluster)
    wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
    tw <- sum(wgt*wgt1)
    if(pcfactor.ind) {
       N.cluster <- unique(Ncluster)
       stage1size.u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var.ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit response values
# or residuals and the variance of those totals for each stage one sampling unit

    total2est <- numeric(ncluster)
    var2est <- numeric(ncluster)
    for(i in 1:ncluster) {

   # Calculate weighted response or weighted residual vectors

      n <- length(z.lst[[i]])
      rv.mean <- wgt2.lst[[i]]*(z.lst[[i]] - mean.est)

   # Calculate total estimates for the stage one sampling unit

      total2est[i] <- sum(rv.mean)

   # Adjust the variance estimator for small sample size

      SRSind <- FALSE
      if(vartype == "Local" && n < 4) {
        warn.ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum.ind) {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], "\nin stratum ", stratum.level, ", the simple random sampling variance estimator \nwas used to calculate variance of the total estimates.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=I(stratum.level), warning=I(warn), action=I(act)))
        } else {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe total estimates.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

# Calculate the population correction factor for the stage two sample

      pcfactor <- ifelse(pcfactor.ind, (stage1size.u[i] - n)/stage1size.u[i], 1)

# Calculate variance estimates for the stage one sampling unit

      if(var.ind[i]) {
        if(vartype == "Local") {
          weight.lst <- localmean_weight(x2.lst[[i]], y2.lst[[i]],
            1/wgt2.lst[[i]])
               var2est[i] <- pcfactor*localmean_var(rv.mean, weight.lst)
        } else {
          var2est[i] <- pcfactor*n*var(rv.mean)
          if(SRSind)
            vartype <- "Local"
        }
      }
    }

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && ncluster < 4) {
         warn.ind <- TRUE
         act <- "The simple random sampling variance estimator was used.\n"
         if(stratum.ind) {
            warn <- paste("There are less than four stage one sampling units in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe total estimates.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
               action=I(act)))
         } else {
            warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the total estimates.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=warn.vec[1], subpop=warn.vec[2],
               indicator=warn.vec[3], stratum=NA, warning=I(warn),
               action=I(act)))
         }
         vartype <- "SRS"
      }

# Calculate the population correction factor for the stage one sample

      pcfactor <- ifelse(pcfactor.ind, (N.cluster - ncluster)/N.cluster, 1)

# Calculate the variance estimate

      if(vartype == "Local") {
        weight.lst <- localmean_weight(x1.u, y1.u, 1/wgt1.u)
        varest <- pcfactor * localmean_var(total2est * wgt1.u, weight.lst) /
          (tw^2)
      } else {
        varest <- NULL
      }

# End of section for a two-stage sample

    } else {

# Begin the section for a single-stage sample

# Calculate additional required values

      n <- length(z)
      tw <- sum(wgt)
      if(pcfactor.ind) {
        fpcsize.u <- unique(fpcsize)
      }

# Calculate the weighted residuals vector

      rv.mean <- wgt * (z - mean.est)

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && n < 4) {
        warn.ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum.ind) {
          warn <- paste("There are less than four response values in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe total estimates.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=I(stratum.level), warning=I(warn), action=I(act)))
        } else {
          warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the total estimates.\n"
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2], indicator=warn.vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
        }
        vartype <- "SRS"
      }

# Calculate the population correction factor

      pcfactor <- ifelse(pcfactor.ind, (fpcsize.u - n) / fpcsize.u, 1)

# Calculate variance estimates

      if(vartype == "Local") {
         weight.lst <- localmean_weight(x, y, prb=1/wgt)
         varest <- pcfactor * localmean_var(rv.mean, weight.lst) / (tw^2)
      } else {
        varest <- NULL
      }

# End section for a single-stage sample

   }

# Return the variance estimate, the warning message indicator, and the warn.df
# data frame

   list(vartype=vartype, varest=varest, warn.ind=warn.ind, warn.df=warn.df)
}
