################################################################################
# Function: cdfvar_total
# Programmer: Tom Kincaid
# Date: July 2, 2020
#
#' Variance Estimates of the Estimated CDF using the Total Scale
#'
#' This function calculates variance estimates of the estimated cumulative
#' distribution function (CDF) for the total
#'
#' @param z Vector of the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single- stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'   the x-coordinate for a single-stage sample or the stage two x-coordinate
#'   for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'   the y-coordinate for a single-stage sample or the stage two y-coordinate
#'   for a two-stage sample.
#'
#' @param val Vector of the set of values at which the CDF is estimated.
#'
#' @param stratum.ind  Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = not a stratified
#'   sample.
#'
#' @param stratum.level The stratum level.
#'
#' @param cluster.ind Logical value that indicates whether the sample is a
#'   two- stage sample, where TRUE = a two-stage sample and FALSE = not a
#'   two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling unit (primary sampling unit
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
#'   which is required for calculation of the finite population correction
#'   factor for a two-stage sample.
#'
#' @param stage1size Vector of the size of the stage one sampling units of a
#'   two-stage sample, which is required for calculation of the finite
#'   population correction factor for a two-stage sample.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param warn.ind Logical value that indicates whether warning messages were
#'   generated, where TRUE = warning messages were generated and FALSE = warning
#'   messages were not generated.
#'
#' @param warn.df A data frame for storing warning messages.
#'
#' @param warn.vec Vector that contains names of the subpopulation type, the
#'   subpopulation, and the response variable.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{vartype}}{character variable containing the type of variance
#'       estimator}
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
#'   }
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

cdfvar_total <- function(z, wgt, x, y, val, stratum.ind, stratum.level,
  cluster.ind, clusterID, wgt1, x1, y1, pcfactor.ind, fpcsize, Ncluster,
  stage1size, vartype, warn.ind, warn.df, warn.vec) {

# Assign the function name

  fname <- "cdfvar_total"

# Branch to handle two-stage and single-stage samples

  if(cluster.ind) {

# Begin the section for a two-stage sample

# Calculate additional required values

    m <- length(val)
    cluster <- factor(clusterID)
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
    if(pcfactor.ind) {
      N.cluster <- unique(Ncluster)
      stage1size.u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var.ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit residuals
# and the variance of those totals for each stage one sampling unit

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m)
    for(i in 1:ncluster) {

# Calculate the weighted residuals matrix

      n <- length(z.lst[[i]])
      im <- ifelse(matrix(rep(z.lst[[i]], m), nrow = n) <= matrix(rep(val,
        n), nrow = n, byrow = TRUE), 1, 0)
      rm <- im * matrix(rep(wgt2.lst[[i]], m), nrow = n)

# Calculate total estimates for the stage one sampling unit

      total2est[i,] <- apply(rm, 2, sum)

# Adjust the variance estimator for small sample size

      SRSind <- FALSE
      if(vartype == "Local" && n < 4) {
        warn.ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum.ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit ", cluster.levels[i], "\nin stratum ", stratum.level, ", the simple random sampling variance estimator \nwas used to calculate variance of the CDF estimate.\n")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=I(stratum.level),
            warning=I(warn), action=I(act)))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit ", cluster.levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=NA, warning=I(warn),
            action=I(act)))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

# Calculate the population correction factor for the stage two sample

    pcfactor <- ifelse(pcfactor.ind, (stage1size.u[i] - n)/stage1size.u[i], 1)

# Calculate variance estimates for the stage one sampling unit

      if(var.ind[i]) {
        if(vartype == "Local") {
          weight.lst <- localmean_weight(x2.lst[[i]], y2.lst[[i]], 1/wgt2.lst[[i]])
          var2est[i,] <- pcfactor*apply(rm, 2, localmean_var, weight.lst)
        } else {
          var2est[i,] <- pcfactor*n*apply(rm, 2, var)
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
        warn <- paste0("There are less than four stage one sampling units in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the CDF estimate.\n")
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
      varest <- pcfactor*apply(total2est * matrix(rep(wgt1.u, m),
        nrow = ncluster), 2, localmean_var, weight.lst) +
        apply(var2est * matrix(rep(wgt1.u, m), nrow = ncluster), 2, sum)
    } else {
      varest <- NULL
    }

# End of section for a two-stage sample

  } else {

# Begin the section for a single-stage sample

# Calculate additional required values

    n <- length(z)
    m <- length(val)
    if(pcfactor.ind) {
      fpcsize.u <- unique(fpcsize)
    }

# Calculate the weighted residuals matrix

    im <- ifelse(matrix(rep(z, m), nrow = n) <= matrix(rep(val, n), nrow = n,
      byrow = TRUE), 1, 0)
    rm <- im * matrix(rep(wgt, m), nrow = n)

# Adjust the variance estimator for small sample size

    if(vartype == "Local" && n < 4) {
      warn.ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if(stratum.ind) {
        warn <- paste0("There are less than four response values in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the CDF estimate.\n"
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=NA, warning=I(warn),
          action=I(act)))
       }
      vartype <- "SRS"
    }

# Calculate the population correction factor

    pcfactor <- ifelse(pcfactor.ind, (fpcsize.u - n)/fpcsize.u, 1)

# Calculate the variance estimate

    if(vartype == "Local") {
      weight.lst <- localmean_weight(x, y, 1/wgt)
      varest <- pcfactor * apply(rm, 2, localmean_var, weight.lst)
    } else {
      varest <- NULL
    }

# End section for a single-stage sample

  }

# Return the indicator for type of variance estimator, the variance estimate,
# the warning message indicator, and the warn.df data frame

  list(vartype=vartype, varest=varest, warn.ind=warn.ind, warn.df=warn.df)
}
