###############################################################################
# Function: catvar_total (exported)
# Programmer: Tom Kincaid
# Date: April 21, 2020
# Revised: February 10, 2021 to properly handle the case where categories
#          contained in the size_names vector are not present among levels of
#          the response varible.
#
#' Variance Estimates of the Estimated Size for Categorical Data
#'
#' This function calculates variance estimates of the estimated size in each of
#' a set of categories.
#'
#' @param z Vector of the values of the categorical response variable or the
#'   site status for each site.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of the x-coordinates for location for each site, which is
#'   either the x-coordinate for a single-stage sample or the stage two
#'   x-coordinate for a two-stage sample.
#'
#' @param y Vector of the y-coordinates for location for each site, which is
#'   either the y-coordinate for a single-stage sample or the stage two
#'   y-coordinate for a two-stage sample.
#'
#' @param size_names Vector of the category names for the response variable plus
#'   "Total".
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not a stratified
#'   sample.
#'
#' @param stratum_level Vector of the stratum for each site.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two-stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not a
#'   two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling units (primary sampling
#'   unit or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @param x1 Vector of the stage one x-coordinates for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinates for location for each site.
#'
#' @param pcfactor_ind Logical value that indicates whether the finite
#'   population correction factor is used during variance estimation, where \code{TRUE}
#'   = use the population correction factor and \code{FALSE} = do not use the factor.
#'   To employ the correction factor for a single-stage sample, a value must be
#'   supplied for argument \code{fpcsize}.  To employ the correction factor for a
#'   two-stage sample, values must be supplied for arguments \code{Ncluster} and
#'   \code{stage1size}.
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
#' @param vartype The choice of variance estimator, where \code{"Local"} = local mean
#'   estimator and \code{"SRS"} = SRS estimator.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and \code{FALSE} = warning
#'   messages were not generated.
#'
#' @param warn_df A data frame for storing warning messages.
#'
#' @param warn_vec A vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{vartype}}{character variable containing the type of variance
#'       estimator}
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
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
#' @author Tom Kincaid \email{kincaid.tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
###############################################################################

catvar_total <- function(z, wgt, x, y, size_names, stratum_ind, stratum_level,
  cluster_ind, clusterID, wgt1, x1, y1, pcfactor_ind, fpcsize, Ncluster,
  stage1size, vartype, warn_ind, warn_df, warn_vec) {

# Assign the function name

  fname <- "catvar_total"

# Branch to handle two-stage and single-stage samples

  if(cluster_ind) {

# Begin the section for a two-stage sample

# Calculate additional required values

    size_names <- size_names[size_names %in% c(levels(z), "Total")]
    m <- length(size_names)
    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_1st <- split(z, cluster)
    if(vartype == "Local") {
      x2_1st <- split(x, cluster)
      y2_1st <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt2_1st <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    if(pcfactor_ind) {
      N.cluster <- unique(Ncluster)
      stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit response
# variable and the variance of those totals for each stage one sampling unit

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m)
    for(i in 1:ncluster) {

# Calculate the weighted indicator matrix

      n <- length(z_1st[[i]])
      im <- cbind(tapply(wgt2_1st[[i]], z_1st[[i]]) == matrix(rep(1:(m-1), n),
        nrow = n, byrow = TRUE), rep(1,n)) * matrix(rep(wgt2_1st[[i]], m),
        nrow = n)

# Calculate total estimates for the stage one sampling unit

      total2est[i,] <- apply(im, 2, sum)

# Adjust the variance estimator for small sample size

      SRSind <- FALSE
      if(vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum_ind) {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate variance of the category size estimates.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=I(stratum_level), warning=I(warn), action=I(act)))
        } else {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category size estimates.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
        }
          vartype <- "SRS"
          SRSind <- TRUE
      }

# Calculate the population correction factor for the stage two sample

      pcfactor <- ifelse(pcfactor_ind, (stage1size_u[i] - n)/stage1size_u[i], 1)

# Calculate variance estimates for the stage one sampling unit

      if(var_ind[i]) {
        if(vartype == "Local") {
          weight_1st <- localmean_weight(x2_1st[[i]], y2_1st[[i]],
            1/wgt2_1st[[i]])
          var2est[i,] <- pcfactor*apply(im, 2, localmean_var, weight_1st)
        } else {
          var2est[i,] <- pcfactor*n*apply(im, 2, var)
          if(SRSind)
            vartype <- "Local"
        }
      }
    }

# Adjust the variance estimator for small sample size

    if(vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if(stratum_ind) {
        warn <- paste("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category size estimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
          stratum=I(stratum_level), warning=I(warn), action=I(act)))
      } else {
        warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the category size \nestimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
          stratum=NA, warning=I(warn), action=I(act)))
      }
      vartype <- "SRS"
    }

# Calculate the population correction factor for the stage one sample

    pcfactor <- ifelse(pcfactor_ind, (N.cluster - ncluster)/N.cluster, 1)

# Calculate the variance estimate

    if(vartype == "Local") {
      weight_1st <- localmean_weight(x1_u, y1_u, 1/wgt1_u)
      varest <- pcfactor*apply(total2est * matrix(rep(wgt1_u, m),
        nrow = ncluster), 2, localmean_var, weight_1st) +
        apply(var2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2, sum)
    } else {
      varest <- NULL
    }

# End of section for a two-stage sample

  } else {

# Begin the section for a single-stage sample

# Calculate additional required values

    n <- length(z)
    size_names <- size_names[size_names %in% c(levels(z), "Total")]
    m <- length(size_names)
    if(pcfactor_ind) {
      fpcsize_u <- unique(fpcsize)
    }

# Calculate the weighted indicator matrix

    im <- cbind(tapply(wgt, z) == matrix(rep(1:(m-1), n), nrow = n,
      byrow = TRUE), rep(1,n)) * matrix(rep(wgt, m), nrow = n)

# Adjust the variance estimator for small sample size

    if(vartype == "Local" && n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if(stratum_ind) {
        warn <- paste("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category size estimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
          stratum=I(stratum_level), warning=I(warn), action=I(act)))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the category size estimates.\n"
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
          stratum=NA, warning=I(warn), action=I(act)))
      }
      vartype <- "SRS"
    }

# Calculate the population correction factor

    pcfactor <- ifelse(pcfactor_ind, (fpcsize_u - n)/fpcsize_u, 1)

# Calculate the variance estimate

    if(vartype == "Local") {
      weight_1st <- localmean_weight(x, y, 1/wgt)
      varest <- pcfactor*apply(im, 2, localmean_var, weight_1st)
    } else {
      varest <- NULL
    }

# End section for a single-stage sample

  }
  if(!is.null(varest)) {
    names(varest) <- size_names
  }

# Return the indicator for type of variance estimator, the variance estimate,
# the warning message indicator, and the warn_df data frame

   list(vartype=vartype, varest=varest, warn_ind=warn_ind, warn_df=warn_df)
}
