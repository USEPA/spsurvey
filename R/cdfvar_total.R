###############################################################################
# Function: cdfvar_total (exported)
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
#' @param stratum_ind  Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not a stratified
#'   sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two- stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not a
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
#' @param warn_vec Vector that contains names of the subpopulation type, the
#'   subpopulation, and the response variable.
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
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
###############################################################################

cdfvar_total <- function(z, wgt, x, y, val, stratum_ind, stratum_level,
                         cluster_ind, clusterID, wgt1, x1, y1, pcfactor_ind, fpcsize, Ncluster,
                         stage1size, vartype, warn_ind, warn_df, warn_vec) {

  # Assign the function name

  fname <- "cdfvar_total"

  # Branch to handle two-stage and single-stage samples

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    m <- length(val)
    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_1st <- split(z, cluster)
    if (vartype == "Local") {
      x2_1st <- split(x, cluster)
      y2_1st <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt2_1st <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    if (pcfactor_ind) {
      N_cluster <- unique(Ncluster)
      stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # Calculate estimates of the total of the stage two sampling unit residuals
    # and the variance of those totals for each stage one sampling unit

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m)
    for (i in 1:ncluster) {

      # Calculate the weighted residuals matrix

      n <- length(z_1st[[i]])
      im <- ifelse(matrix(rep(z_1st[[i]], m), nrow = n) <= matrix(rep(
        val,
        n
      ), nrow = n, byrow = TRUE), 1, 0)
      rm <- im * matrix(rep(wgt2_1st[[i]], m), nrow = n)

      # Calculate total estimates for the stage one sampling unit

      total2est[i, ] <- apply(rm, 2, sum)

      # Adjust the variance estimator for small sample size

      SRSind <- FALSE
      if (vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate variance of the CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level),
            warning = I(warn), action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

      # Calculate the population correction factor for the stage two sample

      pcfactor <- ifelse(pcfactor_ind, (stage1size_u[i] - n) / stage1size_u[i], 1)

      # Calculate variance estimates for the stage one sampling unit

      if (var_ind[i]) {
        if (vartype == "Local") {
          weight_1st <- localmean_weight(x2_1st[[i]], y2_1st[[i]], 1 / wgt2_1st[[i]])
          var2est[i, ] <- pcfactor * apply(rm, 2, localmean_var, weight_1st)
        } else {
          var2est[i, ] <- pcfactor * n * apply(rm, 2, var)
          if (SRSind) {
            vartype <- "Local"
          }
        }
      }
    }

    # Adjust the variance estimator for small sample size

    if (vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the population correction factor for the stage one sample

    pcfactor <- ifelse(pcfactor_ind, (N_cluster - ncluster) / N_cluster, 1)

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_1st <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      varest <- pcfactor * apply(total2est * matrix(rep(wgt1_u, m),
        nrow = ncluster
      ), 2, localmean_var, weight_1st) +
        apply(var2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2, sum)
    } else {
      varest <- NULL
    }

    # End of section for a two-stage sample
  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z)
    m <- length(val)
    if (pcfactor_ind) {
      fpcsize_u <- unique(fpcsize)
    }

    # Calculate the weighted residuals matrix

    im <- ifelse(matrix(rep(z, m), nrow = n) <= matrix(rep(val, n),
      nrow = n,
      byrow = TRUE
    ), 1, 0)
    rm <- im * matrix(rep(wgt, m), nrow = n)

    # Adjust the variance estimator for small sample size

    if (vartype == "Local" && n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the CDF estimate.\n"
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the population correction factor

    pcfactor <- ifelse(pcfactor_ind, (fpcsize_u - n) / fpcsize_u, 1)

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_1st <- localmean_weight(x, y, 1 / wgt)
      varest <- pcfactor * apply(rm, 2, localmean_var, weight_1st)
    } else {
      varest <- NULL
    }

    # End section for a single-stage sample
  }

  # Return the indicator for type of variance estimator, the variance estimate,
  # the warning message indicator, and the warn_df data frame

  list(vartype = vartype, varest = varest, warn_ind = warn_ind, warn_df = warn_df)
}
