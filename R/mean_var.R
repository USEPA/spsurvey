###############################################################################
# Function: mean_var (not exported)
# Programmer: Tom Kincaid
# Date: June 24, 2010
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
#
#' Local Mean Variance Estimate for the Mean
#'
#' This function calculates a variance estimate of the estimated population
#' mean of a response variable. Either the simple random sampling (SRS) variance
#' estimator or the local mean variance estimator is calculated.  The SRS
#' variance estimator uses the independent random sample approximation to
#' calculate joint inclusion probabilities.  The function can  accomodate
#' single-stage and two-stage samples.
#'
#' @param z Vector of the response value for each site.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
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
#' @param mean_est The mean estimate.
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind  Logical value that indicates whether the sample is a
#'   two-stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE}
#'   = not a two-stage sample.
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
#' @param pcfactor_ind Logical value that indicates whether the finite
#'   population correction factor is used during variance estimation, where
#'   \code{TRUE} = use the population correction factor and \code{FALSE} = do
#'   not use the factor.  To employ the correction factor for a single-stage
#'   sample, a value must be supplied for argument \code{fpcsize}.  To employ
#'   the correction factor for a two-stage sample, values must be supplied for
#'   arguments \code{Ncluster} and \code{stage1size}.
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
#' @param stage1size Vector of the size of the stage one sampling units of a
#'   two-stage sample, which is required for calculation of the finite
#'   population correction factor for a two-stage sample.
#'
#' @param vartype The choice of variance estimator, where \code{"Local"} = local
#'   mean estimator and \code{"SRS"} = SRS estimator.
#'
#' @param warn_ind LogicLal value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return Object in list format composed of a vector named \code{varest}, which
#'   contains variance estimates, a logical variable named \code{warn_ind},
#'   which is the indicator for warning messges, and a data frame named
#'   \code{warn_df}, which contains warning messages.
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
#' @noRd
###############################################################################

mean_var <- function(z, wgt, x, y, mean_est, stratum_ind, stratum_level,
                     cluster_ind, cluster, wgt1, x1, y1, pcfactor_ind, fpcsize, Ncluster,
                     stage1size, vartype, warn_ind, warn_df, warn_vec) {

  # Assign the function name

  fname <- "mean_var"

  # Branch to handle two-stage and single-stage samples

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    cluster <- factor(cluster)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_lst <- split(z, cluster)
    if (vartype == "Local") {
      x2_lst <- split(x, cluster)
      y2_lst <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    tw <- sum(wgt * wgt1)
    if (pcfactor_ind) {
      N_cluster <- unique(Ncluster)
      stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # Calculate estimates of the total of the stage two sampling unit response values
    # or residuals and the variance of those totals for each stage one sampling unit

    total2est <- numeric(ncluster)
    var2est <- numeric(ncluster)
    for (i in 1:ncluster) {

      # Calculate weighted response or weighted residual vectors

      n <- length(z_lst[[i]])
      rv_mean <- wgt2_lst[[i]] * (z_lst[[i]] - mean_est)

      # Calculate mean estimate for the stage one sampling unit

      total2est[i] <- sum(rv_mean)

      # Adjust the variance estimator for small sample size

      SRSind <- FALSE
      if (vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if (stratum_ind) {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate variance of the mean estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
            stratum = I(stratum_level), warning = I(warn), action = I(act)
          ))
        } else {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe mean estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
            stratum = NA, warning = I(warn), action = I(act)
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
          weight_lst <- localmean_weight(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt2_lst[[i]]
          )
          if(is.null(weight_lst)) {
            warn_ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling variance estimator was used to \ncalculate variance of the mean estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
            var2est[i] <- pcfactor * n * var(rv_mean)
          } else {
            var2est[i] <- pcfactor * localmean_var(rv_mean, weight_lst)
          }
        } else {
          var2est[i] <- pcfactor * n * var(rv_mean)
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
        warn <- paste("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe mean estimate.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the mean estimate.\n", sep = "")
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
      weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      if(is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator was used to calculate variance of the mean estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level,
          warning = I(warn), action = I(act)
        ))
        varest <- pcfactor * ncluster * apply(total2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, var) + apply(var2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, sum) / (tw^2)
      } else {
        varest <- pcfactor*apply(total2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, localmean_var, weight_lst) + apply(var2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, sum) / (tw^2)
      }
    } else {
      varest <- pcfactor * ncluster * apply(total2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, var) + apply(var2est*matrix(rep(wgt1_u, 4), nrow = ncluster), 2, sum) / (tw^2)
    }

    # End of section for a two-stage sample

  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z)
    tw <- sum(wgt)
    if (pcfactor_ind) {
      fpcsize_u <- unique(fpcsize)
    }

    # Calculate the weighted residuals vector

    rv_mean <- wgt * (z - mean_est)

    # Adjust the variance estimator for small sample size

    if (vartype == "Local" && n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if (stratum_ind) {
        warn <- paste("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe mean estimate.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the mean estimate.\n"
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = NA, warning = I(warn), action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the population correction factor

    pcfactor <- ifelse(pcfactor_ind, (fpcsize_u - n) / fpcsize_u, 1)

    # Calculate variance estimates

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x, y, prb = 1 / wgt)
      if(is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator was used to calculate variance of the mean estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level,
          warning = I(warn), action = I(act)
        ))
        varest <- pcfactor * n * var(rv_mean) / (tw^2)
      } else {
        varest <- pcfactor * localmean_var(rv_mean, weight_lst) / (tw^2)
      }
    } else {
      varest <- pcfactor * n * var(rv_mean) / (tw^2)
    }

    # End section for a single-stage sample
  }

  # Return the variance estimate, the warning message indicator, and the warn_df
  # data frame

  list(vartype = vartype, varest = varest, warn_ind = warn_ind, warn_df = warn_df)
}
