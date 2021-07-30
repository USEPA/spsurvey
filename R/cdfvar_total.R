################################################################################
# Function: cdfvar_total (not exported)
# Programmer: Tom Kincaid
# Date: July 2, 2020
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3, 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8, 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
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
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a two-
#'   stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not
#'   a two-stage sample.
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
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
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
#'     \item{\code{localmean_weight}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{localmean_var}}{calculate the local mean variance
#'       estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

cdfvar_total <- function(z, wgt, x, y, val, stratum_ind, stratum_level,
                         cluster_ind, clusterID, wgt1, x1, y1, warn_ind,
                         warn_df, warn_vec) {

  # Assign the function name

  fname <- "cdfvar_total"

  # Assign the variance type

  vartype <- "Local"

  #
  # Branch to handle two-stage and single-stage samples
  #

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    m <- length(val)
    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_lst <- split(z, cluster)
    x2_lst <- split(x, cluster)
    y2_lst <- split(y, cluster)
    x1_u <- as.vector(tapply(x1, cluster, unique))
    y1_u <- as.vector(tapply(y1, cluster, unique))
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # For each stage one sampling unit and each category, calculate an estimate
    # of the total of the stage two sampling unit residuals and the variance of
    # the total

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m)
    for (i in 1:ncluster) {

      # Calculate the weighted residuals matrix

      n <- length(z_lst[[i]])
      im <-
        ifelse(
          matrix(rep(z_lst[[i]], m), nrow = n) <=
            matrix(rep(val, n), nrow = n, byrow = TRUE),
          1, 0
        )
      rm <- im * matrix(rep(wgt2_lst[[i]], m), nrow = n)

      # Calculate the total estimate for each category

      total2est[i, ] <- apply(rm, 2, sum)

      # Adjust the variance estimator for small sample size

      if (var_ind[i] && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling variance estimator for an infinite population was used to calculate variance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\", \nthe simple random sampling variance estimator for an infinite population was used to calculate variance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
      }

      # Calculate the variance estimate for each category

      if (var_ind[i]) {
        if (vartype == "Local") {
          weight_lst <- localmean_weight(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt2_lst[[i]]
          )
          if (is.null(weight_lst)) {
            warn_ind <- TRUE
            act <- "The simple random sampling variance estimator for an infinite population was used.\n"
            if (stratum_ind) {
              warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \nvariance estimator for an infinite population was used to calculate variance of the \ntotal of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = stratum_level,
                warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling variance estimator for an \ninfinite population was used to calculate variance of the total of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            }
            var2est[i, ] <- n * apply(rm, 2, var)
          } else {
            var2est[i, ] <- apply(rm, 2, localmean_var, weight_lst)
            if (any(var2est[i, ] < 0)) {
              warn_ind <- TRUE
              act <- "The simple random sampling variance estimator for an infinite population was used.\n"
              if (stratum_ind) {
                warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates for \nstage one sampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the total of the residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3],
                  stratum = stratum_level, warning = I(warn), action = I(act)
                ))
              } else {
                warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates for \nstage one sampling unit \"", cluster_levels[i], "\", the simple random sampling variance estimator \nfor an infinite population was used to calculate variance of the total of the residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                  warning = I(warn), action = I(act)
                ))
              }
              var2est[i, ] <- n * apply(rm, 2, var)
            }
          }
        } else {
          var2est[i, ] <- n * apply(rm, 2, var)
          vartype <- "Local"
        }
      }
    }

    # Adjust the variance estimator for small sample size

    if (ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", \nthe simple random sampling variance estimator for an infinite population was used \nto calculate variance of the CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling \nvariance estimator for an infinite population was used to calculate variance of the \nCDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = NA, warning = I(warn), action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA,
            warning = I(warn), action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <-
          apply(
            total2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2,
            localmean_var, weight_lst
          ) +
          apply(var2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2, sum)
      }
    } else {
      varest <- 0
    }

    # End of section for a two-stage sample
  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z)
    m <- length(val)

    # Calculate the weighted residuals matrix

    im <-
      ifelse(
        matrix(rep(z, m), nrow = n) <=
          matrix(rep(val, n), nrow = n, byrow = TRUE),
        1, 0
      )
    rm <- im * matrix(rep(wgt, m), nrow = n)

    # Adjust the variance estimator for small sample size

    if (n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling variance estimator \nfor an infinite population was used to calculate variance of the CDF estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x, y, 1 / wgt)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the CDF estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <- apply(rm, 2, localmean_var, weight_lst)
      }
    } else {
      varest <- 0
    }

    # End of section for a single-stage sample
  }

  # Return the indicator for type of variance estimator, the variance estimate,
  # the warning message indicator, and the warn_df data frame

  list(
    vartype = vartype, varest = varest, warn_ind = warn_ind,
    warn_df = warn_df
  )
}
