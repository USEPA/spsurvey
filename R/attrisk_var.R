################################################################################
# Function: attrisk_var (not exported)
# Programmer: Tom Kincaid
# Date: June 24, 2020
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Compute the Variance/Covariance Estimate for Attributable Risk
#'
#' This function calculates the variance/covariance estimate for the cell totals
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
#' @param response_levels Vector of category values (levels) for the
#'   categorical response variable.
#'
#' @param stressor_levels Vector of category values (levels) for the
#'   categorical stressor variable.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
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
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} =
#'   not a stratified  sample.
#'
#' @param stratum_level Vector of the stratum levels for the sites.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
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
#' @param vartype The choice of variance estimator, where \code{"Local"} = local
#'   mean estimator and \code{"SRS"} = SRS estimator.
#'
#' @param  warn_ind  Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df A data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{localmean_cov}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'     \item{\code{localmean_weight}}{calculate the weighting matrix for
#'        the local mean variance estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

attrisk_var <- function(response, stressor, response_levels, stressor_levels,
                        wgt, x, y, stratum_ind, stratum_level, cluster_ind,
                        cluster, wgt1, x1, y1, vartype, warn_ind, warn_df,
                        warn_vec) {

  # Assign the function name
  fname <- "attrisk_var"

  #
  # Branch to handle two-stage and single-stage samples
  #

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    cluster <- factor(cluster)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    response_lst <- split(response, cluster)
    stressor_lst <- split(stressor, cluster)
    if (vartype == "Local") {
      x2_lst <- split(x, cluster)
      y2_lst <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # For each stage one sampling unit and each contingency table cell,
    # calculate an estimate of the total of the stage two indicator variables
    # and the variance of the total

    total2est <- matrix(0, ncluster, 4)
    var2est <- matrix(0, ncluster, 16)
    for (i in 1:ncluster) {

      # Calculate the number of response values

      nresp <- length(response_lst[[i]])

      # Create indicator variables for contingency table cells

      Ind1 <- (response_lst[[i]] == response_levels[1]) * (stressor_lst[[i]] ==
        stressor_levels[1])
      Ind2 <- (response_lst[[i]] == response_levels[2]) * (stressor_lst[[i]] ==
        stressor_levels[1])
      Ind3 <- (response_lst[[i]] == response_levels[1]) * (stressor_lst[[i]] ==
        stressor_levels[2])
      Ind4 <- (response_lst[[i]] == response_levels[2]) * (stressor_lst[[i]] ==
        stressor_levels[2])

      # Calculate the matrix of weighted indicator variables

      rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt2_lst[[i]]

      # Calculate the total estimate for each indicator variable

      total2est[i, ] <- apply(rm, 2, sum)

      # Adjust the variance/covariance estimator for small sample size

      SRSind <- FALSE
      if (vartype == "Local" && nresp < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the total of the indicator variables.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\", \nthe simple random sampling covariance estimator for an infinite population was used \nto calculate covariance of the total of the indicator variables.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

      # Calculate the variance/covariance estimate for the weighted indicator
      # variables

      if (var_ind[i]) {
        if (vartype == "Local") {
          weight_lst <- localmean_weight(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt2_lst[[i]]
          )
          if (is.null(weight_lst)) {
            if (stratum_ind) {
              warn_ind <- TRUE
              act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
              warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \ncovariance estimator for an infinite population was used to calculate covariance of the \ntotal of the indicator variables.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = stratum_level,
                warning = I(warn), action = I(act)
              ))
            } else {
              warn_ind <- TRUE
              act <- "The simple random sampling covariance estimator was used.\n"
              warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the indicator variables.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            }
            var2est[i, ] <- as.vector(nresp * var(rm))
          } else {
            temp <- localmean_cov(rm, weight_lst)
            var2est[i, ] <- as.vector(temp)
            if (any(diag(temp) < 0)) {
              warn_ind <- TRUE
              act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
              if (stratum_ind) {
                warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstage one sampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the total of the indicator variables.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3],
                  stratum = stratum_level, warning = I(warn), action = I(act)
                ))
              } else {
                warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates \nfor stage one sampling unit \"", cluster_levels[i], "\", the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the total of \nthe indicator variables.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1],
                  subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                  warning = I(warn), action = I(act)
                ))
              }
              var2est[i, ] <- as.vector(nresp * var(rm))
            }
          }
        } else {
          var2est[i, ] <- as.vector(nresp * var(rm))
          if (SRSind) {
            vartype <- "Local"
          }
        }
      }
    }

    # Adjust the variance/covariance estimator for small sample size

    if (vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", the simple \nrandom sampling covariance estimator for an infinite population was used to calculate \ncovariance of the attributable risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the attributable \nrisk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance/covariance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite population \nwas used to calculate covariance of the attributable risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the attributable risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA,
            warning = I(warn), action = I(act)
          ))
        }
        varest <- ncluster * var(total2est * matrix(rep(wgt1_u, 4),
          nrow = ncluster
        )) + matrix(apply(var2est * matrix(rep(wgt1_u, 16),
          nrow = ncluster
        ), 2, sum), nrow = 4)
      } else {
        varest <- localmean_cov(total2est * matrix(rep(wgt1_u, 4),
          nrow = ncluster
        ), weight_lst) + matrix(apply(var2est *
          matrix(rep(wgt1_u, 16), nrow = ncluster), 2, sum), nrow = 4)
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the attributable risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the attributable risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, 4),
            nrow = ncluster
          )) + matrix(apply(var2est * matrix(rep(wgt1_u, 16),
            nrow = ncluster
          ), 2, sum), nrow = 4))
        }
      }
    } else {
      varest <- ncluster * var(total2est * matrix(rep(wgt1_u, 4),
        nrow = ncluster
      )) + matrix(apply(var2est * matrix(rep(wgt1_u, 16),
        nrow = ncluster
      ), 2, sum), nrow = 4)
    }

    # End of section for a two-stage sample
  } else {

    # Begin the section for a single-stage sample

    # Calculate the number of response values
    nresp <- length(response)

    # Create indicator variables for cells
    Ind1 <- (response == response_levels[1]) * (stressor == stressor_levels[1])
    Ind2 <- (response == response_levels[2]) * (stressor == stressor_levels[1])
    Ind3 <- (response == response_levels[1]) * (stressor == stressor_levels[2])
    Ind4 <- (response == response_levels[2]) * (stressor == stressor_levels[2])

    # Calculate the matrix of weighted indicator variables
    rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt

    # Adjust the variance/covariance estimator for small sample size

    if (vartype == "Local" && nresp < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple random \nsampling covariance estimator for an infinite population was used to calculate covariance \nof the attributable risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling covariance estimator \nfor an infinite population was used to calculate covariance of the attributable risk \nestimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance/covariance estimate for the cell totals

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x = x, y = y, prb = 1 / wgt)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite population \nwas used to calculate covariance of the attributable risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate covariance \nof the attributable risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        varest <- nresp * var(rm)
      } else {
        varest <- localmean_cov(rm, weight_lst)
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the attributable risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1],
              subpop = warn_vec[2], indicator = warn_vec[3],
              stratum = stratum_level, warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the attributable risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1],
              subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
              warning = I(warn), action = I(act)
            ))
          }
          varest <- nresp * var(rm)
        }
      }
    } else {
      varest <- nresp * var(rm)
    }

    # End of section for a single-stage sample
  }

  # Return the variance/covariance estimate, the warning message indicator, and
  # the warn_df data frame

  list(varest = varest, warn_ind = warn_ind, warn_df = warn_df)
}
