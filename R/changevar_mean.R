################################################################################
# Function: changevar_mean (not exported)
# Programmer: Tom Kincaid
# Date: July 22, 2020
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Covariance or Correlation Matrix Estimate of Change in Means between Two
#' Surveys
#'
#' This function uses the repeat visit sites for two probability surveys to
#' calculate either covariance or correlation estimates of estimated change in
#' population means.  Either the simple random sampling (SRS) variance estimator
#' or the local mean variance estimator is calculated, which is subject to user
#' control.  The simple random sampling variance estimator uses the independent
#' random sample approximation to calculate joint inclusion probabilities.  The
#' function can accomodate single-stage and two-stage samples.
#'
#' @param z1 Vector of the response value for each site for survey one.
#'
#' @param z2 Vector of the response value for each site for survey two.
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
#' @param revisitwgt Logical value that indicates whether each repeat visit site
#'   has the same survey design weight in the two surveys, where \code{TRUE} =
#'   the weight for each repeat visit site is the same and \code{FALSE} = the
#'   weight for each repeat visit site is not the same.  When this argument is
#'   \code{FALSE}, all of the repeat visit sites are assigned equal weights when
#'   calculating the covariance component of the change estimate standard error.
#'
#' @param mean1 The estimated mean for survey one.
#'
#' @param mean2 The estimated mean for survey two.
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two- stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE}
#'   = not a two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling unit (primary sampling unit
#'  or cluster) code for each site.
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
#' @param warn_ind  Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return An object in list format composed of a vector named \code{rslt},
#'   which contains the covariance or correlation estimate, a logical variable
#'   named \code{warn_ind}, which is the indicator for warning messges, and a
#'   data frame named \code{warn_df}, which contains warning messages.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{localmean_weight}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{localmean_cov}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

changevar_mean <- function(z1, z2, wgt, x, y, revisitwgt, mean1, mean2,
                           stratum_ind, stratum_level, cluster_ind, clusterID,
                           wgt1, x1, y1, vartype, warn_ind, warn_df, warn_vec) {

  # Assign the function name

  fname <- "changevar_mean"

  #
  # Calculate covariance or correlation using the repeat visit sites
  #

  # Begin the section for a two-stage sample

  if (cluster_ind) {

    # Calculate additional required values

    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z1_lst <- split(z1, cluster)
    z2_lst <- split(z2, cluster)
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    tw2 <- (sum(wgt1 * wgt))^2
    if (vartype == "Local") {
      x2_lst <- split(x, cluster)
      y2_lst <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # Determine whether the mean could be calculated for both surveys
    if (is.na(mean1) | is.na(mean2)) {
      warn_ind <- TRUE
      act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
      if (stratum_ind) {
        warn <- paste("The repeat visit sites mean in stratum \"", stratum_level, "\" \ncould not be calculated in one of the surveys.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste("The repeat visit sites mean could not be calculated in one of the surveys.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      rslt <- NA
    } else {

      # For each stage one sampling unit and each survey, calculate an estimate
      # of the total of the stage two sampling unit residuals; then calculate
      # the variance/covariance matrix of the totals

      total2est <- matrix(0, ncluster, 2)
      var2est <- matrix(0, ncluster, 4)
      phat <- c(mean1, mean2)
      for (i in 1:ncluster) {

        # Calculate the weighted residuals matrix

        n <- length(z1_lst[[i]])
        z1 <- z1_lst[[i]]
        z2 <- z2_lst[[i]]
        rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow = n, byrow = TRUE)) *
          matrix(rep(wgt2_lst[[i]], 2), nrow = n)

        # Calculate total estimates for the stage one sampling unit

        total2est[i, ] <- apply(rm, 2, sum)

        # Adjust the variance/covariance estimator for small sample size

        SRSind <- FALSE
        if (vartype == "Local" && n < 4) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the residuals.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
              action = I(act)
            ))
          } else {
            warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\", \nthe simple random sampling covariance estimator for an infinite population was used \nto calculate covariance of the total of the residuals.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          vartype <- "SRS"
          SRSind <- TRUE
        }

        # Calculate variance/covariance estimates for the stage one sampling unit

        if (var_ind[i]) {
          if (vartype == "Local") {
            weight_lst <- localmean_weight(
              x2_lst[[i]], y2_lst[[i]],
              1 / wgt2_lst[[i]]
            )
            if(is.null(weight_lst)) {
              warn_ind <- TRUE
              act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
              if (stratum_ind) {
                warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \ncovariance estimator for an infinite population was used to calculate covariance of the \ntotal of the residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                  indicator = warn_vec[3], stratum = stratum_level,
                  warning = I(warn), action = I(act)
                ))
              } else {
                warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the residuals.\n")
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                  indicator = warn_vec[3], stratum = NA, warning = I(warn),
                  action = I(act)
                ))
              }
              var2est[i, ] <- as.vector(n * var(rm))
            } else {
              temp <- localmean_cov(rm, weight_lst)
              var2est[i, ] <- as.vector(temp)
              if(any(diag(temp) < 0)) {
                warn_ind <- TRUE
                act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
                if (stratum_ind) {
                  warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstage one sampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the total of the residuals.\n")
                  warn_df <- rbind(warn_df, data.frame(
                    func = I(fname), subpoptype = warn_vec[1],
                    subpop = warn_vec[2], indicator = warn_vec[3],
                    stratum = stratum_level, warning = I(warn), action = I(act)
                  ))
                } else {
                  warn <- paste0("The local mean covariance estimator produced one or more  negative variance estimates \nfor stage one sampling unit \"", cluster_levels[i], "\", the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the total of \nthe residuals.\n")
                  warn_df <- rbind(warn_df, data.frame(
                    func = I(fname), subpoptype = warn_vec[1],
                    subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                    warning = I(warn), action = I(act)
                  ))
                }
                var2est[i, ] <- as.vector(n * var(rm))
              }
            }
          } else {
            var2est[i, ] <- as.vector(n * var(rm))
            if (SRSind) {
              vartype <- "Local"
            }
          }
        }
      }

      # Adjust the variance estimator for small sample size

      if (vartype == "Local" && ncluster < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", the simple \nrandom sampling covariance estimator for an infinite population was used to calculate \ncovariance of the change estimate for means.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
            stratum = I(stratum_level), warning = I(warn), action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four stage one sampling units, the simple random sampling covariance\nestimator for an infinite population was used to calculate covariance of the change \nestimate for means.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
            stratum = NA, warning = I(warn), action = I(act)
          ))
        }
        vartype <- "SRS"
      }

      # Calculate the covariance or correlation estimates

      if (vartype == "Local") {
        weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
        if(is.null(weight_lst)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn_ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the change estimate for means.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
              action = I(act)
            ))
          } else {
            warn_ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the change estimate for means.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA,
              warning = I(warn), action = I(act)
            ))
          }
          varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, 2),
            nrow = ncluster)) + matrix(apply(var2est * matrix(rep(wgt1_u, 4),
            nrow = ncluster), 2, sum), nrow = 2)) / tw2
        } else {
          varest <- (localmean_cov(total2est * matrix(rep(wgt1_u, 2),
            nrow = ncluster), weight_lst) + matrix(apply(var2est *
            matrix(rep(wgt1_u, 4), nrow = ncluster), 2, sum), nrow = 2)) / tw2
          temp <- diag(varest)
          if(any(temp < 0)) {
            warn_ind <- TRUE
            act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
            if (stratum_ind) {
              warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the change estimate for means.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1],
                subpop = warn_vec[2], indicator = warn_vec[3],
                stratum = stratum_level, warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the change estimate for means.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1],
                subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                warning = I(warn), action = I(act)
              ))
            }
            varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, 2),
              nrow = ncluster)) + matrix(apply(var2est * matrix(rep(wgt1_u, 4),
              nrow = ncluster), 2, sum), nrow = 2)) / tw2
          }
        }
      } else {
        varest <- (ncluster * var(total2est * matrix(rep(wgt1_u, 2),
          nrow = ncluster )) + matrix(apply(var2est * matrix(rep(wgt1_u, 4),
          nrow = ncluster), 2, sum), nrow = 2)) / tw2
      }
      if (revisitwgt) {
        rslt <- varest[1, 2]
      } else {
        if (varest[1, 1] == 0 | varest[2, 2] == 0 | any(is.na(varest))) {
          warn_ind <- TRUE
          act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
          if (stratum_ind) {
            warn <- paste("The variance estimate for the repeat visit sites mean in stratum \"", stratum_level, "\" \nwas equal to zero for at least one of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = I(stratum_level),
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste("The variance estimate for the repeat visit sites mean was equal to zero \nfor at least \none of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          rslt <- NA
        } else {
          rslt <- varest[1, 2] / sqrt(varest[1, 1] * varest[2, 2])
        }
      }
    }

    # End of  section for a two-stage sample

  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z1)
    tw2 <- (sum(wgt))^2

    # Determine whether the mean could be calculated for both surveys

    if (is.na(mean1) | is.na(mean2)) {
      warn_ind <- TRUE
      act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
      if (stratum_ind) {
        warn <- paste("The repeat visit sites mean in stratum \"", stratum_level, "\" \ncould not be calculated in one of the surveys.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste("The repeat visit sites mean could not be calculated in one of the surveys.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      rslt <- NA
    } else {

      # Calculate the weighted residuals matrix

      phat <- c(mean1, mean2)
      rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow = n, byrow = TRUE)) *
        matrix(rep(wgt, 2), nrow = n)

      # Adjust the variance estimator for small sample size

      if (vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the change estimate for means.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level),
            warning = I(warn), action = I(act)
          ))
        } else {
          warn <- paste0("\nThere are less than four response values, the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the change \nestimate for means.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
      }

      # Calculate covariance or correlation estimates

      if (vartype == "Local") {
        weight_lst <- localmean_weight(x, y, 1 / wgt)
        if(is.null(weight_lst)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the change estimate for means.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate \ncovariance of the change estimate for means.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          varest <- n * var(rm) / tw2
        } else {
          varest <- localmean_cov(rm, weight_lst) / tw2
          temp <- diag(varest)
          if(any(temp < 0)) {
            warn_ind <- TRUE
            act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
            if (stratum_ind) {
              warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the change estimate for means.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1],
                subpop = warn_vec[2], indicator = warn_vec[3],
                stratum = stratum_level, warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the change estimate for means.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1],
                subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
                warning = I(warn), action = I(act)
              ))
            }
            varest <- n * var(rm) / tw2
          }
        }
      } else {
        varest <- n * var(rm) / tw2
      }
      if (revisitwgt) {
        rslt <- varest[1, 2]
      } else {
        if (varest[1, 1] == 0 | varest[2, 2] == 0 | any(is.na(varest))) {
          warn_ind <- TRUE
          act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
          if (stratum_ind) {
            warn <- paste("The variance estimate for the repeat visit sites mean in stratum \"", stratum_level, "\" \nwas equal to zero for at least one of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = I(stratum_level),
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste("The variance estimate for the repeat visit sites mean was equal to zero \nfor at least \none of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          rslt <- NA
        } else {
          rslt <- varest[1, 2] / sqrt(varest[1, 1] * varest[2, 2])
        }
      }
    }

    # End of section for a single-stage sample

  }

  # Return the covariance or correlation estimate, the warning message indicator,
  # and the warn_df data frame

  list(rslt = rslt, warn_ind = warn_ind, warn_df = warn_df)
}
