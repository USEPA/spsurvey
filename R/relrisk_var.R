################################################################################
# Function: relrisk_var (not exported)
# Programmer: Tom Kincaid
# Date: June 24, 2020
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Variance-Covariance Estimate for the Relative Risk Estimator
#'
#' This function calculates the variance-covariance estimate for the cell and
#' marginal totals used to calculate the relative risk estimate.  Either the
#' simple random sampling (SRS) variance estimator or the local mean variance
#' estimator is calculated, which is subject to user control.  The SRS variance
#' estimator uses the independent random sample approximation to calculate joint
#' inclusion probabilities.  The function can  accomodate single-stage and
#' two-stage samples.
#'
#' @param response Vector of the categorical response variable.
#'
#' @param stressor  Vector of the categorical stressor variable.
#'
#' @param response_levels Vector of category values (levels) for the
#'   categorical response variable, where the first level is used for
#'   calculating the relative risk estimate.
#'
#' @param stressor_levels Vector of category values (levels) for the
#'   categorical stressor variable, where the first level is used for
#'   calculating the numerator of the relative risk estimate and the second
#'   level is used for calculating the denominator of the estimate.
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
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a two-
#'   stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not
#'   a two-stage sample.
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
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @param subset_local If \code{TRUE}, construct neighborhoods using only
#'   domain members. The internal default, \code{FALSE}, retains eligible
#'   responding sites in the sampling stratum and zeros contributions outside
#'   the domain. Used only for local variance estimation.
#'
#' @param subpop_ind Numeric 0/1 vector, the same length as \code{response},
#'   that is 1 for members of the subpopulation (domain) and 0 otherwise.
#'   Only used when \code{subset_local} is \code{FALSE}.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

relrisk_var <- function(response, stressor, response_levels, stressor_levels,
                        wgt, x, y, stratum_ind, stratum_level, cluster_ind,
                        cluster, wgt1, x1, y1, vartype, warn_ind, warn_df,
                        warn_vec, subset_local = FALSE, subpop_ind = NULL, local_nbh = 15, local_context = NULL) {
  local_context <- localmean_group_context(local_context,
    if (is.null(stratum_level)) "unstratified" else paste0("stratum:", stratum_level))
  # Relative risk RR = P(Event | Stressor) / P(Event | No Stressor) needs
  # two joint cell totals and two margin totals, not the full 2x2 table:
  # Ind1 = event & stressor present (joint cell, numerator's numerator),
  # Ind2 = stressor present (margin, numerator's denominator),
  # Ind3 = event & stressor absent (joint cell, denominator's numerator),
  # Ind4 = stressor absent (margin, denominator's denominator). As in
  # attrisk_var(), this function estimates the local mean (or SRS) 4x4
  # covariance matrix of those four totals jointly (see mean_var() for the
  # underlying method), which relrisk_analysis() combines via the delta
  # method into the variance of the relative risk estimate.

  # Assign the function name

  fname <- "relrisk_var"

  if (cluster_ind && vartype == "Local" && !subset_local &&
      !is.null(subpop_ind) && any(subpop_ind == 0)) {
    return(localmean_cluster_risk(response, stressor, response_levels,
      stressor_levels, subpop_ind, wgt, cluster, wgt1, x, y, x1, y1,
      warn_df, warn_vec, local_nbh, local_context, margins = TRUE))
  }

  # Branch to handle two-stage and single-stage samples

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
    wgt1_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # For each stage one sampling unit and each required contingency table cell
    # or margin,  calculate an estimate of the total of the stage two indicator
    # variables and the variance of the total

    total2est <- matrix(0, ncluster, 4)
    var2est <- matrix(0, ncluster, 16)
    for (i in 1:ncluster) {
      # Calculate the number of response values

      nresp <- length(response_lst[[i]])

      # Create indicator variables for required contingency table cells and
      # margins

      Ind1 <- (response_lst[[i]] == response_levels[1]) * (stressor_lst[[i]] ==
        stressor_levels[1])
      Ind2 <- (stressor_lst[[i]] == stressor_levels[1])
      Ind3 <- (response_lst[[i]] == response_levels[1]) * (stressor_lst[[i]] ==
        stressor_levels[2])
      Ind4 <- (stressor_lst[[i]] == stressor_levels[2])

      # Calculate the matrix of weighted indicator variables

      rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt1_lst[[i]]

      # Calculate the total estimate for each indicator variable

      total2est[i, ] <- apply(rm, 2, sum)

      # Adjust the variance-covariance estimator for small sample size

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
          weight_lst <- localmean_weight_internal(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt1_lst[[i]],
            local_nbh = local_nbh, local_context = localmean_group_context(local_context, paste0("stage2:", cluster_levels[i])))
          if (is.null(weight_lst)) {
            warn_ind <- TRUE
            act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
            if (stratum_ind) {
              warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \ncovariance estimator for an infinite population was used to calculate covariance of \nthe total of the indicator variables.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = stratum_level,
                warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the total of the indicator variables.\n")
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

    # Adjust the variance-covariance estimator for small sample size

    if (vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum ", stratum_level, ", the simple \nrandom sampling covariance estimator for an infinite population was used to calculate \ncovariance of the relative risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling covariance \nestimator for an infinite population was used to calculate covariance of the relative risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2], indicator = warn_vec[3],
          stratum = NA, warning = I(warn), action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance-covariance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight_internal(x1_u, y1_u, 1 / wgt1_u, local_nbh = local_nbh, local_context = localmean_group_context(local_context, "stage1"))
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the relative risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate covariance \nof the relative risk estimate.\n")
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
        varest <- localmean_cluster_combine(total2est, var2est, wgt1_u,
          weight_lst, "covariance")
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the relative risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the relative risk estimate.\n")
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
    dind <- if (subset_local) rep(1, nresp) else subpop_ind
    n_eff <- if (subset_local) nresp else sum(subpop_ind)

    # Create indicator variables for required cells and margins

    Ind1 <- (response == response_levels[1]) * (stressor == stressor_levels[1])
    Ind2 <- (stressor == stressor_levels[1])
    Ind3 <- (response == response_levels[1]) * (stressor == stressor_levels[2])
    Ind4 <- (stressor == stressor_levels[2])

    # Calculate the matrix of weighted indicator variables
    # (when subset_local = FALSE, x/y/response/stressor/wgt cover the full
    # stratum rather than domain members only, and multiplying by dind
    # zeroes out every column's contribution from sites outside the
    # domain; the neighbor structure below is still built from the full
    # stratum; see localmean_weight()

    rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt * dind

    # Adjust the variance-covariance estimator for small sample size

    if (vartype == "Local" && n_eff < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum ", stratum_level, ", the simple random \nsampling covariance estimator for an infinite population was used to calculate covariance \nof the relative risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling covariance estimator \nfor an infinite population was used to calculate covariance of the relative risk estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the variance-covariance estimate for the cell and marginal
    # totals

    # srs_var is the SRS fallback variance-covariance estimate, used both
    # when localmean_weight()/localmean_cov() cannot produce a valid
    # estimate and directly when vartype = "SRS". Under subset_local =
    # FALSE, rm's many exact-zero rows for non-domain sites would otherwise
    # deflate var(rm), so the SRS fallback is instead computed only from
    # the domain-member rows.
    srs_var <- if (subset_local) {
      nresp * var(rm)
    } else {
      n_eff * var(rm[subpop_ind == 1, , drop = FALSE])
    }

    if (vartype == "Local") {
      weight_lst <- localmean_weight_internal(x = x, y = y, prb = 1 / wgt, local_nbh = local_nbh, local_context = local_context)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling covariance estimator for an infinite \npopulation was used to calculate covariance of the relative risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean covariance estimator cannot calculate valid estimates, the simple random \nsampling covariance estimator for an infinite population was used to calculate covariance \nof the relative risk estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        varest <- srs_var
      } else {
        varest <- localmean_cov(rm, weight_lst)
        temp <- diag(varest)
        if (any(temp < 0)) {
          warn_ind <- TRUE
          act <- "The simple random sampling covariance estimator for an infinite population was used.\n"
          if (stratum_ind) {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates for \nstratum \"", stratum_level, "\", the simple random sampling covariance estimator for an \ninfinite population was used to calculate covariance of the relative risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1],
              subpop = warn_vec[2], indicator = warn_vec[3],
              stratum = stratum_level, warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste0("The local mean covariance estimator produced one or more negative variance estimates, \nthe simple random sampling covariance estimator for an infinite population was used to \ncalculate covariance of the relative risk estimate.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname), subpoptype = warn_vec[1],
              subpop = warn_vec[2], indicator = warn_vec[3], stratum = NA,
              warning = I(warn), action = I(act)
            ))
          }
          varest <- srs_var
        }
      }
    } else {
      varest <- srs_var
    }

    # End of section for a single-stage sample
  }

  # Return the variance-covariance estimate, the warning message indicator, and
  # the warn_df data frame

  list(varest = varest, warn_ind = warn_ind, warn_df = warn_df)
}
