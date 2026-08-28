################################################################################
# Function: total_var (not exported)
# Programmer: Tom Kincaid
# Date: October 25, 2021
#
#' Local Mean Variance Estimate for the Total
#'
#' This function calculates a variance estimate of the estimated population
#' total of a response variable. Either the simple random sampling (SRS) variance
#' estimator or the local total variance estimator is calculated.  The SRS
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
#' @param warn_ind LogicLal value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @param subset_local Logical value indicating whether the local mean
#'   neighbor structure is built from domain members only (\code{TRUE},
#'   the default behavior) or from all sites in the stratum
#'   with the response zeroed out for non-domain members via
#'   \code{subpop_ind} (\code{FALSE}). The default value is \code{TRUE}.
#'
#' @param subpop_ind Numeric 0/1 vector, the same length as \code{z}, that
#'   is 1 for members of the subpopulation (domain, in classical survey
#'   sampling terminology) and 0 otherwise. Only used when
#'   \code{subset_local} is \code{FALSE}.
#'
#' @return Object in list format composed of a vector named \code{varest}, which
#'   contains variance estimates, a logical variable named \code{warn_ind},
#'   which is the indicator for warning messges, and a data frame named
#'   \code{warn_df}, which contains warning messages.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{localmean_weight}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{localmean_var}}{calculate the local mean variance estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

total_var <- function(z, wgt, x, y, stratum_ind, stratum_level, cluster_ind,
                      cluster, wgt1, x1, y1, warn_ind, warn_df, warn_vec,
                      subset_local = TRUE, subpop_ind = NULL) {
  # This is the local mean (spatial neighborhood) variance estimator for a
  # TOTAL, following the same approach as mean_var() (see that file for
  # details on the neighborhood-contrast method of Stevens & Olsen 2003 and
  # the two-stage between/within decomposition) but using the raw weighted
  # response wgt * z as the residual (rather than subtracting the mean
  # estimate) and skipping the final division by squared total weight,
  # since the target here is Var(total) rather than Var(mean).

  # Assign the function name

  fname <- "total_var"

  # Assign the variance type

  vartype <- "Local"

  #
  # Branch to handle two-stage and single-stage samples
  #

  if (cluster_ind) {
    # Begin the section for a two-stage sample

    # Calculate additional required values

    cluster <- factor(cluster)
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

    # Calculate estimates of the total of the stage two sampling unit response
    # values or residuals and the variance of those totals for each stage one
    # sampling unit

    total2est <- numeric(ncluster)
    var2est <- numeric(ncluster)
    for (i in 1:ncluster) {
      # Calculate the weighted residuals vector

      n <- length(z_lst[[i]])
      rv_total <- wgt2_lst[[i]] * z_lst[[i]]

      # Calculate total of the residuals for the stage one sampling unit

      total2est[i] <- sum(rv_total)

      # Adjust the variance estimator for small sample size

      if (var_ind[i] && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling variance estimator for an \ninfinite population was used to calculate variance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\", \nthe simple random sampling variance estimator for an infinite population was used to \ncalculate variance of the total of the residuals.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
      }

      # Calculate variance estimates for the stage one sampling unit

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
              warn <- paste0("The local total variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \nvariance estimator for an infinite population was used to calculate variance of the \ntotal of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = stratum_level,
                warning = I(warn), action = I(act)
              ))
            } else {
              warn <- paste0("The local total variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling variance estimator for an \ninfinite population was used to calculate variance of the total of the residuals.\n")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            }
            var2est[i] <- n * var(rv_total)
          } else {
            var2est[i] <- localmean_var(rv_total, weight_lst)
          }
        } else {
          var2est[i] <- n * var(rv_total)
          vartype <- "Local"
        }
      }
    }

    # Adjust the variance estimator for small sample size

    if (ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", \nthe simple random sampling variance estimator for an infinite population was used \nto calculate variance of the total estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level),
          warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling variance \nestimator for an infinite population was used to calculate variance of the total \nestimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
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
          warn <- paste0("The local total variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite population \nwas used to calculate variance of the total estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local total variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the total estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA,
            warning = I(warn), action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <- localmean_var(total2est * wgt1_u, weight_lst) +
          sum(var2est * wgt1_u)
      }
    } else {
      varest <- 0
    }

    # End of section for a two-stage sample
  } else {
    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z)
    dind <- if (subset_local) rep(1, n) else subpop_ind
    n_eff <- if (subset_local) n else sum(subpop_ind)

    # Calculate the weighted residuals vector
    # (when subset_local = FALSE, x/y/z/wgt cover the full stratum rather
    # than domain members only, and dind zeroes the contribution of every
    # site outside the domain)

    rv_total <- dind * wgt * z

    # Adjust the variance estimator for small sample size

    if (n_eff < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the total estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = I(stratum_level),
          warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling variance estimator \nfor an infinite population was used to calculate variance of the total estimate.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Above the benchmarked large-n threshold, subset_local = FALSE still
    # proceeds as explicitly requested, but warns

    if (!subset_local && n > 2000) {
      warn_ind <- TRUE
      act <- "Proceeding as requested; this computation may take a long time and use a large amount of memory.\n"
      warn <- paste0("subset_local = FALSE requires building the local mean neighbor structure from all ", n, ifelse(stratum_ind, paste0(" sites in stratum \"", stratum_level, "\""), " sites in this design"), ", which exceeds the recommended threshold of 2,000 sites. This computation may take several minutes or longer and use several GB of memory.\n")
      warn_df <- rbind(warn_df, data.frame(
        func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
        indicator = warn_vec[3], stratum = if (stratum_ind) stratum_level else NA,
        warning = I(warn), action = I(act)
      ))
    }

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x, y, prb = 1 / wgt)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local total variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the total estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local total variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the total estimate.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <- localmean_var(rv_total, weight_lst)
      }
    } else {
      varest <- 0
    }

    # End of section for a single-stage sample
  }

  # Return the variance estimate, the warning message indicator, and the warn_df
  # data frame

  list(
    vartype = vartype, varest = varest, warn_ind = warn_ind,
    warn_df = warn_df
  )
}
