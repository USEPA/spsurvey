################################################################################
# Function: catvar_prop (not exported)
# Programmer: Tom Kincaid
# Date: May 7, 2020
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: May 3 2021 to correct an error that occurs when warning messages for
#          unstratified samples are added to the warn_df data frame
# Revised: June 8 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Calculation of Local Mean Variance Estimates of Estimated Proportions for
#' Categorical Data
#'
#' This function calculates local mean variance estimates of the estimated
#' proportion in each of a set of categories.
#'
#' @param z Vector of the value of the categorical response variable or the
#'   site status for each site.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of the x-coordinate for location for each site, which is
#'   either the x-coordinate for a single-stage sample or the stage two
#'   x-coordinate for a two-stage sample.
#'
#' @param y Vector of the y-coordinate for location for each site, which is
#'   either the y-coordinate for a single-stage sample or the stage two
#'   y-coordinate for a two-stage sample.
#'
#' @param prop Vector of the proportion estimates.
#'
#' @param prop_names Vector of the category names for the response variable plus
#'   \code{"Total"}.
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two-stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} =
#'   not a two-stage sample.
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
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the subpopulation type, the
#'   subpopulation, and the response variable.
#'
#' @param subset_local Logical value indicating whether the local mean
#'   neighbor structure is built from domain members only (\code{TRUE},
#'   the default behavior) or from all sites in the stratum
#'   with the residual zeroed out for non-domain members via
#'   \code{subpop_ind} (\code{FALSE}). The default value is \code{TRUE}.
#'
#' @param subpop_ind Numeric 0/1 vector, the same length as \code{z}, that
#'   is 1 for members of the subpopulation (domain, in classical survey
#'   sampling terminology) and 0 otherwise. Only used when
#'   \code{subset_local} is \code{FALSE}.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{vartype}}{character variable containing the type of variance
#'       estimator}
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#'
#' @author Tom Kincaid \email{kincaid.tom@@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

catvar_prop <- function(z, wgt, x, y, prop, prop_names, stratum_ind,
                        stratum_level, cluster_ind, clusterID, wgt1, x1, y1,
                        warn_ind, warn_df, warn_vec, subset_local = TRUE,
                        subpop_ind = NULL) {
  # This is the local mean (spatial neighborhood) variance estimator (see
  # mean_var() for the underlying method) generalized to a vector of m
  # category proportions at once: for each category, membership is
  # represented as a 0/1 indicator, and the weighted residual (indicator
  # minus that category's estimated proportion) plays the same role that
  # wgt * (z - mean_est) does in mean_var(). Each category's residual
  # column is passed through localmean_var() independently (not as a joint
  # covariance), then divided by squared total weight to convert a total's
  # variance into a proportion's variance.

  # Assign the function name

  fname <- "catvar_prop"

  # Assign the variance type

  vartype <- "Local"

  # Remove zero values from the prop vector

  prop_names <- prop_names[prop > 0]
  prop <- prop[prop > 0]

  #
  # Branch to handle two-stage and single-stage samples
  #

  if (cluster_ind) {
    # Begin the section for a two-stage sample

    # Calculate additional required values

    m <- length(prop)
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
    tw2 <- (sum(wgt1 * wgt))^2
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # For each stage one sampling unit and each category, calculate an estimate
    # of the total of the stage two sampling unit residuals and the variance of
    # the total

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m)
    for (i in 1:ncluster) {
      # Calculate the weighted residuals matrix
      # im: n x m one-hot indicator matrix, im[j, k] = TRUE iff site j's
      # response falls in category k. rm subtracts the category proportion
      # from each indicator column and multiplies by the site weight, so
      # column k of rm is the weighted residual for category k (analogous
      # to wgt * (z - mean_est) for a single continuous variable).

      n <- length(z_lst[[i]])
      im <- tapply(wgt2_lst[[i]], z_lst[[i]]) ==
        matrix(rep(1:m, n), nrow = n, byrow = TRUE)
      rm <- (im - matrix(rep(prop, n), nrow = n, byrow = TRUE)) *
        matrix(rep(wgt2_lst[[i]], m), nrow = n)

      # Calculate the total estimate for each category

      total2est[i, ] <- apply(rm, 2, sum)

      # Adjust the variance estimator for small sample size

      if (var_ind[i] && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("There are less than four response values for stage one sampling unit \"", cluster_levels[i], "\"\nin stratum \"", stratum_level, "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the total of the residuals.\n")
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
              warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\" in stratum \"", stratum_level, "\", the simple random sampling \nvariance estimator for an infinite population was used to calculate variance of the \nthe total of the residuals.\n")
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
        warn <- paste0("There are less than four stage one sampling units in stratum \"", stratum_level, "\", the simple \nrandom sampling variance estimator for an infinite population was used to calculate \nvariance of the category proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("There are less than four stage one sampling units, the simple random sampling variance \nestimator for an infinite population was used to calculate variance of the category \nproportion estimates.\n")
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
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the category proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the category proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA,
            warning = I(warn), action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <- (
          apply(
            total2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2,
            localmean_var, weight_lst
          ) +
            apply(var2est * matrix(rep(wgt1_u, m), nrow = ncluster), 2, sum)
        ) / tw2
        names(varest) <- prop_names
      }
    } else {
      varest <- 0
    }

    # End of section for a two-stage sample
  } else {
    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(z)
    m <- length(prop)
    dind <- if (subset_local) rep(1, n) else subpop_ind
    n_eff <- if (subset_local) n else sum(subpop_ind)
    tw2 <- if (subset_local) (sum(wgt))^2 else (sum(wgt * subpop_ind))^2

    # Calculate the weighted residuals matrix
    # (see the two-stage section above for what im/rm represent: a one-hot
    # category-membership indicator matrix and the resulting per-category
    # weighted residuals, respectively). Under subset_local = TRUE, z only
    # ever contains domain members, so the tapply()-based indexing below is
    # safe and m and levels(z) always refer to the same (domain-only) set of
    # categories. Under subset_local = FALSE, z covers the full stratum,
    # which can contain categories with zero domain members and the
    # tapply() index numbering would then no longer line up with m (fixed
    # by the domain's own category count), so im is instead built by
    # matching category labels directly against prop_names. Consider replacing
    # with just outer() in the future.

    if (subset_local) {
      im <- tapply(wgt, z) == matrix(rep(1:m, n), nrow = n, byrow = TRUE)
    } else {
      im <- outer(as.character(z), prop_names, "==")
    }
    rm <- dind * (im - matrix(rep(prop, n), nrow = n, byrow = TRUE)) *
      matrix(rep(wgt, m), nrow = n)

    # Adjust the variance estimator for small sample size

    if (n_eff < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator for an infinite population was used.\n"
      if (stratum_ind) {
        warn <- paste0("There are less than four response values in stratum \"", stratum_level, "\", the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the category proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
          action = I(act)
        ))
      } else {
        warn <- paste0("\nThere are less than four response values, the simple random sampling variance estimator \nfor an infinite population was used to calculate variance of the category proportion \nestimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA, warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Above the benchmarked large-n threshold, subset_local = FALSE still
    # proceeds as explicitly requested but warns

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
      weight_lst <- localmean_weight(x, y, 1 / wgt)
      if (is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        if (stratum_ind) {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stratum \n\"", stratum_level, "\", the simple random sampling variance estimator for an infinite population \nwas used to calculate variance of the category proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = stratum_level, warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator for an infinite population was used to calculate variance \nof the category proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        varest <- 0
      } else {
        varest <- apply(rm, 2, localmean_var, weight_lst) / tw2
        names(varest) <- prop_names
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
