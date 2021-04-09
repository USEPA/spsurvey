###############################################################################
# Function: changevar_prop (not exported)
# Programmer: Tom Kincaid
# Date: July 22, 2020
#'
#' Covariance or Correlation Matrix Estimate of Change in Class Proportions
#' between Two Surveys
#'
#' This function uses the repeat visit sites for two probability surveys to
#' calculate either covariance or correlation estimates of estimated change in
#' the  proportion in each of a set of categories.  Covariance estimates are
#' calculated when the resivit sites have the same survey design weight in both
#' surveys.  Correlation estimates are calculated when the revisit sites do not
#' have the same weight in both surveys, in which case the sites are assigned
#' equal weights.  The revisitwgt argument controls whether covariance or
#' correlation estimates are calculated.  Either the simple random sampling
#' (SRS) variance/covariance estimator or the local mean variance/covariance
#' estimator is calculated, which is subject to user control.  The simple random
#' sampling variance/covariance estimator uses the independent random sample
#' approximation to calculate joint inclusion probabilities.  The function can
#' accomodate single-stage and two-stage samples.  Finite population and
#' continuous population correction factors can be utilized in variance
#' estimation.
#'
#' @param catvar_levels Vector of the set of categorical response values.
#'
#' @param catvar1 Vector of the response value for each site for survey one.
#'
#' @param catvar2 Vector of the response value for each site for survey two.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
#'  inclusion probability) for each site, which is either the weight for a
#'  single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'  the x- coordinate for a single-stage sample or the stage two x-coordinate
#'  for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'  the y- coordinate for a single-stage sample or the stage two y-coordinate
#'  for a two-stage sample.
#'
#' @param revisitwgt Logical value that indicates whether each repeat visit
#'  site has the same survey design weight in the two surveys, where \code{TRUE} = the
#'  weight for each repeat visit site is the same and \code{FALSE} = the weight for
#'  each repeat visit site is not the same.  When this argument is \code{FALSE}, all
#'  of the repeat visit sites are assigned equal weights when calculating the
#'  covariance component of the change estimate standard error.
#'
#' @param prop1 The set of category proportion estimates for survey one.
#'
#' @param prop2 The set of category proportion estimates for survey two.
#'
#' @param stratum_ind Logical value that indicates whether the sample is
#'  stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not a stratified
#'  sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'  two- stage sample, where \code{TRUE} = a two-stage sample and \code{FALSE} = not a
#'  two-stage sample.
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
#' @param pcfactor_ind Logical value that indicates whether the finite
#'  population correction factor is used during variance estimation, where \code{TRUE}
#'  = use the population correction factor and \code{FALSE} = do not use the factor.
#'  To employ the correction factor for a single-stage sample, a value must be
#'  supplied for argument \code{fpcsize}.  To employ the correction factor for a
#'  two-stage sample, values must be supplied for arguments \code{Ncluster} and
#'  \code{stage1size}.
#'
#' @param fpcsize Size of the resource, which is required for calculation of the
#'  finite population correction factor for a single-stage sample.
#'
#' @param Ncluster The number of stage one sampling units in the resource,
#'  which is required for calculation of the finite population correction
#'  factor for a two-stage sample.
#'
#' @param stage1size Vector of the size of the stage one sampling units of a
#'  two-stage sample, which is required for calculation of the finite
#'  population correction factor for a two-stage sample.
#'
#' @param vartype The choice of variance estimator, where \code{"Local"} = local mean
#'  estimator and \code{"SRS"} = SRS estimator.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'  generated, where \code{TRUE} = warning messages were generated and \code{FALSE} = warning
#'  messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'  subpopulation, and an indicator.
#'
#' @return An object in list format composed of a vector named \code{rslt}, which
#'  contains the covariance or correlation estimates, a logical variable named
#'  \code{warn_ind}, which is the indicator for warning messges, and a data frame
#'  named \code{warn_df}, which contains warning messages.
#'
#' @section Other Functions Required:
#'  \describe{
#'  \item{\code{\link{localmean_weight}}}{calculate the weighting matrix for
#'   the local mean variance estimator}
#'  \item{\code{\link{localmean_cov}}}{calculate the variance/covariance
#'   matrix using the local mean estimator}
#'  }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

changevar_prop <- function(catvar_levels, catvar1, catvar2, wgt, x, y,
                           revisitwgt, prop1, prop2, stratum_ind, stratum_level, cluster_ind, clusterID,
                           wgt1, x1, y1, pcfactor_ind, fpcsize, Ncluster, stage1size, vartype, warn_ind,
                           warn_df, warn_vec) {

  # Assign the function name

  fname <- "changevar_prop"

  #
  # Calculate covariance or correlation using the repeat visit sites
  #

  # Begin the section for a two-stage sampl

  if (cluster_ind) {

    # Calculate additional required values

    m <- length(catvar_levels)
    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    catvar1_1st <- split(catvar1, cluster)
    catvar2_1st <- split(catvar2, cluster)
    wgt2_1st <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    tw2 <- (sum(wgt1 * wgt))^2
    if (vartype == "Local") {
      x2_1st <- split(x, cluster)
      y2_1st <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    if (pcfactor_ind) {
      N_cluster <- unique(Ncluster)
      stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # Loop through each category level

    rslt <- rep(NA, m)
    for (k in 1:m) {

      # Determine whether the categorical level is present in both surveys

      if (is.na(prop1[k]) | is.na(prop2[k])) {
        warn_ind <- TRUE
        act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
        if (stratum_ind) {
          warn <- paste("Category level \"", catvar_levels[k], "\" in stratum \"", stratum_level, "\" \nwas not present among the repeat visit sites in one of the surveys.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste("Category level \"", catvar_levels[k], "\" was not present among the repeat visit sites \nin one of the surveys.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        next
      }

      # Calculate estimates of the total of the stage two sampling unit residuals
      # and the variance/covariance of those totals for each stage one sampling unit

      total2est <- matrix(0, ncluster, 2)
      var2est <- matrix(0, ncluster, 4)
      phat <- c(prop1[k], prop2[k])
      for (i in 1:ncluster) {

        # Calculate the weighted residuals matrix

        n <- length(catvar1_1st[[i]])
        z1 <- catvar1_1st[[i]] == catvar_levels[k]
        z2 <- catvar2_1st[[i]] == catvar_levels[k]
        rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow = n, byrow = TRUE)) *
          matrix(rep(wgt2_1st[[i]], 2), nrow = n)

        # Calculate total estimates for the stage one sampling unit

        total2est[i, ] <- apply(rm, 2, sum)

        # Adjust the variance/covariance estimator for small sample size

        SRSind <- FALSE
        if (vartype == "Local" && n < 4) {
          warn_ind <- TRUE
          act <- "The simple random sampling variance estimator was used.\n"
          if (stratum_ind) {
            warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate covariance of the estimate.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = I(stratum_level),
              warning = I(warn), action = I(act)
            ))
          } else {
            warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep = "")
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

        pcfactor <- ifelse(pcfactor_ind, (stage1size_u[i] - n) / stage1size_u[i],
          1
        )

        # Calculate variance/covariance estimates for the stage one sampling unit

        if (var_ind[i]) {
          if (vartype == "Local") {
            weight_1st <- localmean_weight(
              x2_1st[[i]], y2_1st[[i]],
              1 / wgt2_1st[[i]]
            )
            var2est[i, ] <- as.vector(pcfactor * localmean_cov(rm, weight_1st))
          } else {
            var2est[i, ] <- as.vector(pcfactor * n * var(rm))
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
          warn <- paste("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate covariance of the estimate.\n", sep = "")
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

      # Calculate the covariance or correlation estimates

      if (vartype == "Local") {
        weight_1st <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
        varest <- (pcfactor * localmean_cov(total2est * matrix(rep(wgt1_u, 2),
          nrow = ncluster
        ), weight_1st) + matrix(apply(var2est *
          matrix(rep(wgt1_u, 4), nrow = ncluster), 2, sum), nrow = 2)) / tw2
      } else {
        varest <- (pcfactor * ncluster * var(total2est * matrix(rep(wgt1_u, 2),
          nrow = ncluster
        )) + matrix(apply(var2est * matrix(rep(wgt1_u, 4),
          nrow = ncluster
        ), 2, sum), nrow = 2)) / tw2
      }
      if (revisitwgt) {
        rslt[k] <- varest[1, 2]
      } else {
        rslt[k] <- varest[1, 2] / sqrt(varest[1, 1] * varest[2, 2])
      }

      # End the loop for category levels
    }

    # End the section for a two-stage sample
  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    n <- length(catvar1)
    m <- length(catvar_levels)
    tw2 <- (sum(wgt))^2
    if (pcfactor_ind) {
      fpcsize_u <- unique(fpcsize)
    }

    # Loop through each category level

    rslt <- rep(NA, m)
    for (i in 1:m) {

      # Determine whether the categorical level is present in both surveys

      if (is.na(prop1[i]) | is.na(prop2[i])) {
        warn_ind <- TRUE
        act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
        if (stratum_ind) {
          warn <- paste("Category level \"", catvar_levels[i], "\" in stratum \"", stratum_level, "\" \nwas not present among the repeat visit sites in one of the surveys.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- paste("Category level \"", catvar_levels[i], "\" was not present among the repeat visit sites \nin one of the surveys.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        next
      }

      # Calculate the weighted residuals matrix

      z1 <- catvar1 == catvar_levels[i]
      z2 <- catvar2 == catvar_levels[i]
      phat <- c(prop1[i], prop2[i])
      rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow = n, byrow = TRUE)) *
        matrix(rep(wgt, 2), nrow = n)

      # Adjust the variance estimator for small sample size

      if (vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if (stratum_ind) {
          warn <- paste("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
            action = I(act)
          ))
        } else {
          warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate covariance of the estimate.\n"
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

      # Calculate covariance or correlation estimates

      if (vartype == "Local") {
        weight_1st <- localmean_weight(x, y, 1 / wgt)
        varest <- pcfactor * localmean_cov(rm, weight_1st) / tw2
      } else {
        varest <- pcfactor * n * var(rm) / tw2
      }
      if (revisitwgt) {
        rslt[i] <- varest[1, 2]
      } else {
        if (varest[1, 1] == 0 | varest[2, 2] == 0 | any(is.na(varest))) {
          warn_ind <- TRUE
          act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
          if (stratum_ind) {
            warn <- paste("The variance estimate for category level \"", catvar_levels[i], "\" \nin stratum \"", stratum_level, "\" was equal to zero for at least one of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = I(stratum_level), warning = I(warn),
              action = I(act)
            ))
          } else {
            warn <- paste("The variance estimate for category level \"", catvar_levels[i], "\" was equal to zero \nfor at least one of the surveys.\n", sep = "")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            ))
          }
          next
        }
        rslt[i] <- varest[1, 2] / sqrt(varest[1, 1] * varest[2, 2])
      }

      # End the loop for category levels
    }

    # End the section for a single-stage sample
  }

  # Return the covariance or correlation estimates, the warning message indicator,
  # and the warn_df data frame

  list(rslt = rslt, warn_ind = warn_ind, warn_df = warn_df)
}
