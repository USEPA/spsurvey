################################################################################
# Function: cdf_localmean_prop (not exported)
# Programmer: Tom Kincaid
# Date: July 9, 2020
# Revised: April 27, 2021 to check whether the local mean variance estimator
#          produced negative estimates and to use the SRS variance estimator
#          when that situation occurs
# Revised: June 8, 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Local Mean Variance Estimates of the Estimated CDF using the Proportion Scale
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for the estimated CDF using the proportion scale.
#'
#' @param itype Character value that identifies a factor variable in the design
#'   argument containing subpopulation (domain) values.
#'
#' @param lev_itype Character vector that provides levels of the subpopulation
#'   variable.
#'
#' @param nlev_itype Numeric value that provides the number of levels of the
#'   subpopulation variable.
#'
#' @param ivar Character value that identifies a factor variable in the design
#'   argument containing categorical response values.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param cdfval Vector of the set of values at which the CDF is estimated.
#'
#' @param ncdfval Numeric value containing length of the set of values at which
#'   the CDF is estimated.
#'
#' @param cdfest_P Object that provides CDF estimates on the proportion scale
#'   for the continuous response variable.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param subset_local Logical value indicating whether the local mean
#'   neighbor structure is built from domain members only (\code{TRUE},
#'   the default behavior) or from all sites in the relevant stratum
#'   (\code{FALSE}). The default value is
#'   \code{TRUE}.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{stderr_P}}{data frame containing standard error estimates}
#'     \item{\code{confval_P}}{data frame containing confidence bound estimates}
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

cdf_localmean_prop <- function(itype, lev_itype, nlev_itype, ivar, design,
                               design_names, cdfval, ncdfval, cdfest_P, mult,
                               warn_ind, warn_df, subset_local = TRUE) {
  # Parallels cat_localmean_prop(), but computes the local mean variance of
  # the CDF proportion estimate at each threshold in cdfval (rather than of
  # a fixed set of category proportions), via cdfvar_prop(). Per-stratum
  # variances are combined using the same population-size-share-weighted
  # formula as mean_localmean()/cat_localmean_prop().

  # Assign a value to the function name variable

  fname <- "cdf_localmean_prop"

  # For variables that exist in the design$variables data frame, assign survey
  # design variables

  dframe <- design$variables
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

  # Assign values to the continuous response variable vector, contvar

  contvar <- dframe[, ivar]

  # Assign a value to the indicator variable for a two-stage sample

  cluster_ind <- !is.null(clusterID)

  # Assign values to weight variables

  if (cluster_ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

  # Create the output data frames for standard error estimates and confidence
  # bound estimates

  stderr_P <- data.frame(array(0, c(nlev_itype, ncdfval)))
  rownames(stderr_P) <- lev_itype
  confval_P <- data.frame(array(0, c(nlev_itype * ncdfval, 2)))
  dimnames(confval_P) <- list(
    paste(rep(lev_itype, ncdfval),
      rep(1:ncdfval, rep(nlev_itype, ncdfval)),
      sep = ":"
    ),
    c("LCB", "UCB")
  )

  # Loop through all subpopulations

  for (isubpop in 1:nlev_itype) {
    tst <- !is.na(dframe[, ivar]) &
      (dframe[, itype] %in% lev_itype[isubpop])

    # tst_resp/subpop_ind_full support subset_local = FALSE, the same way
    # as in cdf_localmean_total() and tst_resp is every responded site
    # regardless of subpopulation, subpop_ind_full flags real domain
    # members. tst itself is untouched since it still determines which
    # strata this subpopulation touches.

    tst_resp <- !is.na(dframe[, ivar])
    subpop_ind_full <- as.numeric(dframe[, itype] %in% lev_itype[isubpop])

    # Assign values to the vector of CDF estimates, prop

    prop <- unlist(cdfest_P[isubpop, ])

    # Assign values to the warn_vec vector

    warn_vec <- c(itype, lev_itype[isubpop], ivar)

    # Assign a value to the indicator variable for a stratified sample

    stratum_ind <- !is.null(stratumID)

    # For a stratified design, determine whether the subpopulation contains a single
    # stratum

    if (stratum_ind) {
      stratum <- factor(stratumID[tst])
      stratum_levels <- levels(stratum)
      nstrata <- length(stratum_levels)
      if (nstrata == 1) {
        stratum_ind <- FALSE
      }
    }

    # Branch for a stratified sample

    if (stratum_ind) {
      # Calculate values required for weighting strata

      if (cluster_ind) {
        popsize_hat <- tapply(wgt1[tst] * wgt2[tst], stratum, sum)
        sum_popsize_hat <- sum(wgt1[tst] * wgt2[tst])
      } else {
        popsize_hat <- tapply(wgt[tst], stratum, sum)
        sum_popsize_hat <- sum(wgt[tst])
      }

      # Begin the subsection for individual strata

      for (i in 1:nstrata) {
        # Calculate CDF estimates for the stratum

        stratum_i <- tst & stratumID == stratum_levels[i]
        if (cluster_ind) {
          cdfest_st <- cdf_prop(
            contvar[stratum_i], wgt2[stratum_i], cdfval,
            cluster_ind, clusterID[stratum_i], wgt1[stratum_i]
          )
        } else {
          # cdfest_st is the domain's own CDF point estimate for this
          # stratum and is unaffected by subset_local, always computed from
          # domain members only (stratum_i), regardless of what feeds the
          # variance call below.
          cdfest_st <- cdf_prop(
            contvar[stratum_i], wgt[stratum_i], cdfval,
            cluster_ind
          )
        }

        # Calculate variance estimates

        if (cluster_ind) {
          # subset_local = FALSE is not yet supported for two-stage
          # designs (blocked upstream in cont_analysis()), so this branch
          # is intentionally left passing domain-only data
          temp <- cdfvar_prop(
            contvar[stratum_i], wgt2[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], cdfval, cdfest_st,
            stratum_ind, stratum_levels[i], cluster_ind, clusterID[stratum_i],
            wgt1[stratum_i], xcoord1[stratum_i], ycoord1[stratum_i], warn_ind,
            warn_df, warn_vec
          )
        } else {
          # stratum_data_i is the row mask actually fed to cdfvar_prop():
          # under subset_local = TRUE it's identical to stratum_i (domain
          # members only); under FALSE it widens to
          # every responded site in this stratum, and subpop_ind_full
          # tells cdfvar_prop() which of those rows are real domain
          # members so it can zero out the rest.
          stratum_data_i <- if (subset_local) {
            stratum_i
          } else {
            tst_resp & stratumID == stratum_levels[i]
          }
          temp <- cdfvar_prop(contvar[stratum_data_i], wgt[stratum_data_i],
            xcoord[stratum_data_i], ycoord[stratum_data_i], cdfval, cdfest_st,
            stratum_ind, stratum_levels[i], cluster_ind,
            warn_ind = warn_ind,
            warn_df = warn_df, warn_vec = warn_vec, subset_local = subset_local,
            subpop_ind = subpop_ind_full[stratum_data_i]
          )
        }
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
        if (any(temp$varest < 0)) {
          temp$vartype <- "SRS"
          warn_ind <- TRUE
          act <- "The simple random sampling variance estimator for an infinite population was used.\n"
          warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates in \nstratum \"", stratum_levels[i], "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the CDF proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_levels[i]),
            warning = I(warn), action = I(act)
          ))
        }
        if (temp$vartype == "SRS") {
          # the fallback estimate must cover only this stratum (stratum_i),
          # not the whole subpopulation (tst); this loop contributes one
          # stratum's variance at a time
          rslt_svy <- lapply(cdfval, function(x) {
            svymean(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              design = subset(design, stratum_i), na.rm = TRUE
            )
          })
          varest <- sapply(rslt_svy, function(x) SE(x)[2])^2
        } else {
          varest <- temp$varest
        }

        # Add estimates to the stderr_P data frame

        stderr_P[isubpop, ] <- stderr_P[isubpop, ] +
          ((popsize_hat[i] / sum_popsize_hat)^2) * varest

        # End the subsection for individual strata
      }

      # Begin the subsection for all strata combined

      # Add estimates to the data frames for results

      stderr_P[isubpop, ] <- sqrt(stderr_P[isubpop, ])
      lbound <- unlist(pmax(prop - mult * stderr_P[isubpop, ], 0))
      ubound <- unlist(pmin(prop + mult * stderr_P[isubpop, ], 1))
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep = ":")
      ind <- rownames(confval_P) %in% temp
      confval_P[ind, ] <- cbind(lbound, ubound)

      # End the subsection for all strata combined

      # Branch for an unstratified sample
    } else {
      # Calculate the standard error estimates

      if (cluster_ind) {
        # subset_local = FALSE is not yet supported for two-stage designs
        # (blocked upstream in cont_analysis()), so this branch is
        # intentionally left passing domain-only data
        temp <- cdfvar_prop(
          contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
          cdfval, prop, stratum_ind, NULL, cluster_ind, clusterID[tst],
          wgt1[tst], xcoord1[tst], ycoord1[tst], warn_ind, warn_df, warn_vec
        )
      } else {
        # data_tst mirrors stratum_data_i above: domain-only under TRUE,
        # every responded site under FALSE, with subpop_ind_full marking
        # the real domain members. prop (the point estimate) is
        # unaffected by subset_local.
        data_tst <- if (subset_local) tst else tst_resp
        temp <- cdfvar_prop(contvar[data_tst], wgt[data_tst], xcoord[data_tst],
          ycoord[data_tst], cdfval, prop, stratum_ind, NULL, cluster_ind,
          warn_ind = warn_ind,
          warn_df = warn_df, warn_vec = warn_vec, subset_local = subset_local,
          subpop_ind = subpop_ind_full[data_tst]
        )
      }
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      if (any(temp$varest < 0)) {
        temp$vartype <- "SRS"
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates, the \nsimple random sampling variance estimator for an infinite population was used to \ncalculate variance of the CDF proportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA,
          warning = I(warn), action = I(act)
        ))
      }
      if (temp$vartype == "SRS") {
        rslt_svy <- lapply(cdfval, function(x) {
          svymean(make.formula(paste0("I(", ivar, " <= ", x, ")")),
            design = subset(design, tst), na.rm = TRUE
          )
        })
        sdest <- sapply(rslt_svy, function(x) SE(x)[2])
      } else {
        sdest <- sqrt(temp$varest)
      }

      # Calculate confidence bounds

      lbound <- pmax(prop - mult * sdest, 0)
      ubound <- pmin(prop + mult * sdest, 1)

      # Add estimates to the data frames for results

      stderr_P[isubpop, ] <- sdest
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep = ":")
      ind <- rownames(confval_P) %in% temp
      confval_P[ind, ] <- cbind(lbound, ubound)
    }
  }

  # Return results

  list(
    stderr_P = stderr_P,
    confval_P = confval_P,
    warn_ind = warn_ind,
    warn_df = warn_df
  )
}
