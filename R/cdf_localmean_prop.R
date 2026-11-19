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
#' @param subset_local If \code{TRUE}, construct neighborhoods using only
#'   domain members. The internal default, \code{FALSE}, retains eligible
#'   responding sites in the sampling stratum and zeros contributions outside
#'   the domain. Used only for local variance estimation.
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
                               warn_ind, warn_df, subset_local = FALSE, local_nbh = 8, local_context = NULL) {
  # Stratified ratios use the overall estimate and denominator. Summing
  # local variances of these contributions retains uncertainty in the
  # estimated stratum sizes, including for All Sites.

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

  # Time analyses may ignore a domain column that is also the stratum column.
  # The survey design retains the original sampling strata independently.
  if (!is.null(stratumID)) stratumID <- design$strata[[1]]

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

    if (cluster_ind && !subset_local && any(subpop_ind_full[tst_resp] == 0)) {
      response <- outer(contvar, cdfval, `<=`)
      full <- localmean_cluster_domain(design, response, subpop_ind_full,
        tst_resp, TRUE, design_names, warn_df, warn_vec,
        local_nbh, local_context, estimate = prop)
      sdest <- sqrt(full$covariance)
      stderr_P[isubpop, ] <- sdest
      rows <- paste(lev_itype[isubpop], seq_len(ncdfval), sep = ":")
      confval_P[rows, ] <- cbind(pmax(prop - mult * sdest, 0), pmin(prop + mult * sdest, 1))
      warn_df <- full$warn_df
      warn_ind <- warn_ind || full$warn_ind
      next
    }

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
      response <- outer(contvar, cdfval, `<=`)
      full <- localmean_ratio(design, response, subpop_ind_full,
        tst_resp, prop, design_names, warn_df, warn_vec,
        subset_local, local_nbh, local_context)
      sdest <- sqrt(full$covariance)
      stderr_P[isubpop, seq_along(sdest)] <- sdest
      rows <- paste(lev_itype[isubpop], seq_len(ncdfval), sep = ":")
      confval_P[rows, ] <- cbind(pmax(prop - mult * sdest, 0), pmin(prop + mult * sdest, 1))
      warn_df <- full$warn_df
      warn_ind <- warn_ind || full$warn_ind

      # Branch for an unstratified sample
    } else {
      # Calculate the standard error estimates

      if (cluster_ind) {
        temp <- cdfvar_prop(
          contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
          cdfval, prop, stratum_ind,
          if (is.null(stratumID)) NULL else stratum_levels[1], cluster_ind, clusterID[tst],
          wgt1[tst], xcoord1[tst], ycoord1[tst], warn_ind, warn_df, warn_vec,
          local_nbh = local_nbh, local_context = local_context)
      } else {
        # data_tst mirrors stratum_data_i above: domain-only under TRUE,
        # every responded site under FALSE, with subpop_ind_full marking
        # the real domain members. prop (the point estimate) is
        # unaffected by subset_local.
        data_tst <- if (subset_local) tst else if (!is.null(stratumID)) {
          tst_resp & stratumID == stratum_levels[1]
        } else tst_resp
        temp <- cdfvar_prop(contvar[data_tst], wgt[data_tst], xcoord[data_tst],
          ycoord[data_tst], cdfval, prop, stratum_ind,
          if (is.null(stratumID)) NULL else stratum_levels[1], cluster_ind,
          warn_ind = warn_ind,
          warn_df = warn_df, warn_vec = warn_vec, subset_local = subset_local,
          subpop_ind = subpop_ind_full[data_tst],
          local_nbh = local_nbh, local_context = local_context)
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
