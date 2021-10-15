################################################################################
# Function: percentile_est (not exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: May 20, 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
# Revised: June 11, 2021 to eliminate estimating means, which will be performed
#          by a new function named mean_est
# Revised: September 9, 2021 to remove argument vartype, which is not used
# Revised: September 17 2021 to set argument ties equal to "rounded" when
#          calling function oldsvyquantile in order to ensure backward
#          compatability with spsurvey prior to version 5.0.0
# Revised: October 12, 2021 to correct an error that occurs when assigning
#          results for the case where one or both subpopulations contain a
#          single value
#
#' Percentile Estimates for Probability Survey Data
#'
#' This function calculates percentile estimates using the
#' \code{oldsvyquantile()} function in the survey package (\code{svyquantile()}
#' on survey version pre 4.1-1).  Upper and lower confidence bounds also are
#' estimated.
#'
#' @param pctsum Data frame containing estimates.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
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
#' @param ivar Character value that identifies the response variable.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the \code{design} argument.
#'
#' @param var_nondetect Character value that identifies the name of a logical
#'   variable in the \code{dframe} data frame specifying the presence of not
#'   detected (nondetect) values for the response variable.
#'
#' @param conf Numeric value for the confidence level.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param pctval  Vector of the set of values at which percentiles are
#'   estimated.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @return A list composed of the following objects:
#'   \itemize{
#'     \item{\code{pctsum}}{data frame containing the percentile estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey univar
#'
#' @noRd
################################################################################

percentile_est <- function(pctsum, dframe, itype, lev_itype, nlev_itype, ivar,
                           design, design_names, var_nondetect, conf, mult,
                           pctval, warn_ind, warn_df) {

  # Assign a value to the function name variable

  fname <- "percentile_est"

  # Assign the number of perentile values

  npctval <- length(pctval)

  #
  # Calculate percentile estimates
  #

  if (is.null(var_nondetect)) {

    # Calculate percentile estimates, standard error estimates, and confidence
    # bound estimates for each combination of subpopulation and response
    # variable for the case where nondetects are not present

    tst <- !is.na(dframe[, itype])
    if (nlev_itype == 1) {
      nresp <- sum(!is.na(dframe[tst, ivar]))
      if (nresp == 1) {
        warn_ind <- TRUE
        act <- "Percentiles were not calculated.\n"
        warn <- paste0("Percentile estimates were not calculated for subpopulation type \"", itype, "\" \nsince the number of non-missing response values equals one.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = itype, subpop = NA, indicator = ivar,
          stratum = NA, warning = I(warn), action = I(act)
        ))
        pctest <- rep(NA, npctval)
        nresp <- rep(NA, npctval)
        stderr <- rep(NA, npctval)
        lbound <- rep(NA, npctval)
        ubound <- rep(NA, npctval)
      } else {
        options(warn = -1)
        rslt_svy <- oldsvyquantile(make.formula(ivar),
          design = subset(design, tst), quantiles = pctval / 100,
          alpha = (100 - conf) / 100, ci = TRUE, na.rm = TRUE, ties = "rounded"
        )
        options(warn = 0)
        pctest <- rslt_svy$quantiles
        nresp <- cdf_nresp(dframe[, ivar], as.vector(pctest))
        stderr <- SE(rslt_svy)
        temp <- confint(rslt_svy)
        lbound <- temp[, 1]
        ubound <- temp[, 2]
      }
    } else {
      pctest <- array(NA, c(nlev_itype, npctval))
      nresp <- array(NA, c(nlev_itype, npctval))
      stderr <- array(NA, c(nlev_itype, npctval))
      lbound <- array(NA, c(nlev_itype, npctval))
      ubound <- array(NA, c(nlev_itype, npctval))
      nval <- tapply(dframe[tst, ivar], dframe[tst, itype], function(x) {
        sum(!is.na(x))
      })
      subpop_ind <- nval > 1
      if (any(!subpop_ind)) {
        temp_str <- vecprint(lev_itype[!subpop_ind])
        warn_ind <- TRUE
        act <- "Percentiles were not calculated.\n"
        warn <- paste0("Percentile estimates were not calculated for the following subpopulations of \nsubpopulation type \"", itype, "\" since the number of non-missing response values \nequals one for each subpopulation:\n", temp_str)
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = itype, subpop = NA, indicator = ivar, stratum = NA,
          warning = I(warn), action = I(act)
        ))
      }
      if (any(subpop_ind)) {
        tst <- tst & dframe[, itype] %in% lev_itype[subpop_ind]
        levs <- (1:nlev_itype)[subpop_ind]
        options(warn = -1)
        rslt_svy <- svyby(make.formula(ivar), make.formula(itype),
          design = subset(design, tst), oldsvyquantile,
          quantiles = pctval / 100, alpha = (100 - conf) / 100, ci = TRUE,
          na.rm = TRUE, ties = "rounded"
        )
        options(warn = 0)
        j <- 1
        for (i in levs) {
          pctest[i, ] <- unlist(rslt_svy[j, 2:(npctval + 1)])
          j <- j + 1
        }
        for (i in levs) {
          ind <- dframe[, itype] == lev_itype[i]
          nresp[i, ] <- cdf_nresp(dframe[ind, ivar], pctest[i, ])
        }
        rownames(nresp) <- lev_itype
        temp <- SE(rslt_svy)
        j <- 1
        for (i in levs) {
          stderr[i, ] <- unlist(temp[j, 1:npctval])
          j <- j + 1
        }
        temp <- confint(rslt_svy)
        j <- 1
        for (i in levs) {
          lbound[i, ] <- temp[seq(j, by = length(levs), length = npctval), 1]
          ubound[i, ] <- temp[seq(j, by = length(levs), length = npctval), 2]
          j <- j + 1
        }
        rownames(lbound) <- lev_itype
        rownames(ubound) <- lev_itype
      }
    }
  } else {
    # To be implemented
  }

  # Assign identifiers and estimates to the pctsum data frame

  if (is.null(var_nondetect)) {
    if (nlev_itype == 1) {
      pctsum <- rbind(pctsum, data.frame(
        Type = itype,
        Subpopulation = lev_itype,
        Indicator = ivar,
        Statistic = paste0(pctval, "Pct"),
        nResp = nresp,
        Estimate = pctest[1],
        StdError = stderr,
        MarginofError = mult * stderr,
        LCB = lbound,
        UCB = ubound
      ))
    } else {
      for (i in 1:nlev_itype) {
        pctsum <- rbind(pctsum, data.frame(
          Type = itype,
          Subpopulation = lev_itype[i],
          Indicator = ivar,
          Statistic = paste0(pctval, "Pct"),
          nResp = nresp[i, ],
          Estimate = pctest[i, ],
          StdError = stderr[i, ],
          MarginofError = mult * stderr[i, ],
          LCB = lbound[i, ],
          UCB = ubound[i, ]
        ))
      }
    }
  } else {
    # To be implemented
  }

  # Return the pctsum data frame, the warn_ind logical value, and the warn_df
  # data frame

  list(pctsum = pctsum, warn_ind = warn_ind, warn_df = warn_df)
}
