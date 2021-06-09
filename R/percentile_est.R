################################################################################
# Function: percentile_est (not exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: May 20 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Percentile Estimates for Probability Survey Data
#'
#' This function calculates percentile estimates using the svyquantile function
#' in the survey package.  In additon, the mean is estimated using the svymean
#' function in the survey package.  Upper and lower confidence bounds also are
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
#' @param vartype Character value providing the choice of the variance
#'   estimator, where "Local" = the local mean estimator, \code{"SRS"} = the
#'   simple random sampling estimator, \code{"HT"} = the Horvitz-Thompson
#'   estimator, and \code{"YG"} = the Yates-Grundy estimator.  The default value
#'   is \code{"Local"}.
#'
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
#'   \describe{
#'     \item{\code{pctsum}}{data frame containing the percentile estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{cdf_nresp}}{calculates number of response values less
#'       than a set of values}
#'     \item{\code{\link{SE}}}{extracts standard errors from a survey design
#'       object}
#'     \item{\code{\link{confint}}}{computes confidence intervals for a survey
#'       design object}
#'     \item{\code{mean_localmean}}{organizes input and output for
#'       calculation of the local mean variance estimator for the estimated
#'       mean}
#'     \item{\code{\link{svyby}}}{Compute survey statistics on subsets of a
#'       survey defined by factors}
#'     \item{\code{\link{svymean}}}{calculates the mean for a complex survey
#'       design}
#'     \item{\code{\link{svyquantile}}}{calculates percentile estimates for a
#'       complex survey design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @seealso
#'   \code{\link{confint}}
#'   \code{\link{SE}}
#'   \code{\link{svyby}}
#'   \code{\link{svymean}}
#'   \code{\link{svyquantile}}
#'
#' @keywords survey univar
#'
#' @noRd
################################################################################

percentile_est <- function(pctsum, dframe, itype, lev_itype, nlev_itype, ivar,
                           design, design_names, var_nondetect, vartype, conf,
                           mult, pctval, warn_ind, warn_df) {

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
          func = I(fname),
          subpoptype = itype, subpop = NA, indicator = ivar, stratum = NA,
          warning = I(warn), action = I(act)
        ))
        pctest <- rep(NA, npctval)
        nresp <- rep(NA, npctval)
        stderr <- rep(NA, npctval)
        lbound <- rep(NA, npctval)
        ubound <- rep(NA, npctval)
        temp <- mean(dframe[tst, ivar], na.rm = TRUE)
        meanest <- temp
        nresp_mean <- 1
        stderr_mean <- 0
        lbound_mean <- temp
        ubound_mean <- temp
      } else {
        options(warn = -1)
        rslt_svy <- svyquantile(make.formula(ivar),
          design = subset(design, tst), quantiles = pctval / 100,
          alpha = (100 - conf) / 100, ci = TRUE, na.rm = TRUE
        )
        options(warn = 0)
        pctest <- rslt_svy$quantiles
        nresp <- cdf_nresp(dframe[, ivar], as.vector(pctest))
        stderr <- SE(rslt_svy)
        temp <- confint(rslt_svy)
        lbound <- temp[, 1]
        ubound <- temp[, 2]
        rslt <- svymean(make.formula(ivar),
          design = subset(design, tst),
          na.rm = TRUE
        )
        meanest <- rslt
        nresp_mean <- sum(!is.na(dframe[, ivar]))
        if (vartype == "Local") {
          temp <- mean_localmean(
            itype, lev_itype, nlev_itype, c(1), ivar, design, design_names,
            meanest[1], mult, warn_ind, warn_df
          )
          stderr_mean <- temp$stderr
          lbound_mean <- unlist(temp$confval[1])
          ubound_mean <- unlist(temp$confval[2])
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df
        } else {
          stderr_mean <- SE(rslt)
          temp <- confint(rslt, level = conf / 100)
          lbound_mean <- temp[1]
          ubound_mean <- temp[2]
        }
      }
    } else {
      pctest <- array(NA, c(nlev_itype, npctval))
      nresp <- array(NA, c(nlev_itype, npctval))
      stderr <- array(NA, c(nlev_itype, npctval))
      lbound <- array(NA, c(nlev_itype, npctval))
      ubound <- array(NA, c(nlev_itype, npctval))
      meanest <- rep(NA, nlev_itype)
      nresp_mean <- rep(NA, nlev_itype)
      stderr_mean <- rep(NA, nlev_itype)
      lbound_mean <- rep(NA, nlev_itype)
      ubound_mean <- rep(NA, nlev_itype)
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
        levs <- (1:nlev_itype)[!subpop_ind]
        for (i in levs) {
          tst_mean <- tst & dframe[, itype] %in% lev_itype[i]
          temp <- mean(dframe[tst_mean, ivar], na.rm = TRUE)
          meanest[i] <- temp
          nresp_mean[i] <- 1
          stderr_mean[i] <- 0
          lbound_mean[i] <- temp
          ubound_mean[i] <- temp
        }
      }
      if (any(subpop_ind)) {
        tst <- tst & dframe[, itype] %in% lev_itype[subpop_ind]
        levs <- (1:nlev_itype)[subpop_ind]
        options(warn = -1)
        rslt_svy <- svyby(make.formula(ivar), make.formula(itype),
          design = subset(design, tst), svyquantile, quantiles = pctval / 100,
          alpha = (100 - conf) / 100, ci = TRUE, na.rm = TRUE
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
        rslt <- svyby(make.formula(ivar), make.formula(itype),
          design = subset(design, tst), svymean, na.rm = TRUE
        )
        meanest[levs] <- rslt[, 2]
        temp <- tapply(dframe[, ivar], dframe[, itype], function(x) {
          sum(!is.na(x))
        })
        nresp_mean[levs] <- temp[levs]
        if (vartype == "Local") {
          temp <- mean_localmean(
            itype, lev_itype, nlev_itype, levs, ivar, design, design_names,
            meanest, mult, warn_ind, warn_df
          )
          stderr_mean[levs] <- temp$stderr[levs]
          lbound_mean[levs] <- unlist(temp$confval[levs, 1])
          ubound_mean[levs] <- unlist(temp$confval[levs, 2])
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df
        } else {
          stderr_mean[levs] <- SE(rslt)
          temp <- confint(rslt, level = conf / 100)
          lbound_mean[levs] <- temp[, 1]
          ubound_mean[levs] <- temp[, 2]
        }
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
        Statistic = c(paste0(pctval, "Pct"), "Mean"),
        nResp = c(nresp, nresp_mean),
        Estimate = c(pctest, meanest),
        StdError = c(stderr, stderr_mean),
        MarginofError = mult * c(stderr, stderr_mean),
        LCB = c(lbound, lbound_mean),
        UCB = c(ubound, ubound_mean)
      ))
    } else {
      for (i in 1:nlev_itype) {
        pctsum <- rbind(pctsum, data.frame(
          Type = itype,
          Subpopulation = lev_itype[i],
          Indicator = ivar,
          Statistic = c(paste0(pctval, "Pct"), "Mean"),
          nResp = c(nresp[i, ], nresp_mean[i]),
          Estimate = c(pctest[i, ], meanest[i]),
          StdError = c(stderr[i, ], stderr_mean[i]),
          MarginofError = mult * c(stderr[i, ], stderr_mean[i]),
          LCB = unlist(c(lbound[i, ], lbound_mean[i])),
          UCB = unlist(c(ubound[i, ], ubound_mean[i]))
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
