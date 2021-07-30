################################################################################
# Function: mean_est (not exported)
# Programmer: Tom Kincaid
# Date: June 11, 2021
#
#' Mean Estimates for Probability Survey Data
#'
#' This function calculates mean estimates using the svymean function in the
#' survey package.  Upper and lower confidence bounds also are estimated.
#'
#' @param meansum Data frame containing estimates.
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
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @return A list composed of the following objects:
#'   \describe{
#'     \item{\code{meansum}}{data frame containing the percentile estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
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
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @seealso
#'   \code{\link{confint}}
#'   \code{\link{SE}}
#'   \code{\link{svyby}}
#'   \code{\link{svymean}}
#'
#' @keywords survey univar
#'
#' @noRd
################################################################################

mean_est <- function(meansum, dframe, itype, lev_itype, nlev_itype, ivar,
                     design, design_names, var_nondetect, vartype, conf, mult,
                     warn_ind, warn_df) {

  # Assign a value to the function name variable

  fname <- "mean_est"

  #
  # Calculate mean estimates
  #

  if (is.null(var_nondetect)) {

    # Calculate the mean estimate, standard error estimate, and confidence
    # bound estimates for each combination of subpopulation and response
    # variable for the case where nondetects are not present

    tst <- !is.na(dframe[, itype])
    if (nlev_itype == 1) {
      nresp <- sum(!is.na(dframe[tst, ivar]))
      if (nresp == 1) {
        warn_ind <- TRUE
        act <- "The mean estimate was not calculated.\n"
        warn <- paste0("The mean estimate was not calculated for subpopulation type \"", itype, "\" since \nthe number of non-missing response values equals one.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = itype, subpop = NA, indicator = ivar,
          stratum = NA, warning = I(warn), action = I(act)
        ))
        temp <- mean(dframe[tst, ivar], na.rm = TRUE)
        meanest <- temp
        nresp <- 1
        stderr <- 0
        lbound <- temp
        ubound <- temp
      } else {
        rslt <- svymean(
          make.formula(ivar),
          design = subset(design, tst), na.rm = TRUE
        )
        meanest <- rslt
        nresp <- sum(!is.na(dframe[, ivar]))
        if (vartype == "Local") {
          temp <- mean_localmean(
            itype, lev_itype, nlev_itype, c(1), ivar, design, design_names,
            meanest[1], mult, warn_ind, warn_df
          )
          stderr <- temp$stderr
          lbound <- unlist(temp$confval[1])
          ubound <- unlist(temp$confval[2])
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df
        } else {
          stderr <- SE(rslt)
          temp <- confint(rslt, level = conf / 100)
          lbound <- temp[1]
          ubound <- temp[2]
        }
      }
    } else {
      meanest <- rep(NA, nlev_itype)
      nresp <- rep(NA, nlev_itype)
      stderr <- rep(NA, nlev_itype)
      lbound <- rep(NA, nlev_itype)
      ubound <- rep(NA, nlev_itype)
      nval <- tapply(dframe[tst, ivar], dframe[tst, itype], function(x) {
        sum(!is.na(x))
      })
      subpop_ind <- nval > 1
      if (any(!subpop_ind)) {
        levs <- (1:nlev_itype)[!subpop_ind]
        for (i in levs) {
          tst_mean <- tst & dframe[, itype] %in% lev_itype[i]
          temp <- mean(dframe[tst_mean, ivar], na.rm = TRUE)
          meanest[i] <- temp
          nresp[i] <- 1
          stderr[i] <- 0
          lbound[i] <- temp
          ubound[i] <- temp
        }
      }
      if (any(subpop_ind)) {
        tst <- tst & dframe[, itype] %in% lev_itype[subpop_ind]
        levs <- (1:nlev_itype)[subpop_ind]
        rslt <- svyby(
          make.formula(ivar), make.formula(itype),
          design = subset(design, tst),
          svymean, na.rm = TRUE
        )
        meanest[levs] <- rslt[, 2]
        temp <- tapply(dframe[, ivar], dframe[, itype], function(x) {
          sum(!is.na(x))
        })
        nresp[levs] <- temp[levs]
        if (vartype == "Local") {
          temp <- mean_localmean(
            itype, lev_itype, nlev_itype, levs, ivar, design, design_names,
            meanest, mult, warn_ind, warn_df
          )
          stderr[levs] <- temp$stderr[levs]
          lbound[levs] <- unlist(temp$confval[levs, 1])
          ubound[levs] <- unlist(temp$confval[levs, 2])
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df
        } else {
          stderr[levs] <- SE(rslt)
          temp <- confint(rslt, level = conf / 100)
          lbound[levs] <- temp[, 1]
          ubound[levs] <- temp[, 2]
        }
      }
    }
  } else {
    # To be implemented
  }

  # Assign identifiers and estimates to the meansum data frame

  if (is.null(var_nondetect)) {
    if (nlev_itype == 1) {
      meansum <- rbind(meansum, data.frame(
        Type = itype,
        Subpopulation = lev_itype,
        Indicator = ivar,
        nResp = nresp,
        Estimate = meanest[1],
        StdError = stderr[1],
        MarginofError = mult * stderr[1],
        LCB = lbound,
        UCB = ubound
      ))
    } else {
      for (i in 1:nlev_itype) {
        meansum <- rbind(meansum, data.frame(
          Type = itype,
          Subpopulation = lev_itype[i],
          Indicator = ivar,
          nResp = nresp[i],
          Estimate = meanest[i],
          StdError = stderr[i],
          MarginofError = mult * stderr[i],
          LCB = unlist(lbound[i]),
          UCB = unlist(ubound[i])
        ))
      }
    }
  } else {
    # To be implemented
  }

  # Return the meansum data frame, the warn_ind logical value, and the warn_df
  # data frame

  list(meansum = meansum, warn_ind = warn_ind, warn_df = warn_df)
}
