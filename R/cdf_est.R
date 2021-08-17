################################################################################
# Function: cdf_est (not exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: May 25 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Cumulative Distribution Function Estimate for Probability Survey Data
#'
#' This function calculates an estimate of the cumulative distribution function
#' (CDF) for the proportion (expressed as percent) and the total of a
#' quantitative response variable, where the response variable may be defined
#' for either a finite or an extensive resource.  In addition, the standard
#' error of the estimated CDF and confidence bounds are calculated.  For the
#' CDF of a proportion, the Horvitz-Thompson ratio estimator, i.e., the ratio
#' of two Horvitz-Thompson estimators, is used to calculate the CDF estimate.
#' For the CDF of a total, the classic ratio estimator is used to calculate the
#' CDF estimate, where that estimator is the product of the known value and the
#' Horvitz-Thompson ratio estimator.  Variance estimates for the estimated CDF
#' are calculated using either the local mean variance estimator or the simple
#' random sampling (SRS) variance estimator.  The choice of variance estimator
#' is subject to user control. The local mean variance estimator requires the
#' x-coordinate and the y-coordinate of each site.  The SRS variance estimator
#' uses the independent random sampling approximation to calculate joint
#' inclusion probabilities.  Confidence bounds are calculated using a Normal
#' distribution multiplier.  The function can accommodate a stratified sample.
#' For a stratified sample, separate estimates and standard errors are
#' calculated for each stratum, which are used to produce estimates and standard
#' errors for all strata combined. The function can accommodate single-stage and
#' two-stage samples for both stratified and unstratified sampling designs.  The
#' finite population correction factor can be utilized in variance estimation.
#'
#' @param cdfsum Data frame containing estimates.
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
#' @param ivar Character value that identifies a factor variable in the design
#'   argument containing categorical response values.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
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
#'   \itemize{
#'     \item{\code{cdfsum}}{data frame containing the CDF estimate}
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

cdf_est <- function(cdfsum, dframe, itype, lev_itype, nlev_itype, ivar, design,
                    design_names, var_nondetect, vartype, conf, mult, warn_ind,
                    warn_df) {

  # Assign a value to the function name variable

  fname <- "cdf_est"

  #
  # Calculate CDF estimates
  #

  if (is.null(var_nondetect)) {

    # Using the proportion scale, calculate CDF estimates, standard error
    # estimates, and confidence bound estimates for each combination of
    # subpopulation and response variable for the case where nondetects are not
    # present

    tst <- !is.na(dframe[, itype])
    cdfval <- sort(unique(dframe[!is.na(dframe[, ivar]), ivar]))
    ncdfval <- length(cdfval)
    if (nlev_itype == 1) {
      rslt_svy <- lapply(cdfval, function(x) {
        svymean(make.formula(paste0("I(", ivar, " <= ", x, ")")),
          design = subset(design, tst), na.rm = TRUE
        )
      })
      cdfest_P <- sapply(rslt_svy, function(x) x[2])
      cdfest_P <- as.data.frame(t(cdfest_P))
      nresp <- list(cdf_nresp(dframe[, ivar], cdfval))
      if (vartype == "Local") {
        temp <- cdf_localmean_prop(
          itype, lev_itype, nlev_itype, ivar, design, design_names, cdfval,
          ncdfval, cdfest_P, mult, warn_ind, warn_df
        )
        stderr_P <- temp$stderr_P
        confval_P <- temp$confval_P
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_P <- sapply(rslt_svy, function(x) SE(x)[2])
        stderr_P <- as.data.frame(t(stderr_P))
        temp <- sapply(rslt_svy, function(x) confint(x, level = conf / 100))
        confval_P <- t(temp[c(2, 4), ])
      }
    } else {
      rslt_svy <- lapply(cdfval, function(x) {
        svyby(make.formula(paste0("I(", ivar, " <= ", x, ")")),
          make.formula(itype),
          design = subset(design, tst), svymean,
          na.rm = TRUE
        )
      })
      cdfest_P <- sapply(rslt_svy, function(x) x[, 3])
      rownames(cdfest_P) <- lev_itype
      nresp <- tapply(dframe[, ivar], dframe[, itype], cdf_nresp, cdfval)
      if (vartype == "Local") {
        temp <- cdf_localmean_prop(
          itype, lev_itype, nlev_itype, ivar, design, design_names, cdfval,
          ncdfval, cdfest_P, mult, warn_ind, warn_df
        )
        stderr_P <- temp$stderr_P
        confval_P <- temp$confval_P
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_P <- sapply(rslt_svy, function(x) SE(x)[, 2])
        rownames(stderr_P) <- lev_itype
        temp <- lapply(rslt_svy, function(x) {
          confint(x, level = conf / 100)[
            (nlev_itype + 1):(2 * nlev_itype),
          ]
        })
        confval_P <- NULL
        for (i in 1:length(temp)) {
          confval_P <- rbind(confval_P, temp[[i]])
        }
      }
    }

    # Using the total scale, calculate CDF estimates, standard error estimates,
    # and confidence bound estimates for each combination of subpopulation and
    # response variable for the case where nondetects are not present

    if (nlev_itype == 1) {
      rslt_svy <- lapply(cdfval, function(x) {
        svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
          design = subset(design, tst), na.rm = TRUE
        )
      })
      confest_U <- sapply(rslt_svy, function(x) x[2])
      confest_U <- as.data.frame(t(confest_U))
      if (vartype == "Local") {
        temp <- cdf_localmean_total(
          itype, lev_itype, nlev_itype, ivar, design, design_names, cdfval,
          ncdfval, confest_U, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_U <- sapply(rslt_svy, function(x) SE(x)[2])
        stderr_U <- as.data.frame(t(stderr_U))
        temp <- sapply(rslt_svy, function(x) confint(x, level = conf / 100))
        confval_U <- t(temp[c(2, 4), ])
      }
    } else {
      rslt_svy <- lapply(cdfval, function(x) {
        svyby(make.formula(paste0("I(", ivar, " <= ", x, ")")),
          make.formula(itype),
          design = subset(design, tst), svytotal,
          na.rm = TRUE
        )
      })
      confest_U <- sapply(rslt_svy, function(x) x[, 3])
      rownames(confest_U) <- lev_itype
      if (vartype == "Local") {
        temp <- cdf_localmean_total(
          itype, lev_itype, nlev_itype, ivar, design, design_names, cdfval,
          ncdfval, confest_U, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_U <- sapply(rslt_svy, function(x) SE(x)[, 2])
        rownames(stderr_U) <- lev_itype
        temp <- lapply(rslt_svy, function(x) {
          confint(x, level = conf / 100)[
            (nlev_itype + 1):(2 * nlev_itype),
          ]
        })
        confval_U <- NULL
        for (i in 1:length(temp)) {
          confval_U <- rbind(confval_U, temp[[i]])
        }
      }
    }
  } else {

    # Calculate CDF estimates, standard error estimates, and confidence bound
    # estimates for each combination of subpopulation and response variable for
    # the case where nondetects are present

    maxval <- max(dframe[, ivar], na.rm = TRUE)
    flip <- maxval - dframe[, ivar]
    event <- !dframe[, var_nondetect]
    if (nlev_itype == 1) {
      rslt_svy <- svykm(Surv(flip, event) ~ 1, design = subset(design, tst))
    }
  }

  # Assign identifiers and estimates to the cdfsum data frame

  if (is.null(var_nondetect)) {
    for (i in 1:nlev_itype) {
      indx <- seq(i, by = nlev_itype, length = ncdfval)
      cdfsum <- rbind(cdfsum, data.frame(
        Type = itype,
        Subpopulation = lev_itype[i],
        Indicator = ivar,
        Value = cdfval,
        nResp = nresp[[i]],
        Estimate.P = 100 * unlist(cdfest_P[i, ]),
        StdError.P = 100 * unlist(stderr_P[i, ]),
        MarginofError.P = 100 * (mult * unlist(stderr_P[i, ])),
        LCB.P = 100 * pmax(confval_P[indx, 1], 0),
        UCB.P = 100 * pmin(confval_P[indx, 2], 1),
        Estimate.U = unlist(confest_U[i, ]),
        StdError.U = unlist(stderr_U[i, ]),
        MarginofError.U = mult * unlist(stderr_U[i, ]),
        LCB.U = pmax(confval_U[indx, 1], 0),
        UCB.U = confval_U[indx, 2]
      ))
    }
  } else {
    # To be implemented
  }


  # Return the cdfsum data frame, the warn_ind logical value, and the warn_df
  # data frame

  list(cdfsum = cdfsum, warn_ind = warn_ind, warn_df = warn_df)
}
