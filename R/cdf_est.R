################################################################################
# Function: cdf_est
# Programmer: Tom Kincaid
# Date: July 23, 2020
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
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param var_nondetect Character value that identifies the name of a logical
#'   variable in the dframe data frame specifying the presence of not detected
#'   (nondetect) values for the response variable.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor should be employed during variance estimation.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param conf Numeric value for the confidence level.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn.ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @return A list composed of the following objects:
#'   \describe{
#'     \item{\code{cdfsum}}{data frame containing the CDF estimate}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cdf_localmean_prop}}}{organizes input and output for
#'       calculation of the local mean variance estimator for the estimated CDF
#'       using the proportion scale}
#'     \item{\code{\link{cdf_localmean_total}}}{organizes input and output for
#'       calculation of the local mean variance estimator for the estimated CDF
#'       using the total scale}
#'     \item{\code{\link{cdf_nresp}}}{calculates number of response values less
#'       than a set of values}
#'     \item{\code{\link{SE}}}{extracts standard errors from a survey design
#'       object}
#'     \item{\code{\link{confint}}}{computes confidence intervals for a survey
#'       design object}
#'     \item{\code{\link{svyby}}}{Compute survey statistics on subsets of a
#'       survey defined by factors}
#'     \item{\code{\link{svymean}}}{calculates the mean for a complex survey
#'       design}
#'     \item{\code{\link{svytotal}}}{calculates the total for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cdf_localmean_prop}}
#'   \code{\link{cdf_localmean_total}}
#'   \code{\link{confint}}
#'   \code{\link{SE}}
#'   \code{\link{svyby}}
#'   \code{\link{svymean}}
#'   \code{\link{svytotal}}
#'
#' @keywords survey univar
#'
#' @export
################################################################################

cdf_est <- function(cdfsum, dframe, itype, lev_itype, nlev_itype, ivar, design,
  design_names, var_nondetect, popcorrect, vartype, conf, mult, warn.ind,
  warn.df) {

# Assign a value to the function name variable

  fname <- "cdf_est"

#
# Calculate CDF estimates
#

  if(is.null(var_nondetect)) {

# Using the proportion scale, calculate CDF estimates, standard error estimates,
# and confidence bound estimates for each combination of subpopulation and
# response variable for the case where nondetects are not present

    tst <- !is.na(dframe[, itype])
    cdfval <- sort(unique(dframe[!is.na(dframe[, ivar]), ivar]))
    ncdfval <- length(cdfval)
    if(nlev_itype == 1) {
      rslt.svy <- lapply(cdfval, function(x)
        svymean(make.formula(paste0("I(", ivar, " <= ", x, ")")),
                design = subset(design, tst), na.rm = TRUE))
      cdfest.P <- sapply(rslt.svy, function(x) x[2])
      cdfest.P <- as.data.frame(t(cdfest.P))
      nresp <- list(cdf_nresp(dframe[, ivar], cdfval))
      if(vartype == "Local") {
        temp <- cdf_localmean_prop(itype, lev_itype, nlev_itype, ivar,
          design, design_names, cdfval, ncdfval, cdfest.P, popcorrect, vartype,
          mult, warn.ind, warn.df)
        stderr.P <- temp$stderr.P
        confval.P <- temp$confval.P
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.P <- sapply(rslt.svy, function(x) SE(x)[2])
        stderr.P <- as.data.frame(t(stderr.P))
        temp <- sapply(rslt.svy, function(x) confint(x, level = conf/100))
        confval.P <- t(temp[c(2, 4), ])
      }
    } else {
      rslt.svy <- lapply(cdfval, function(x)
        svyby(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              make.formula(itype), design = subset(design, tst), svymean,
              na.rm = TRUE))
      cdfest.P <- sapply(rslt.svy, function(x) x[, 3])
      rownames(cdfest.P) <- lev_itype
      nresp <- tapply(dframe[, ivar], dframe[, itype], cdf_nresp, cdfval)
      if(vartype == "Local") {
        temp <- cdf_localmean_prop(itype, lev_itype, nlev_itype, ivar,
          design, design_names, cdfval, ncdfval, cdfest.P, popcorrect, vartype,
          mult, warn.ind, warn.df)
        stderr.P <- temp$stderr.P
        confval.P <- temp$confval.P
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.P <- sapply(rslt.svy, function(x) SE(x)[, 2])
        rownames(stderr.P) <- lev_itype
        temp <- lapply(rslt.svy, function(x) confint(x, level = conf/100)[
          (nlev_itype+1):(2*nlev_itype), ])
        confval.P <- NULL
        for(i in 1:length(temp)) {
          confval.P <- rbind(confval.P, temp[[i]])
        }
      }
    }

# Using the total scale, calculate CDF estimates, standard error estimates,
# and confidence bound estimates for each combination of subpopulation and
# response variable for the case where nondetects are not present

    if(nlev_itype == 1) {
      rslt.svy <- lapply(cdfval, function(x)
        svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
                 design = subset(design, tst), na.rm = TRUE))
      cdfest.U <- sapply(rslt.svy, function(x) x[2])
      cdfest.U <- as.data.frame(t(cdfest.U))
      if(vartype == "Local") {
        temp <- cdf_localmean_total(itype, lev_itype, nlev_itype, ivar,
          design, design_names, cdfval, ncdfval, cdfest.U, popcorrect, vartype,
          mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.U <- sapply(rslt.svy, function(x) SE(x)[2])
        stderr.U <- as.data.frame(t(stderr.U))
        temp <- sapply(rslt.svy, function(x) confint(x, level = conf/100))
        confval.U <- t(temp[c(2, 4), ])
      }
    } else {
      rslt.svy <- lapply(cdfval, function(x)
        svyby(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              make.formula(itype), design = subset(design, tst), svytotal,
              na.rm = TRUE))
      cdfest.U <- sapply(rslt.svy, function(x) x[, 3])
      rownames(cdfest.U) <- lev_itype
      if(vartype == "Local") {
        temp <- cdf_localmean_total(itype, lev_itype, nlev_itype, ivar,
          design, design_names, cdfval, ncdfval, cdfest.U, popcorrect, vartype,
          mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.U <- sapply(rslt.svy, function(x) SE(x)[, 2])
        rownames(stderr.U) <- lev_itype
        temp <- lapply(rslt.svy, function(x) confint(x, level = conf/100)[
          (nlev_itype+1):(2*nlev_itype), ])
        confval.U <- NULL
        for(i in 1:length(temp)) {
          confval.U <- rbind(confval.U, temp[[i]])
        }
      }
    }

  } else {

# Calculate CDF estimates, standard error estimates, and confidence bound
# estimates for each combination of subpopulation and response variable for the
# case where nondetects are present

    maxval <- max(dframe[, ivar], na.rm = TRUE)
    flip <- maxval - dframe[, ivar]
    event <- !dframe[, var_nondetect]
    if(nlev_itype == 1) {
      rslt.svy <- svykm(Surv(flip, event)~1, design = subset(design, tst))
    }
  }

# Assign identifiers and estimates to the cdfsum data frame

  if(is.null(var_nondetect)) {
    for(i in 1:nlev_itype) {
      indx <- seq(i, by = nlev_itype, length = ncdfval)
      cdfsum <- rbind(cdfsum, data.frame(
        Type = itype,
        Subpopulation = lev_itype[i],
        Indicator = ivar,
        Value = cdfval,
        nResp = nresp[[i]],
        Estimate.P = 100 * unlist(cdfest.P[i, ]),
        StdError.P = 100 * unlist(stderr.P[i, ]),
        MarginofError.P = 100 * (mult * unlist(stderr.P[i, ])),
        LCB.P = 100 * pmax(confval.P[indx, 1], 0),
        UCB.P = 100 * pmin(confval.P[indx, 2], 1),
        Estimate.U = unlist(cdfest.U[i, ]),
        StdError.U = unlist(stderr.U[i, ]),
        MarginofError.U = mult * unlist(stderr.U[i, ]),
        LCB.U = pmax(confval.U[indx, 1], 0),
        UCB.U = confval.U[indx, 2]))
    }

  } else {
# To be implemented
  }


# Return the cdfsum data frame, the warn.ind logical value, and the warn.df
# data frame

  list(cdfsum = cdfsum, warn.ind = warn.ind, warn.df = warn.df)
}