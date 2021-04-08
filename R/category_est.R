################################################################################
# Function: category_est
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: February 9, 2021 to fix incorrect coding of "confval_P" as
#          "stderr_P".
# Revised: February 16, 2021 to correctly handle the case where the response
#          variable contains a single category.
# Revised: February 22, 2021 to fix incorrect coding of "rslt_T" as
#          "rslt_U".
#
#' Category Proportion and Total Estimates for Probability Survey Data
#'
#' This function calculates estimates of the proportion (expressed as percent)
#' and total (size) for each of the categories of a categorical response
#' variable.  Upper and lower confidence bounds also are estimated.  Proportion
#' estimates are calculated using the Horvitz-Thompson ratio estimator, i.e.,
#' the ratio of two Horvitz-Thompson estimators.  The numerator of the ratio
#' estimates the total of the category.  The denominator of the ratio estimates
#' the total of the resource. Variance estimates for the proportion estimates
#' are calculated using either the local mean variance estimator or the simple
#' random sampling (SRS) variance estimator.  The choice of variance estimator
#' is subject to user control. The local mean variance estimator requires the
#' x-coordinate and the y-coordinate of each site.  Confidence bounds are
#' calculated using a Normal distribution multiplier. For a finite resource,
#' total is the number of units in the resource.  For an extensive resource,
#' total is the measure (extent) of the resource, i.e., length, area, or volume.
#' Total estimates are calculated using the Horvitz-Thompson estimator.
#' Variance estimates for the total estimates are calculated using either the
#' local mean variance estimator or the SRS variance estimator.
#'
#' @param catsum Data frame containing estimates.
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
#' @param lev_ivar Character vector that provides levels of the categorical
#'   response variable.
#'
#' @param nlev_ivar Numeric value that provides the number of levels of the
#'   categorical response variable.
#'
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
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
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @return A list composed of the following objects:
#'   \describe{
#'     \item{\code{catsum}}{data frame containing the category estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cat_localmean_prop}}}{organizes input and output for
#'       calculation of the local mean variance estimator for estimated
#'       proportions for categorical data}
#'     \item{\code{\link{cat_localmean_total}}}{organizes input and output for
#'       calculation of the local mean variance estimator for estimated sizes
#'       (totals) for categorical data}
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
#'   \code{\link{cat_localmean_prop}}
#'   \code{\link{cat_localmean_total}}
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

category_est <- function(catsum, dframe, itype, lev_itype, nlev_itype, ivar,
                         lev_ivar, nlev_ivar, design, design_names, popcorrect, vartype, conf, mult,
                         warn_ind, warn_df) {

  # Assign a value to the function name variable

  fname <- "category_est"

  # Calculate category proportion estimates, standard error estimates, and
  # confidence bound estimates for each combination of subpopulation and response
  # variable

  tst <- !is.na(dframe[, itype])
  if (nlev_itype == 1) {
    if (nlev_ivar <= 1) {
      temp <- ifelse(all(is.na(dframe[, ivar])), 0.0, 1.0)
      rslt_P <- data.frame(ivar = temp, Total = temp)
      dimnames(rslt_P)[1] <- lev_itype
      stderr_P <- data.frame(ivar = 0.0, Total = 0.0)
      temp <- data.frame(LCB = temp, UCB = temp)
      confval_P <- rbind(ivar = temp, Total = temp)
    } else {
      rslt_svy <- svymean(make.formula(ivar),
        design = subset(design, tst),
        na.rm = TRUE
      )
      rslt_P <- cbind(data.frame(t(as.data.frame(rslt_svy)[1])),
        Total = 1.0
      )
      dimnames(rslt_P)[1] <- lev_itype
      if (vartype == "Local") {
        temp <- cat_localmean_prop(
          itype, lev_itype, nlev_itype, ivar, lev_ivar,
          nlev_ivar, design, design_names, rslt_P, popcorrect, vartype, mult,
          warn_ind, warn_df
        )
        stderr_P <- temp$stderr_P
        confval_P <- temp$confval_P
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_P <- cbind(t(data.frame(SE(rslt_svy))), Total = 0.0)
        confval_P <- confint(rslt_svy, level = conf / 100)
        tempdf <- data.frame(LCB = 1.0, UCB = 1.0)
        dimnames(tempdf) <- list("Total", colnames(confval_P))
        confval_P <- rbind(confval_P, tempdf)
      }
    }
  } else {
    if (nlev_ivar == 1) {
      ivar_splt <- split(dframe[, ivar], dframe[, itype])
      temp <- ifelse(sapply(ivar_splt, function(x) all(is.na(x))), 0.0, 1.0)
      rslt_P <- data.frame(ivar = temp, Total = temp)
      dimnames(rslt_P)[[1]] <- lev_itype
      stderr_P <- data.frame(ivar = rep(0.0, nlev_itype), Total = rep(
        0.0,
        nlev_itype
      ))
      confval_P <- NULL
      for (i in 1:nlev_itype) {
        tempdf <- rbind(data.frame(temp[i], temp[i]), data.frame(
          temp[i],
          temp[i]
        ))
        rownames(tempdf) <- c(
          paste(lev_itype[i], ivar, sep = ":"),
          paste(lev_itype[i], "Total", sep = ":")
        )
        colnames(tempdf) <- c("LCB", "UCB")
        confval_P <- rbind(confval_P, tempdf)
      }
    } else {
      rslt_svy <- svyby(make.formula(ivar), make.formula(itype),
        design = subset(design, tst), svymean, na.rm = TRUE
      )
      rslt_P <- rslt_svy[, 2:(nlev_ivar + 1)]
      rslt_P <- cbind(rslt_P, Total = rep(1, nlev_itype))
      if (vartype == "Local") {
        temp <- cat_localmean_prop(
          itype, lev_itype, nlev_itype, ivar, lev_ivar,
          nlev_ivar, design, design_names, rslt_P, popcorrect, vartype, mult,
          warn_ind, warn_df
        )
        stderr_P <- temp$stderr_P
        confval_P <- temp$confval_P
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        stderr_P <- cbind(SE(rslt_svy), Total = rep(0, nlev_itype))
        tempdf <- confint(rslt_svy, level = conf / 100)
        confval_P <- NULL
        for (i in 1:nlev_itype) {
          confval_P <- rbind(confval_P, tempdf[seq(i,
            by = nlev_itype,
            length = nlev_ivar
          ), ])
          temp <- data.frame(1.0, 1.0)
          colnames(temp) <- colnames(tempdf)
          confval_P <- rbind(confval_P, Total = temp)
        }
      }
    }
  }

  # Calculate category total estimates, standard error estimates, and
  # confidence bound estimates for each combination of subpopulation and response
  # variable

  if ("postStrata" %in% names(design)) {
    zzz <- ifelse(is.na(design$variables[, ivar]), NA, 1)
  } else {
    zzz <- ifelse(is.na(dframe[, ivar]), NA, 1)
  }
  design <- update(design, zzz = zzz)
  if (nlev_itype == 1) {
    if (nlev_ivar <= 1) {
      rslt_svy <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
      rslt_U <- data.frame(ivar = rslt_svy[1], Total = rslt_svy[1])
      dimnames(rslt_U)[1] <- lev_itype
      if (vartype == "Local") {
        temp <- cat_localmean_total(
          itype, lev_itype, nlev_itype, ivar,
          lev_ivar, nlev_ivar, design, design_names, rslt_U, popcorrect,
          vartype, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        temp <- SE(rslt_svy)
        stderr_U <- data.frame(ivar = as.vector(temp), Total = as.vector(temp))
        temp <- as.data.frame(confint(rslt_svy, level = conf / 100))
        confval_U <- rbind(ivar = temp, Total = temp)
      }
    } else {
      rslt_svy <- svytotal(make.formula(ivar),
        design = subset(design, tst),
        na.rm = TRUE
      )
      rslt_T <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
      rslt_U <- cbind(data.frame(t(as.data.frame(rslt_svy)[1])),
        Total = rslt_T[1]
      )
      dimnames(rslt_U)[1] <- lev_itype
      if (vartype == "Local") {
        temp <- cat_localmean_total(
          itype, lev_itype, nlev_itype, ivar,
          lev_ivar, nlev_ivar, design, design_names, rslt_U, popcorrect,
          vartype, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        temp <- SE(rslt_T)
        stderr_U <- cbind(t(data.frame(SE(rslt_svy))), Total = as.vector(temp))
        temp <- as.data.frame(confint(rslt_T, level = conf / 100))
        confval_U <- rbind(confint(rslt_svy, level = conf / 100), Total = temp)
      }
    }
  } else {
    if (nlev_ivar == 1) {
      rslt_svy <- svyby(~zzz, make.formula(itype),
        design = subset(design, tst),
        svytotal, na.rm = TRUE
      )
      rslt_U <- data.frame(ivar = rslt_svy$zzz, Total = rslt_svy$zzz)
      dimnames(rslt_U)[[1]] <- lev_itype
      if (vartype == "Local") {
        temp <- cat_localmean_total(
          itype, lev_itype, nlev_itype, ivar,
          lev_ivar, nlev_ivar, design, design_names, rslt_U, popcorrect,
          vartype, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        temp <- SE(rslt_svy)
        stderr_U <- data.frame(ivar = temp, Total = temp)
        temp <- as.data.frame(confint(rslt_svy, level = conf / 100))
        confval_U <- rbind(ivar = temp, Total = temp)
      }
    } else {
      rslt_svy <- svyby(make.formula(ivar), make.formula(itype),
        design = subset(design, tst), svytotal, na.rm = TRUE
      )
      rslt_T <- svyby(~zzz, make.formula(itype),
        design = subset(design, tst),
        svytotal, na.rm = TRUE
      )
      rslt_U <- rslt_svy[, 2:(nlev_ivar + 1)]
      temp <- rslt_T[2]
      names(temp) <- "Total"
      rslt_U <- cbind(rslt_U, Total = temp)
      if (vartype == "Local") {
        temp <- cat_localmean_total(
          itype, lev_itype, nlev_itype, ivar,
          lev_ivar, nlev_ivar, design, design_names, rslt_U, popcorrect,
          vartype, mult, warn_ind, warn_df
        )
        stderr_U <- temp$stderr_U
        confval_U <- temp$confval_U
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      } else {
        temp <- SE(rslt_T)
        stderr_U <- cbind(SE(rslt_svy), Total = temp)
        temp <- as.data.frame(confint(rslt_T, level = conf / 100))
        tempdf <- confint(rslt_svy, level = conf / 100)
        confval_U <- NULL
        for (i in 1:nlev_itype) {
          confval_U <- rbind(confval_U, tempdf[seq(i,
            by = nlev_itype,
            length = nlev_ivar
          ), ], Total = temp[i, ])
        }
      }
    }
  }

  # Assign identifiers and estimates to the catsum data frame

  snames <- dimnames(rslt_P)[[1]]
  inames <- rep(ivar, nlev_ivar + 1)
  lev <- c(lev_ivar, "Total")
  cnames <- lev
  nresp <- NULL
  for (j in snames) {
    eval(parse(text = paste0("temp <- with(dframe, addmargins(table(", itype, " ,", ivar, ")))")))
    colnames(temp)[ncol(temp)] <- "Total"
    temp <- temp[-nrow(temp), ]
    for (k in lev) {
      if (nlev_itype == 1) {
        nresp <- c(nresp, temp[k])
      } else {
        nresp <- c(nresp, temp[j, k])
      }
    }
  }
  k <- 1
  for (i in 1:nrow(rslt_P)) {
    inc <- (i - 1) * (nlev_ivar + 1)
    for (j in 1:length(inames)) {
      catsum <- rbind(catsum, data.frame(
        Type = itype,
        Subpopulation = snames[i],
        Indicator = inames[j],
        Category = cnames[j],
        nResp = nresp[k],
        Estimate.P = 100 * rslt_P[i, j],
        StdError.P = 100 * stderr_P[i, j],
        MarginofError.P = 100 * (mult * stderr_P[i, j]),
        LCB.P = 100 * max(confval_P[inc + j, 1], 0),
        UCB.P = 100 * min(confval_P[inc + j, 2], 1),
        Estimate.U = rslt_U[i, j],
        StdError.U = stderr_U[i, j],
        MarginofError.U = mult * stderr_U[i, j],
        LCB.U = max(confval_U[inc + j, 1], 0),
        UCB.U = confval_U[inc + j, 2]
      ))
      k <- k + 1
    }
  }

  # Return the catsum data frame, the warn_ind logical value, and the warn_df
  # data frame

  list(catsum = catsum, warn_ind = warn_ind, warn_df = warn_df)
}
