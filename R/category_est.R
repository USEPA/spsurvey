################################################################################
# Function: category_est
# Programmer: Tom Kincaid
# Date: July 23, 2020
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
#' @param warn.ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @return A list composed of the following objects:
#'   \describe{
#'     \item{\code{catsum}}{data frame containing the category estimates}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
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
  warn.ind, warn.df) {

# Assign a value to the function name variable

  fname <- "category_est"

# Calculate category proportion estimates, standard error estimates, and
# confidence bound estimates for each combination of subpopulation and response
# variable

  tst <- !is.na(dframe[, itype])
  if(nlev_itype == 1) {
    if(nlev_ivar <= 1) {
      temp <- ifelse(all(is.na(dframe[, ivar])), 0.0, 1.0)
      rslt.P <- data.frame(ivar = temp, Total = temp)
      dimnames(rslt.P)[1] <- lev_itype
      stderr.P <- data.frame(ivar = 0.0, Total = 0.0)
      temp <- data.frame(LCB = temp, UCB = temp)
      confval.P <- rbind(ivar = temp, Total = temp)
    } else {
      rslt.svy <- svymean(make.formula(ivar), design = subset(design, tst),
        na.rm = TRUE)
      rslt.P <- cbind(data.frame(t(as.data.frame(rslt.svy)[1])),
        Total = 1.0)
      dimnames(rslt.P)[1] <- lev_itype
      if(vartype == "Local") {
        temp <- cat_localmean_prop(itype, lev_itype, nlev_itype, ivar, lev_ivar,
          nlev_ivar, design, design_names, rslt.P, popcorrect, vartype, mult,
          warn.ind, warn.df)
        stderr.P <- temp$stderr.P
        confval.P <- temp$confval.P
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.P <- cbind(t(data.frame(SE(rslt.svy))), Total = 0.0)
        confval.P <- confint(rslt.svy, level = conf/100)
        tempdf <- data.frame(LCB = 1.0, UCB = 1.0)
        dimnames(tempdf) <- list("Total", colnames(confval.P))
        confval.P <- rbind(confval.P, tempdf)
      }
    }
  } else {
    if(nlev_ivar == 1) {
      ivar_splt <- split(dframe[, ivar], dframe[, itype])
      temp <- ifelse(sapply(ivar_splt, function(x) all(is.na(x))), 0.0, 1.0)
      rslt.P <- data.frame(ivar = temp, Total = temp)
      dimnames(rslt.P)[[1]] <- lev_itype
      stderr.P <- data.frame(ivar = rep(0.0, nlev_itype), Total = rep(0.0,
        nlev_itype))
      confval.P <- NULL
      for(i in 1:nlev_itype) {
        tempdf <- rbind(data.frame(temp[i], temp[i]), data.frame(temp[i],
          temp[i]))
        rownames(tempdf) <- c(paste(lev_itype[i], ivar, sep=":"),
          paste(lev_itype[i], "Total", sep=":"))
        colnames(tempdf) <- c("LCB", "UCB")
        confval.P <- rbind(confval.P, tempdf)
      }
    } else {
      rslt.svy <- svyby(make.formula(ivar), make.formula(itype),
        design = subset(design, tst), svymean, na.rm = TRUE)
      rslt.P <- rslt.svy[, 2:(nlev_ivar + 1)]
      rslt.P <- cbind(rslt.P, Total = rep(1, nlev_itype))
      if(vartype == "Local") {
        temp <- cat_localmean_prop(itype, lev_itype, nlev_itype, ivar, lev_ivar,
          nlev_ivar, design, design_names, rslt.P, popcorrect, vartype, mult,
          warn.ind, warn.df)
        stderr.P <- temp$stderr.P
        confval.P <- temp$confval.P
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        stderr.P <- cbind(SE(rslt.svy), Total = rep(0, nlev_itype))
        tempdf <- confint(rslt.svy, level = conf/100)
        confval.P <- NULL
        for(i in 1:nlev_itype) {
          confval.P <- rbind(confval.P, tempdf[seq(i, by=nlev_itype,
            length=nlev_ivar), ])
          temp <- data.frame(1.0, 1.0)
          colnames(temp) <- colnames(tempdf)
          confval.P <- rbind(confval.P, Total = temp)
        }
      }
    }
  }

# Calculate category total estimates, standard error estimates, and
# confidence bound estimates for each combination of subpopulation and response
# variable

  if("postStrata" %in% names(design)) {
    zzz <- ifelse(is.na(design$variables[, ivar]), NA, 1)
  } else {
    zzz <- ifelse(is.na(dframe[, ivar]), NA, 1)
  }
  design <- update(design, zzz = zzz)
  if(nlev_itype == 1) {
    if(nlev_ivar <= 1) {
      rslt.svy <- svytotal(~zzz, design = subset(design, tst),  na.rm = TRUE)
      rslt.U <- data.frame(ivar = rslt.svy[1], Total = rslt.svy[1])
      dimnames(rslt.U)[1] <- lev_itype
      if(vartype == "Local") {
        temp <- cat_localmean_total(itype, lev_itype, nlev_itype, "zzz",
          lev_ivar, nlev_ivar, design, design_names, rslt.U, popcorrect,
          vartype, mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        temp <- SE(rslt.svy)
        stderr.U <- data.frame(ivar = as.vector(temp), Total = as.vector(temp))
        temp <- as.data.frame(confint(rslt.svy, level = conf/100))
        confval.U <- rbind(ivar = temp, Total = temp)
      }
    } else {
       rslt.svy <- svytotal(make.formula(ivar), design = subset(design, tst),
        na.rm = TRUE)
      rslt.T <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
      rslt.U <- cbind(data.frame(t(as.data.frame(rslt.svy)[1])),
        Total = rslt.T[1])
      dimnames(rslt.U)[1] <- lev_itype
      if(vartype == "Local") {
        temp <- cat_localmean_total(itype, lev_itype, nlev_itype, ivar,
          lev_ivar, nlev_ivar, design, design_names, rslt.U, popcorrect,
          vartype, mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        temp <- SE(rslt.T)
        stderr.U <- cbind(t(data.frame(SE(rslt.svy))), Total = as.vector(temp))
        temp <- as.data.frame(confint(rslt.T, level = conf/100))
        confval.U <- rbind(confint(rslt.svy, level = conf/100), Total = temp)
      }
    }
  } else {
    if(nlev_ivar == 1) {
      rslt.svy <- svyby(~zzz, make.formula(itype), design = subset(design, tst),
        svytotal, na.rm = TRUE)
      rslt.U <- data.frame(ivar = rslt.svy$zzz, Total = rslt.svy$zzz)
      dimnames(rslt.U)[[1]] <- lev_itype
      if(vartype == "Local") {
        temp <- cat_localmean_total(itype, lev_itype, nlev_itype, "zzz",
          lev_ivar,  nlev_ivar, design, design_names, rslt.U, popcorrect,
          vartype, mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        temp <- SE(rslt.svy)
        stderr.U <- data.frame(ivar = temp, Total = temp)
        temp <- as.data.frame(confint(rslt.svy, level = conf/100))
        confval.U <- rbind(ivar = temp, Total = temp)
      }
    } else {
      rslt.svy <- svyby(make.formula(ivar), make.formula(itype),
        design = subset(design, tst), svytotal, na.rm = TRUE)
      rslt.T <- svyby(~zzz, make.formula(itype), design = subset(design, tst),
        svytotal, na.rm = TRUE)
      rslt.U <- rslt.svy[, 2:(nlev_ivar + 1)]
      temp <- rslt.T[2]
      names(temp) <- "Total"
      rslt.U <- cbind(rslt.U, Total = temp)
      if(vartype == "Local") {
        temp <- cat_localmean_total(itype, lev_itype, nlev_itype, ivar,
          lev_ivar,  nlev_ivar, design, design_names, rslt.U, popcorrect,
          vartype, mult, warn.ind, warn.df)
        stderr.U <- temp$stderr.U
        confval.U <- temp$confval.U
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df
      } else {
        temp <- SE(rslt.T)
        stderr.U <- cbind(SE(rslt.svy), Total = temp)
        temp <- as.data.frame(confint(rslt.T, level = conf/100))
        tempdf <- confint(rslt.svy, level = conf/100)
        confval.U <- NULL
        for(i in 1:nlev_itype) {
          confval.U <- rbind(confval.U, tempdf[seq(i, by=nlev_itype,
            length=nlev_ivar), ], Total = temp[i, ])
        }
      }
    }
  }

# Assign identifiers and estimates to the catsum data frame

  snames <- dimnames(rslt.P)[[1]]
  inames <- rep(ivar, nlev_ivar + 1)
  lev <- c(lev_ivar, "Total")
  cnames <- lev
  nresp <- NULL
  for(j in snames) {
    eval(parse(text=paste0("temp <- with(dframe, addmargins(table(", itype, " ,", ivar, ")))")))
    colnames(temp)[ncol(temp)] <- "Total"
    temp <- temp[-nrow(temp), ]
    for(k in lev) {
      if(nlev_itype == 1) {
        nresp <- c(nresp, temp[k])
      } else {
        nresp <- c(nresp, temp[j, k])
      }
    }
  }
  k <- 1
  for(i in 1:nrow(rslt.P)) {
    inc <- (i - 1) * (nlev_ivar + 1)
    for(j in 1:length(inames)) {
      catsum <- rbind(catsum, data.frame(
        Type = itype,
        Subpopulation = snames[i],
        Indicator = inames[j],
        Category = cnames[j],
        nResp = nresp[k],
        Estimate.P = 100 * rslt.P[i, j],
        StdError.P = 100 * stderr.P[i, j],
        MarginofError.P = 100 * (mult * stderr.P[i, j]),
        LCB.P = 100 * max(confval.P[inc + j, 1], 0),
        UCB.P = 100 * min(confval.P[inc + j, 2], 1),
        Estimate.U = rslt.U[i, j],
        StdError.U = stderr.U[i, j],
        MarginofError.U = mult * stderr.U[i, j],
        LCB.U = max(confval.U[inc + j, 1], 0),
        UCB.U = confval.U[inc + j, 2]))
        k <- k + 1
    }
  }

# Return the catsum data frame, the warn.ind logical value, and the warn.df
# data frame

  list(catsum = catsum, warn.ind = warn.ind, warn.df = warn.df)
}
