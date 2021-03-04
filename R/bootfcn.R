################################################################################
# Function: bootfcn (exported)
# Programmer: Tom Kincaid
# Date: February 23, 2021
#
#' Bootstrap Function for Trend Parameter Estimation
#'
#' This function calculates trend parameter estimates using bootstrap replcates.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param indices Vector of indices referencing rows in the dframe data frame.
#'
#' @param ivar Character value providing name of the response variable in the
#'   dframe data frame.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   the dframe data frame.
#'
#' @param yearID Character value providing name of the year variable in
#'   the dframe data frame.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

bootfcn <- function(dframe, indices, ivar, siteID, yearID) {
  dframe <- dframe[indices, ]
  eval(parse(text = paste0("regest <- lmer(", ivar, " ~ Wyear + (1 + Wyear|",
    siteID, ") + (1|", yearID, "), data = dframe,
    control = lmerControl(check.nobs.vs.nRE = 'warning'))")))
  coeff <- summary(regest)$coefficients
  vcor <- as.data.frame(VarCorr(regest))
  rslt <- c(
    siteint = coeff[1, "Estimate"],
    siteint_stderr = coeff[1, "Std. Error"],
    siteslope = coeff[2, "Estimate"],
    siteslope_stderr = coeff[2, "Std. Error"],
    var_siteint = vcor[1, 4],
    var_siteslope = vcor[2, 4],
    corr_site = vcor[3, 5],
    var_year = vcor[4, 4],
    var_resid = vcor[5, 4],
    AIC = extractAIC(regest)[2]
  )
  rslt
}
