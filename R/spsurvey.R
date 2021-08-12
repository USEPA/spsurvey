#' spsurvey: Spatial Probability Survey Design and Analysis
#'
#' The spsurvey package provides functions for spatial probability survey design and analysis.
#' To select spatially balanced probability samples, spsurvey implements the
#' Generalized Random Tessellation Stratified (GRTS) algorithm.  The GRTS algorithm
#' can be applied to finite resources (point geometries) and infinite resources
#' (linear and polygon / areal geometries) and flexibly accommodates a diverse set
#' of sampling design features, including stratification, unequal inclusion
#' probabilities, proportional (to size) inclusion probabilities, legacy
#' (historical) sites, a minimum distance between sites, and two options for
#' replacement sites (reverse hierarchical order and nearest neighbor). To analyze
#' probability samples, spsurvey implements a wide range of analysis functions
#' that cover categorical variable analysis, continuous variable analysis,
#' attributable risk analysis, risk difference analysis, relative risk analysis,
#' change analysis, and trend analysis. spsurvey can also be used to summarize and
#' visualize data, select samples that are not spatially balanced, compute
#' spatial balance metrics, study cumulative distribution functions, select
#' panel samples, and more.
#'
# Import all packages listed as Depends or Imports
#' @import sf
#' @import survey
#' @importFrom boot boot
#' @importFrom crossdes find.BIB
#' @importFrom deldir deldir tile.list
#' @importFrom graphics axis box legend lines mtext par plot points text title
#' @importFrom grDevices graphics.off pdf rainbow
#' @importFrom lme4 lmer VarCorr
#' @importFrom MASS ginv
#' @importFrom stats addmargins chisq.test confint dist dnorm extractAIC ftable
#'             lm model.frame model.matrix na.omit na.pass optimize pchisq pf
#'             pnorm qnorm rnorm runif terms update var weights
#' @importFrom units set_units
#'
#'
#'
"_PACKAGE"
