#' spsurvey: Spatial Sampling Design and Analysis
#'
#' spsurvey implements a design-based approach to statistical inference,
#'   with a focus on spatial data.
#'   Spatially balanced samples are selected using the
#'   Generalized Random Tessellation Stratified (GRTS) algorithm.
#'   The GRTS algorithm can be applied to finite resources (point geometries) and
#'   infinite resources (linear / linestring and areal / polygon geometries) and flexibly
#'   accommodates a diverse set of sampling design features, including
#'   stratification, unequal inclusion probabilities, proportional (to size)
#'   inclusion probabilities, legacy (historical) sites, a minimum distance between
#'   sites, and two options for replacement sites (reverse hierarchical order and
#'   nearest neighbor). Data are analyzed using a wide
#'   range of analysis functions that perform categorical variable analysis, continuous
#'   variable analysis, attributable risk analysis, risk difference analysis, relative
#'   risk analysis, change analysis, and trend analysis. spsurvey can also be used to
#'   summarize objects, visualize objects, select samples that are not spatially balanced,
#'   select panel samples, measure the amount of spatial balance in a sample,
#'   adjust design weights, and more.
#'   This R package has been reviewed in accordance
#'   with U.S. Environmental Protection Agency policy and approved for publication.
#'   Mention of trade names or commercial products does not constitute endorsement or
#'   recommendation for use.
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
