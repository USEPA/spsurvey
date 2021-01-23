################################################################################
# Function: revisit_rand
# Programmer: Tony Olsen
# Date: March 15, 2019
#'
#' Create a Revisit Design with Random Assignment to Panels and Time Periods
#'
#' Create a revisit design for a survey that specifies the panels and time
#' periods that will be sampled by random selection of panels and time periods.
#' Three options for random assignments are "period" where the number of time
#' periods to be sampled in a panel is fixed, "panel" where the number panels to
#' be sampled in a time period is fixed, and "none" where the number of
#' panel-period combinations is fixed.
#'
#' @param n_period  Number of time periods for the survey design. Typically,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month. (v, number of varieties (or treatments) in
#'   BIBD terms)
#'
#' @param n_pnl Number of panels
#'
#' @param rand_control  Character value must be "none", "panel", or "period".
#'   Specifies whether the number of sample events will be fixed for each panel
#'   ("panel"), for each sample occasion ("occasion"), or for total panel-period
#'   combinations ("none").  Default is "panel".
#'
#' @param n_visit If rand_control is "panel", this is the number of panels that
#'   will be sampled in each time period. If rand_control is "period", this is
#'   the number of time periods to be sampled in each panel. If rand_control is
#'   "none", this is the total number of panel-period combinations that will
#'   have units sampled in the revisit design.
#'
#' @param nsamp Number of samples in each panel.
#'
#' @param panel_name  Prefix for name of each panel
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific period.
#'
#' @param skip  Number of sampling occasions to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @details  The revisit design for a survey is created by random selection of
#'   panels and time periods that will have sample events.  The number of sample
#'   occasions that will be visited by a panel is random.
#'
#' @return A two-dimensional array of sample sizes to be sampled for each panel
#'   and each time period.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @seealso 
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{create a panel revisit design}
#'     \item{\code{\link{revisit_bibd}}}{create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{panel_summary}}}{summarize characteristics of a revisit
#'       panel design}
#'     \item{\code{\link{power_dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov_panel_dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' revisit_rand(n_period = 20, n_pnl = 10, rand_control = "none", n_visit = 50,
#'              nsamp = 20)
#' revisit_rand(n_period = 20, n_pnl = 10, rand_control = "panel", n_visit = 5,
#'              nsamp = 10)
#' revisit_rand(n_period = 20, n_pnl = 10, rand_control = "period",
#'               n_visit = 5, nsamp = 10)
#'
#' @export
################################################################################

revisit_rand <- function (n_period, n_pnl, rand_control = "period",  n_visit,
   nsamp, panel_name = "Random", begin = 1, skip = 1) {

  if ( !(rand_control %in% c("none", "panel", "period")) ) {
    stop ("\nRandom control not equal to none, panel or period")
  }

  if (rand_control == "panel" ) {
    pan_dsgn <- matrix(rep (c(rep(nsamp, n_visit), rep (0, n_period - n_visit)), n_pnl),
                       ncol=n_period, byrow = TRUE)
    pan_dsgn <- t (apply (pan_dsgn, 1, function(x) sample(x, size = length(x),
      replace = FALSE)) )
  }

  if (rand_control == "period" ) {
    pan_dsgn <- matrix(rep (c(rep(nsamp, n_visit), rep (0, n_pnl - n_visit)), n_period),
                       ncol=n_period)
    pan_dsgn <- apply (pan_dsgn, 2, function(x) sample(x, size = length(x),
      replace = FALSE))
  }

  if (rand_control == "none") {
    pan_dsgn <- c( rep(nsamp, n_visit), rep (0, (n_pnl * n_period) - n_visit) )
    pan_dsgn <- sample (pan_dsgn, size = length(pan_dsgn), replace = FALSE )
    pan_dsgn <- matrix (pan_dsgn, nrow = n_pnl)
  }

  # drop panels if no visits
  keep <- ifelse (apply (pan_dsgn, 1, sum) > 0, TRUE, FALSE)
  pan_dsgn <- pan_dsgn[keep, ]

  # assign dimnames
  if (nrow(pan_dsgn) < 10) {
    dimnames (pan_dsgn) <- list(c(paste(panel_name, 1:nrow(pan_dsgn), sep="_") ),
                               seq(begin, by = skip, length.out=ncol(pan_dsgn)) )
  }
  else {
    dimnames (pan_dsgn) <- list(c(paste(panel_name, 1:9, sep="_0"),
                                 paste(panel_name, 10:nrow(pan_dsgn), sep="_") ),
                               seq(begin, by = skip, length.out=ncol(pan_dsgn)) )
  }
  class (pan_dsgn) <- "paneldesign"
  return (pan_dsgn)
}
