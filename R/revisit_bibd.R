################################################################################
# Function: revisit_bibd
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Create a Balanced Incomplete Block Panel Revisit Design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods for the units of each panel to be sampled based on searching for a
#' D-optimal block design that is a member of the class of generalized Youden
#' designs.  The resulting design need not be a balanced incomplete block
#' design.  Based on algorithmic idea by Cook and Nachtsheim (1989) and
#' implemented by Robert Wheeler.
#'
#' @param n.period  Number of time periods for the survey design. Typically,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month. (v, number of varieties/treatments in BIBD
#'   terms)
#'
#' @param n.pnl Number of panels (b, number of blocks in BIBD terms)
#'
#' @param n.visit  Number of time periods to be visited in a panel (k, block
#'   size in BIBD terms)
#'
#' @param nsamp Number of samples in each panel.
#'
#' @param panel_name  Prefix for name of each panel
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific
#'   period.
#'
#' @param skip  Number of sampling occasions to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @param iter  Maximum number of iterations in search for D-optimal
#'   Generallized Youden Design.
#'
#' @details  The function uses find.BIB function from crossdes package to
#' search for a D-optimal block design.  crossdes uses package AlgDesign
#' to search balanced incomplete block designs.
#'
#' @return A two-dimensional array of sample sizes to be sampled for each panel
#'   and each sampling occasion.
#'
#' @references
#'   Cook R. D.  and C. Nachtsheim. (1989). Computer-aided blocking of factorial
#'   and response-surface designs. \emph{Technometrics} \bold{31(3)}, 339-346.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @seealso 
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{create a panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{panel_summary}}}{summarize characteristics of a revisit
#'       panel design}
#'     \item{\code{\link{power.dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov.panel.dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Balanced incomplete block design with 20 sample occasions, 20 panels,
#' # 3 visits to each unit, and 20 units in each panel.
#' revisit_bibd(n.period = 20, n.pnl = 20, n.visit = 3, nsamp = 20)
#'
#' @export
################################################################################

revisit_bibd <- function (n.period, n.pnl, n.visit, nsamp, panel_name = "BIB",
   begin = 1, skip = 1, iter = 30 ) {

  # check input parameters for meeting requirements
  if ( !(n.period > n.visit & n.pnl >= n.period & n.visit > 1)) {
    stop ("\nInput parameters do not satisfy minimal conditions")
  }

  # use crossdes package functions to generate BIB design
  bib <- find.BIB(n.period, n.pnl, n.visit, iter = iter)
  pan.dsgn <- table (c(rep(1:n.pnl, rep(n.visit, n.pnl))), as.vector(t(bib)) )
  pan.dsgn[pan.dsgn ==1] <- nsamp

  # assign dimnames
  if (nrow(pan.dsgn) < 10) {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:nrow(pan.dsgn), sep="_") ),
                                seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  else {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:9, sep="_0"),
                                  paste(panel_name, 10:nrow(pan.dsgn), sep="_") ),
                                seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  class (pan.dsgn) <- "paneldesign"
  return (pan.dsgn)
}
