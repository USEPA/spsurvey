###############################################################################
# Function: revisit_bibd (exported)
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Create a balanced incomplete block panel revisit design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods for the units of each panel to be sampled based on searching for a
#' D-optimal block design that is a member of the class of generalized Youden
#' designs.  The resulting design need not be a balanced incomplete block
#' design.  Based on algorithmic idea by Cook and Nachtsheim (1989) and
#' implemented by Robert Wheeler.
#'
#' @param n_period  Number of time periods for the survey design. Typically,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month. (v, number of varieties/treatments in BIBD
#'   terms)
#'
#' @param n_pnl Number of panels (b, number of blocks in BIBD terms)
#'
#' @param n_visit  Number of time periods to be visited in a panel (k, block
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
#'   periods, e.g., sampling will occur only every 5 periods if \code{skip = 5}.
#'
#' @param iter  Maximum number of iterations in search for D-optimal
#'   Generalized Youden Design.
#'
#' @details  The function uses \code{find.BIB} function from crossdes package to
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
#' @author Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{to create a panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{to create a panel revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{summary.paneldesign}}}{ to summarize characteristics of a
#'       panel revisit design}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Balanced incomplete block design with 20 sample occasions, 20 panels,
#' # 3 visits to each unit, and 20 units in each panel.
#' revisit_bibd(n_period = 20, n_pnl = 20, n_visit = 3, nsamp = 20)
#' @export
###############################################################################

revisit_bibd <- function(n_period, n_pnl, n_visit, nsamp, panel_name = "BIB",
                         begin = 1, skip = 1, iter = 30) {

  # check input parameters for meeting requirements
  if (!(n_period > n_visit & n_pnl >= n_period & n_visit > 1)) {
    stop("\nInput parameters do not satisfy minimal conditions")
  }

  # use crossdes package functions to generate BIB design
  bib <- find.BIB(n_period, n_pnl, n_visit, iter = iter)
  pan_dsgn <- table(c(rep(1:n_pnl, rep(n_visit, n_pnl))), as.vector(t(bib)))
  pan_dsgn[pan_dsgn == 1] <- nsamp

  # assign dimnames
  if (nrow(pan_dsgn) < 10) {
    dimnames(pan_dsgn) <- list(
      c(paste(panel_name, 1:nrow(pan_dsgn), sep = "_")),
      seq(begin, by = skip, length.out = ncol(pan_dsgn))
    )
  } else {
    dimnames(pan_dsgn) <- list(
      c(
        paste(panel_name, 1:9, sep = "_0"),
        paste(panel_name, 10:nrow(pan_dsgn), sep = "_")
      ),
      seq(begin, by = skip, length.out = ncol(pan_dsgn))
    )
  }
  class(pan_dsgn) <- "paneldesign"
  return(pan_dsgn)
}
