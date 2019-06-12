################################################################################
# Function: cov.panel.dsgn
# Programmers: Tom Kincaid
#              Tony Olsen
# Date: March 14, 2019
#
#'
#' Covariance Matrix for a Panel Design
#'
#' Covariance structure accounts for the panel design and the four variance
#' components: unit variation, period variation, unit by period interaction
#' variation and index (or residual) variation. The model incorporates unit,
#' period, unit by period, and index variance components. It also includes a
#' provision for unit correlation and period autocorrelation.
#'
#' @param paneldsgn  A matrix (dimensions: number of panels (rows) by number of
#'   periods (columns)) containing the number of units visited for each
#'   combination of panel and period. Default is matrix(50, 1, 10) which is a
#'   single panel of 50 units visited 10 times, typical time is a period.
#'
#' @param nrepeats   Either NULL or a list of matrices the same length as
#'   paneldsgn specifying the number of revisits made to units in a panel in the
#'   same period for each design.  Specifying NULL indicates that number of
#'   revisits to units is the same for all panels and for all periods and for
#'   all panel designs. The default is NULL, a single visit. Names must match
#'   list names in paneldsgn.
#'
#' @param unit.var   The variance component estimate for unit (the default is
#'   Null)
#'
#' @param period.var   The variance component estimate for period (the default
#'   is Null)
#'
#' @param unitperiod.var The variance component estimate for unit by period
#'   interaction (the default is Null)
#'
#' @param index.var  The variance component estimate for index error (the
#'   def0ult is Null)
#'
#' @param unit.rho   unit correlation across periods (the default is 1)
#'
#' @param period.rho   period autocorrelation (the default is 0)
#'
#' @details Covariance structure accounts for the panel design and the four
#'   variance components: unit variation, period variation, unit by period
#'   interaction variation and index (or residual) variation. Uses the model
#'   structure defined by Urquhart.
#'
#'   If nrepeats is NULL, then no units sampled more than once in a specific
#'   panel, period combination) aned then unit by period and index variances are
#'   added together or user may have only estimated unit, period and unit by
#'   period variance components so that index component is zero It calculates
#'   the covariance matrix for the simple linear regression. The standard error
#'   for a linear trend coeficient is the square root of the variance.
#'
#' @references
#'   Urquhart, N. S., W. S. Overton, et al. (1993) Comparing sampling designs
#'   for monitoring ecological status and trends: impact of temporal patterns.
#'   In: \emph{Statistics for the Environment.} V. Barnett and K. F. Turkman.
#'   John Wiley & Sons, New York, pp. 71-86.\cr
#'
#'   Urquhart, N. S. and T. M. Kincaid (1999). Designs for detecting trends
#'   from repeated surveys of ecological resources. \emph{Journal of
#'   Agricultural, Biological, and Environmental Statistics}, \bold{4(4)},
#'   404-414.\cr
#'
#'   Urquhart, N. S. (2012). The role of monitoring design in detecting trend in
#'   long-term ecological monitoring studies. In: \emph{Design and Analysis of
#'   Long-term Ecological Monitoring Studies.} R. A. Gitzen, J. J. Millspaugh,
#'   A. B. Cooper, and D. S. Licht (eds.). Cambridge University Press, New York,
#'   pp. 151-173.
#
#' @return A list containing the covariance matrix (cov) for the panel design,
#'   the input panel design (paneldsgn), the input nrepeats design
#'   (nrepeats.dsgn) and the function call.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @seealso 
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{create a panel revisit design}
#'     \item{\code{\link{revisit_bibd}}}{create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{panel_summary}}}{summarize characteristics of a revisit
#'       panel design}
#'     \item{\code{\link{power.dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @export
################################################################################

cov.panel.dsgn <- function(paneldsgn = matrix(50,1,10), nrepeats = 1,
   unit.var = NULL, period.var = NULL, unitperiod.var = NULL, index.var = NULL,
   unit.rho = 1, period.rho = 0) {

  paneldsgn <- as.matrix (paneldsgn)
  nperiod <- ncol (paneldsgn)
  npanel <- nrow (paneldsgn)

  if (any(is.null(unit.var), is.null(period.var), is.null(unitperiod.var), is.null(index.var)) ) {
    stop("Must provide four variance components. index.var may be zero if nrepeats=1")
  }

  # Create covariance matrix for period correlation
  rhomat <- matrix(0, nrow = nperiod, ncol = nperiod)
  if (nperiod > 1) {
    rhomat[1, ] <- 0:(nperiod-1)
    for(i in 2:nperiod) {
      rhomat[i, ] <- c(rev (1:(i-1)), 0:(nperiod-i))
    }
  }

  period.cov <- period.var * (period.rho ^ rhomat)

  # Create covariance matrix for unit correlation
  unit.cov <- unit.var * (unit.rho ^ rhomat)

  # Construct panel covariance term
  # nunits is number of units in a panel
  nunits <- apply (paneldsgn, 1, max)
  if(npanel > 1) pan.cov <- kronecker (unit.cov, diag (ifelse (nunits >0, 1 / nunits, 0)))
  else pan.cov <- unit.cov / nunits

  # construct period covariance term
  yr.cov <- kronecker(period.cov, matrix (1, nrow = npanel, ncol = npanel))

  # Construct unit by period covariance term
  if (length (nunits) > 1)
    sy.cov <- unitperiod.var * kronecker (diag(nperiod), diag(ifelse(nunits > 0, 1/nunits, 0)))
  else sy.cov <- unitperiod.var * diag(nperiod) / nunits

  # Construct index covariance term
  # Create nrepeats revisit design if necessary
  if(is.null(nrepeats)) {
    nrepeats <- vector("list", length(paneldsgn))
    for (i in names (paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }
  if(npanel > 1) {
    index.cov <- index.var * kronecker(diag (nperiod),
                                       diag (ifelse(nunits > 0, 1/(nunits * apply (nrepeats, 1, max)), 0)) )
  }
  else index.cov <- index.var * diag (nperiod) / nunits

  # overall covariance matrix
  phi <- pan.cov + yr.cov + sy.cov + index.cov

  cov <- list(cov = phi, paneldsgn = paneldsgn, nrepeat.dsgn = nrepeats, call = sys.call() )
  return(cov)
}
