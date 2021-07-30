###############################################################################
# Function: cov_panel_dsgn (exported)
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
#' @param nrepeats   Either \code{NULL} or a list of matrices the same length as
#'   paneldsgn specifying the number of revisits made to units in a panel in the
#'   same period for each design.  Specifying \code{NULL} indicates that number of
#'   revisits to units is the same for all panels and for all periods and for
#'   all panel designs. The default is \code{NULL}, a single visit. Names must match
#'   list names in \code{paneldsgn}.
#'
#' @param unit_var   The variance component estimate for unit. The default is
#'   \code{NULL}.
#'
#' @param period_var   The variance component estimate for period The default is
#'   \code{NULL}.
#'
#' @param unitperiod_var The variance component estimate for unit by period
#'   interaction. The default is \code{NULL}.
#'
#' @param index_var  The variance component estimate for index error. The
#'   default is \code{NULL}.
#'
#' @param unit_rho   Unit correlation across periods. The default is \code{1}.
#'
#' @param period_rho   Period autocorrelation. The default is \code{0}.
#'
#' @details Covariance structure accounts for the panel design and the four
#'   variance components: unit variation, period variation, unit by period
#'   interaction variation and index (or residual) variation. Uses the model
#'   structure defined by Urquhart.
#'
#'   If \code{nrepeats} is \code{NULL}, then no units sampled more than once in a specific
#'   panel, period combination) aned then unit by period and index variances are
#'   added together or user may have only estimated unit, period and unit by
#'   period variance components so that index component is zero. It calculates
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
#' @return A list containing the covariance matrix (\code{cov}) for the panel design,
#'   the input panel design (\code{paneldsgn}), the input nrepeats design
#'   (\code{nrepeats.dsgn}) and the function call.
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
#'     \item{\code{\link{summary.powerpaneldesign}}}{summarize characteristics of a revisit
#'       panel design}
#'     \item{\code{\link{power_dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{plot.powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @export
###############################################################################

cov_panel_dsgn <- function(paneldsgn = matrix(50, 1, 10), nrepeats = 1,
                           unit_var = NULL, period_var = NULL, unitperiod_var = NULL, index_var = NULL,
                           unit_rho = 1, period_rho = 0) {
  paneldsgn <- as.matrix(paneldsgn)
  nperiod <- ncol(paneldsgn)
  npanel <- nrow(paneldsgn)

  if (any(is.null(unit_var), is.null(period_var), is.null(unitperiod_var), is.null(index_var))) {
    stop("Must provide four variance components. index_var may be zero if nrepeats=1")
  }

  # Create covariance matrix for period correlation
  rhomat <- matrix(0, nrow = nperiod, ncol = nperiod)
  if (nperiod > 1) {
    rhomat[1, ] <- 0:(nperiod - 1)
    for (i in 2:nperiod) {
      rhomat[i, ] <- c(rev(1:(i - 1)), 0:(nperiod - i))
    }
  }

  period_cov <- period_var * (period_rho^rhomat)

  # Create covariance matrix for unit correlation
  unit_cov <- unit_var * (unit_rho^rhomat)

  # Construct panel covariance term
  # nunits is number of units in a panel
  nunits <- apply(paneldsgn, 1, max)
  if (npanel > 1) {
    pan_cov <- kronecker(unit_cov, diag(ifelse(nunits > 0, 1 / nunits, 0)))
  } else {
    pan_cov <- unit_cov / nunits
  }

  # construct period covariance term
  yr_cov <- kronecker(period_cov, matrix(1, nrow = npanel, ncol = npanel))

  # Construct unit by period covariance term
  if (length(nunits) > 1) {
    sy_cov <- unitperiod_var * kronecker(diag(nperiod), diag(ifelse(nunits > 0, 1 / nunits, 0)))
  } else {
    sy_cov <- unitperiod_var * diag(nperiod) / nunits
  }

  # Construct index covariance term
  # Create nrepeats revisit design if necessary
  if (is.null(nrepeats)) {
    nrepeats <- vector("list", length(paneldsgn))
    for (i in names(paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }
  if (npanel > 1) {
    index_cov <- index_var * kronecker(
      diag(nperiod),
      diag(ifelse(nunits > 0, 1 / (nunits * apply(nrepeats, 1, max)), 0))
    )
  } else {
    index_cov <- index_var * diag(nperiod) / nunits
  }

  # overall covariance matrix
  phi <- pan_cov + yr_cov + sy_cov + index_cov

  cov <- list(cov = phi, paneldsgn = paneldsgn, nrepeat_dsgn = nrepeats, call = sys.call())
  return(cov)
}
