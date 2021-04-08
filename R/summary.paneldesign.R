###############################################################################
# Function: panel_summary (exported)
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Summary Characteristics of a Revisit Panel Design
#'
#' Revisit panel design characteristics are summarized: number of panels, number
#' of time periods, total number of sample events for the revisit design, total
#' number of sample events for each panel, total number of sample events for
#' each time period and cumulative number of unique units sampled by time
#' periods.
#'
#' @param  object Two-dimensional array with class \code{panel_design} and dimnames specifying revisit
#'   panel design. Typically, array is output from \code{revisit_dsgn}, \code{revisit_bibd} or
#'   \code{revisit_rand} functions.
#'
#' @param  visitdsgn Two-dimensional array with same dimensions as \code{paneldsgn}
#'   specifying the number of times a sample unit is sampled at each time
#'   period. Default is \code{visitdsgn=NULL}, where default assumes that a sample unit
#'   will be sampled only once at each time period.
#'
#' @param ... Additional arguments (S3 consistency)
#'
#' @details The revisit panel design and the visit design (if present) are
#'   summarized. Summaries can be useful to know the effort required to complete
#'   the survey design. See the values returned for the summaries that are
#'   produced.
#'
#' @return List of six elements.
#'   \itemize{ \item \code{n_panel} - number of panels in revisit design
#'
#'   \item \code{n_period} - number of time periods in revisit design
#'
#'   \item \code{n_total} - total number of sample events across all panels and all
#'   time periods, accounting for visitdsgn, that will be sampled in the revisit
#'   design
#'
#'   \item \code{n_periodunit} - Vector of the number of time periods a unit will be
#'   sampled in each panel
#'
#'   \item \code{n_unitpnl} - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled in each panel
#'
#'   \item \code{n_unitperiod} - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled during each time period
#'
#'   \item \code{ncum_unit} - Vector of the cumulative number of unique units that will
#'   be sampled in time periods up to and including the current time period.
#'   }
#'
#' @author Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{create a panel revisit design}
#'     \item{\code{\link{revisit_bibd}}}{create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{power_dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov_panel_dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot.powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Serially alternating panel revisit design summary
#' sa_dsgn <- revisit_dsgn(20, panels = list(SA60N = list(
#'   n = 60, pnl_dsgn = c(1, 4),
#'   pnl_n = NA, start_option = "None"
#' )), begin = 1)
#' summary(sa_dsgn)
#'
#' # Add visit design where first panel is sampled twice at every time period
#' sa_visit <- sa_dsgn
#' sa_visit [sa_visit > 0] <- 1
#' sa_visit [1, sa_visit[1, ] > 0] <- 2
#' summary(sa_dsgn, sa_visit)
#' @export
###############################################################################
summary.paneldesign <- function(object, visitdsgn = NULL, ...) {
  paneldsgn <- object
  n_pan <- dim(paneldsgn)[1]
  n_period <- dim(paneldsgn)[2]

  # determine the cumulative number of unique sample units by sampling occasion
  used <- rep(FALSE, n_pan)
  tot <- vector("numeric", length = n_period)
  for (i in 1:n_period) {
    units <- paneldsgn[, i] > 0
    new <- used + units
    tot[i] <- sum(paneldsgn[new == 1, i])
    used <- new
  }
  n_unique_com <- cumsum(tot)
  names(n_unique_com) <- dimnames(paneldsgn)[[2]]

  # summarize number of sample results by panel and by time period
  # incorporate multiple times unit is sampled if sample units for a time period are
  # sampled more than once.
  ifelse(!is.null(visitdsgn), vis <- visitdsgn * paneldsgn, vis <- paneldsgn)
  n_unitpnl <- apply(vis, 1, sum)
  n_unitperiod <- apply(vis, 2, sum)
  n_total <- sum(n_unitperiod)
  # number of times a sample unit is visited in each panel
  tmp <- array(0, c(n_pan, n_period))
  tmp[paneldsgn > 0] <- 1
  ifelse(!is.null(visitdsgn), vis <- visitdsgn * tmp, vis <- tmp)
  n_periodunit <- apply(vis, 1, sum)
  names(n_periodunit) <- dimnames(paneldsgn)[[1]]

  # create list of results
  rslt <- list(
    n_panel = n_pan,
    n_period = n_period,
    n_total = n_total,
    n_periodunit = n_periodunit,
    n_unitpnl = n_unitpnl,
    n_unitperiod = n_unitperiod,
    ncum_unit = n_unique_com
  )
  return(rslt)
}
