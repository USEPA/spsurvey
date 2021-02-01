################################################################################
# Function:   power.dsgn
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Power Calculation for Multiple Panel Designs
#'
#' Calculates the power for trend detection for one or more variables, for one
#' or more panel designs, for one or more linear trends, and for one or more
#' signficance levels.  The panel designs create a covarance model where the
#' model includes variance components for units, periods, the interaction of
#' units and periods, and the residual (or index) variance.
#'
#' @param ind_names  Vector of indicator names
#'
#' @param ind_values  Vector of indicator mean values
#'
#' @param unit_var  Vector of variance component estimates for unit variability
#'   for the indicators
#'
#' @param period_var  Vector of variance component estimates for period
#'   variability for the indicators
#'
#' @param unitperiod_var  Vector of variance component estimates for unit by
#'   period interaction variability for the indicators
#'
#' @param index_var  Vector of variance component estimates for index (residual)
#'   error for the indicators
#'
#' @param unit_rho  Correlation across units. Default is 1
#'
#' @param period_rho  Correlation across periods. Default is 0
#'
#' @param paneldsgn  A list of panel designs each as a matrix.  Each element of
#'   the list is a matrix with dimnames (dimensions: number of panels (rows) by
#'   number of periods (columns)) containing the number of units visited for 
#'   each combination of panel and period.  Dimnames for columns must be
#'   convertable to an integer (e.g., 2016).  All designs must span the same
#'   number of periods.  Typically, the panel designs are the output of the
#'   function revisit_dsgn.
#'
#' @param nrepeats  Either NULL or a list of matrices the same length as
#'   paneldsgn specifying the number of revisits made to units in a panel in the
#'   same period for each design.  Specifying NULL indicates that number of
#'   revisits to units is the same for all panels and for all periods and for
#'   all panel designs. The default is NULL, a single visit. Names must match
#'   list names in paneldsgn.
#'
#' @param trend_type  Trend type is either "mean" where trend is applied as
#'   percent trend in the indicator mean or "percent" where the trend is applied
#'   as percent trend in the proportion (percent) of the distribution that is
#'   below or above a fixed value. Default is trend_type="mean"
#'
#' @param ind_pct  When trend_type is equal to "percent", a vector of the
#'   values of the indicator fixed value that defines the percent.  Default is
#'   NULL
#'
#' @param ind_tail  When trend_type is equal to "percent", a character vector
#'   with values of either "lower" or "upper" for each indicator.  "lower"
#'   states that the percent is associated with the lower tail of the
#'   distribution and "upper" states that the percent is associated with the
#'   upper tail of the distribution. Default is NULL.
#'
#' @param trend  Single value or vector of assumed percent change from
#'   initial value in the indicator for each period. Assumes the trend is
#'   expressed as percent per period. Note that the trend may be either positive
#'   or negative. The default is trend=2.
#'
#' @param alpha  Single value or vector of significance level for linear
#'   trend test, alpha, Type I error, level.  The defualt is 0.05.
#'
#' @details Calculates the power for detecting a change in the mean for
#'   different panel design structures. The model incorporates unit, period,
#'   unit by period, and index variance components as well as correlation across
#'   units and across periods.  See references for methods.
#
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
#' @return A list with components trend_type, ind_pct, ind_tail, trend values
#'   across periods, periods (all periods included in one or more panel
#'   designs), significance levels, a five-dimensional array of power
#'   calculations (dimensions: panel, design names, periods, indicator names,
#'   trend names, alpha_names), an array of indicator mean values for each trend
#'   and the function call.
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
#'     \item{\code{\link{cov_panel_dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Power for rotating panel with sample size 60
#' power.dsgn("Variable_Name", ind_values = 43, unit_var = 280, period_var = 4,
#'            unitperiod_var = 40, index_var = 90, unit_rho = 1, period_rho = 0,
#'            paneldsgn = list(NoR60=revisit_dsgn(20,
#'                             panels=list(NoR60=list(n=60, pnl_dsgn = c(1, NA),
#'                             pnl_n = NA, start_option = "None")), begin = 1)),
#'             nrepeats = NULL, trend_type = "mean", trend= 1.0, alpha=0.05)
#'
#' @export
################################################################################

power.dsgn <- function(ind_names, ind_values, unit_var, period_var,
   unitperiod_var, index_var, unit_rho = 1, period_rho = 0, paneldsgn,
   nrepeats = NULL, trend_type = "mean", ind_pct = NULL, ind_tail = NULL,
   trend = 2, alpha = 0.05) {

  # number of indicators for power comparison
  n_ind <- length(ind_names)

  # number of trend change values for power comparison
  n_trend <- length (trend)
  trend_names <- paste ("Trend_", round (trend, 1), "%", sep="")

  # check that trend_type has correct possible values
  if (!(trend_type %in% c("mean", "percent"))) stop ("\ntrend_type value not 'mean' or 'percent' ")

  # number of significance levels for power comparison
  n_alpha <- length (alpha)
  alpha_names <- paste ("alpha_", alpha, sep="")

  # number of designs for power comparison
  n_dsgn <- length (paneldsgn)

  # Create nrepeats revisit design if necessary
  if(is.null(nrepeats)) {
    nrepeats <- vector("list", n_dsgn)
    for (i in names (paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }

  # number of periods for power comparison
  # First set up periods to calculate trend for each panel design based on periods when units are sampled
  period_power <- vector("list", n_dsgn)
  names (period_power) <- names (paneldsgn)
  period_values <- numeric(length=0 )
  for (i in names (paneldsgn)) {
    period_power[[i]] <- ifelse ( apply (paneldsgn[[i]], 2, max) > 0, TRUE, FALSE)
    period_power[[i]] <- as.numeric(dimnames(paneldsgn[[i]])[[2]][period_power[[i]]])
    period_values <- c(period_values, period_power[[i]])
  }
  # find minimum and maximum period from designs that are monitored
  period_min <- min (period_values)
  period_max <- max (period_values)
  periods <- seq(period_min, by = 1, to=period_max)
  n_periods <- length (periods)

  # When trend_type = "percent", check if for lower or upper tail
  if (!is.null(ind_tail)) {
    if (any( !(ind_tail %in% c("lower", "upper")) ) ) stop ("\nValues must be lower or upper")
    lower.tail <- ind_tail == "lower"
  }

  # set up output array for calculated power for each indicator, panel design, periods, trend, and alpha
  pout <- array(NA, c(n_dsgn, n_periods, n_ind, n_trend, n_alpha))
  dimnames (pout) <- list(names (paneldsgn), periods, ind_names, trend_names, alpha_names)

  # set up change in mean for output and use in power calculations
  trend_value <- array(NA, c(n_trend, n_periods, n_ind))
  dimnames (trend_value) <- list(trend_names, as.character(periods), ind_names)

  # set up trend for power calculation based on percent per period and indicator value so
  # that trend is in units indicator units of change per period.

  for (ind in 1:length(ind_names)) {

    for (k in 1:length(trend_names)) {
      # calculate trend value
      if (trend_type == "mean") {
        trend_delta <- ind_values[ind] * trend[k] / 100
        trend_value[k, ,ind] <- ind_values[ind] + trend_delta * seq(0, to=n_periods-1, by=1)
        }
      if (trend_type == "percent") {
        # find cut point value for "pct" which is assumed to have a normal distribution
        ind_sd <- sqrt(unit_var[ind] + index_var[ind])
        ind_cut <- qnorm(ind_pct[ind]/100, ind_values[ind], ind_sd, lower.tail = lower.tail)
        # function to search for shift in mean required to change the %Good by trend % change
        mean_change <- function(x, ind_pct, ind_cut, ind_sd, lower.tail = lower.tail) {
          abs (ind_pct/100 - pnorm(ind_cut, x, ind_sd, lower.tail = lower.tail) )
        }
        # find change in mean required to achieve change in percent
        for ( i in 1:n_periods) {
          tmp <- optimize(mean_change, lower=ind_values[ind] - 3.1 * ind_sd,
                          upper=ind_values[ind] + 3.1 * ind_sd,
                          ind_pct = ind_pct[ind] + trend[k] * (i - 1),
                          ind_cut = ind_cut, ind_sd= ind_sd,
                          lower.tail = lower.tail)
          trend_value[k, i, ind] <- tmp$minimum
        }
      }

      # power for individual panal designs
      for (j in names(paneldsgn)) {
          nperiod <- ncol(paneldsgn[[j]])
          npanel <- nrow(paneldsgn[[j]])



          # Assume increasing number of periods monitored to compute power
          for (i_period in 1:length(period_power[[j]]) ) {
            if (i_period == 1) {
              # set up initial period variables for x matrix
              period_xmat <- rep(1, npanel)
              if (trend_type == "percent") {
                period_y <- rep (trend_value[k, 1, ind], npanel)
              }
            }
            if (i_period != 1) {
              # create the covariance matrix for jth design and ith end period
              tmp <- matrix (paneldsgn[[j]][, c(as.character(period_power[[j]][1:i_period])) ] , nrow=npanel)
              tmp2 <- matrix (nrepeats[[j]][,  c(as.character(period_power[[j]][1:i_period])) ], nrow=npanel)
              panel_cov <- cov_panel_dsgn (tmp, tmp2,
                                         unit_var = unit_var[ind], period_var = period_var[ind],
                                         unitperiod_var = unitperiod_var[ind], index_var = index_var[ind],
                                         unit_rho = unit_rho, period_rho = period_rho)

              # compute the power at ith monitoring time
              # define linear trend xmat for the panel
              period_diff <- period_power[[j]][i_period] - period_power[[j]][1]
              period_xmat <- c(period_xmat,  rep(period_diff + 1, npanel) )
              if (trend_type == "percent") {
                period_y <- c(period_y, rep (trend_value[k, i_period, ind], npanel) )
              }
              xmat <- cbind(rep(1, npanel * i_period), period_xmat )

              # remove time periods with no sample units
              indx <- as.vector(tmp) > 0
              xmat <- xmat[indx,]
              if (trend_type == "percent" ) {y <- period_y[indx] }

              # calculate inverse of covariance matrix and calculate trend covariance matrix
              phi_inv <- solve(panel_cov$cov[indx,indx])
              bhat_cov <- solve(t(xmat) %*% phi_inv %*% xmat)
              if (trend_type == "percent") {
                bhat <- bhat_cov %*% t(xmat) %*% phi_inv %*% y
              }

              # calculate trend slope standard error and power
              indexse <- sqrt(bhat_cov[2,2])
              ifelse(trend_type == "mean", bhat.std <- trend_delta/ indexse,
                     bhat.std <- bhat[2] / indexse)
              }
            else {
              bhat.std <- 0
            }

            # calculate power for all alpha
            for (m in 1:length(alpha_names)) {
             pout[j, as.character(period_power[[j]][i_period]), ind_names[ind],
                  trend_names[k], alpha_names[m]]  <-
                (pnorm (qnorm (alpha[m] / 2) - bhat.std )) +
                (1 - pnorm ( qnorm(1-(alpha[m] / 2)) - bhat.std ) )
            }
          }
        }
      }
  }

  # Fill in NAs for time poriods when no sampling occurs by repeating power from
  # last time period sampled, that is, has power calculated
  pwr_fill <- function (jnk) {
    keep <- jnk[1]
    for ( i in 2:length (jnk)) {
      ifelse ( is.na (jnk[i]), jnk[i] <- keep, keep <- jnk[i])
    }
    return (jnk)
  }
  if(n_trend > 1) {
    for (i in names(paneldsgn)) {
      for (j in 1:length(ind_names)) {
        for (k in 1:length(alpha_names)) {
          pout[i, , j, , k] <- apply (pout[i, , j, , k], 2,  pwr_fill)
        }
      }
    }
  }


  # output list
  dsgn_pwr <- vector("list", 11)
  names(dsgn_pwr) <- c("design", "period", "indicator", "trend", "alpha",
                       "trend_type", "ind_pct", "ind_tail", "trend_change", "dsgn_power", "call")
  dsgn_pwr$design <- names (paneldsgn)
  dsgn_pwr$period <- periods
  dsgn_pwr$indicator <- ind_names
  dsgn_pwr$trend <- trend
  dsgn_pwr$alpha <- alpha
  dsgn_pwr$trend_type <- trend_type
  dsgn_pwr$ind_pct <- ind_pct
  dsgn_pwr$ind_tail <- ind_tail
  dsgn_pwr$trend_change <- trend_value
  dsgn_pwr$dsgn_power <- pout
  dsgn_pwr$call <- sys.call()

  class(dsgn_pwr) <- "powerpaneldesign"

  return (dsgn_pwr)
}
