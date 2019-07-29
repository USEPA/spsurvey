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
#' @param ind.names  Vector of indicator names
#'
#' @param ind.values  Vector of indicator mean values
#'
#' @param unit.var  Vector of variance component estimates for unit variability
#'   for the indicators
#'
#' @param period.var  Vector of variance component estimates for period
#'   variability for the indicators
#'
#' @param unitperiod.var  Vector of variance component estimates for unit by
#'   period interaction variability for the indicators
#'
#' @param index.var  Vector of variance component estimates for index (residual)
#'   error for the indicators
#'
#' @param unit.rho  Correlation across units. Default is 1
#'
#' @param period.rho  Correlation across periods. Default is 0
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
#' @param trend.type  Trend type is either "mean" where trend is applied as
#'   percent trend in the indicator mean or "percent" where the trend is applied
#'   as percent trend in the proportion (percent) of the distribution that is
#'   below or above a fixed value. Default is trend.type="mean"
#'
#' @param ind.pct  When trend.type is equal to "percent", a vector of the
#'   values of the indicator fixed value that defines the percent.  Default is
#'   NULL
#'
#' @param ind.tail  When trend.type is equal to "percent", a character vector
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
#' @return A list with components trend.type, ind.pct, ind.tail, trend values
#'   across periods, periods (all periods included in one or more panel
#'   designs), significance levels, a five-dimensional array of power
#'   calculations (dimensions: panel design names, periods, indicator names,
#'   trend names, alpha.names), an array of indicator mean values for each trend
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
#'     \item{\code{\link{cov.panel.dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Power for rotating panel with sample size 60
#' power.dsgn("Variable_Name", ind.values = 43, unit.var = 280, period.var = 4,
#'            unitperiod.var = 40, index.var = 90, unit.rho = 1, period.rho = 0,
#'            paneldsgn = list(NoR60=revisit_dsgn(20,
#'                             panels=list(NoR60=list(n=60, pnl_dsgn = c(1, NA),
#'                             pnl_n = NA, start_option = "None")), begin = 1)),
#'             nrepeats = NULL, trend.type = "mean", trend= 1.0, alpha=0.05)
#'
#' @export
################################################################################

power.dsgn <- function(ind.names, ind.values, unit.var, period.var,
   unitperiod.var, index.var, unit.rho = 1, period.rho = 0, paneldsgn,
   nrepeats = NULL, trend.type = "mean", ind.pct = NULL, ind.tail = NULL,
   trend = 2, alpha = 0.05) {

  # number of indicators for power comparison
  n.ind <- length(ind.names)

  # number of trend change values for power comparison
  n.trend <- length (trend)
  trend.names <- paste ("Trend_", round (trend, 1), "%", sep="")

  # check that trend.type has correct possible values
  if (!(trend.type %in% c("mean", "percent"))) stop ("\ntrend.type value not 'mean' or 'percent' ")

  # number of significance levels for power comparison
  n.alpha <- length (alpha)
  alpha.names <- paste ("alpha_", alpha, sep="")

  # number of designs for power comparison
  n.dsgn <- length (paneldsgn)

  # Create nrepeats revisit design if necessary
  if(is.null(nrepeats)) {
    nrepeats <- vector("list", n.dsgn)
    for (i in names (paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }

  # number of periods for power comparison
  # First set up periods to calculate trend for each panel design based on periods when units are sampled
  period.power <- vector("list", n.dsgn)
  names (period.power) <- names (paneldsgn)
  period.values <- numeric(length=0 )
  for (i in names (paneldsgn)) {
    period.power[[i]] <- ifelse ( apply (paneldsgn[[i]], 2, max) > 0, TRUE, FALSE)
    period.power[[i]] <- as.numeric(dimnames(paneldsgn[[i]])[[2]][period.power[[i]]])
    period.values <- c(period.values, period.power[[i]])
  }
  # find minimum and maximum period from designs that are monitored
  period.min <- min (period.values)
  period.max <- max (period.values)
  periods <- seq(period.min, by = 1, to=period.max)
  n.periods <- length (periods)

  # When trend.type = "percent", check if for lower or upper tail
  if (!is.null(ind.tail)) {
    if (any( !(ind.tail %in% c("lower", "upper")) ) ) stop ("\nValues must be lower or upper")
    lower.tail <- ind.tail == "lower"
  }

  # set up output array for calculated power for each indicator, panel design, periods, trend, and alpha
  pout <- array(NA, c(n.dsgn, n.periods, n.ind, n.trend, n.alpha))
  dimnames (pout) <- list(names (paneldsgn), periods, ind.names, trend.names, alpha.names)

  # set up change in mean for output and use in power calculations
  trend.value <- array(NA, c(n.trend, n.periods, n.ind))
  dimnames (trend.value) <- list(trend.names, as.character(periods), ind.names)

  # set up trend for power calculation based on percent per period and indicator value so
  # that trend is in units indicator units of change per period.

  for (ind in 1:length(ind.names)) {

    for (k in 1:length(trend.names)) {
      # calculate trend value
      if (trend.type == "mean") {
        trend.delta <- ind.values[ind] * trend[k] / 100
        trend.value[k, ,ind] <- ind.values[ind] + trend.delta * seq(0, to=n.periods-1, by=1)
        }
      if (trend.type == "percent") {
        # find cut point value for "pct" which is assumed to have a normal distribution
        ind.sd <- sqrt(unit.var[ind] + index.var[ind])
        ind.cut <- qnorm(ind.pct[ind]/100, ind.values[ind], ind.sd, lower.tail = lower.tail)
        # function to search for shift in mean required to change the %Good by trend % change
        mean.change <- function(x, ind.pct, ind.cut, ind.sd, lower.tail = lower.tail) {
          abs (ind.pct/100 - pnorm(ind.cut, x, ind.sd, lower.tail = lower.tail) )
        }
        # find change in mean required to achieve change in percent
        for ( i in 1:n.periods) {
          tmp <- optimize(mean.change, lower=ind.values[ind] - 3.1 * ind.sd,
                          upper=ind.values[ind] + 3.1 * ind.sd,
                          ind.pct = ind.pct[ind] + trend[k] * (i - 1),
                          ind.cut = ind.cut, ind.sd= ind.sd,
                          lower.tail = lower.tail)
          trend.value[k, i, ind] <- tmp$minimum
        }
      }

      # power for individual panal designs
      for (j in names(paneldsgn)) {
          nperiod <- ncol(paneldsgn[[j]])
          npanel <- nrow(paneldsgn[[j]])



          # Assume increasing number of periods monitored to compute power
          for (i.period in 1:length(period.power[[j]]) ) {
            if (i.period == 1) {
              # set up initial period variables for x matrix
              period.xmat <- rep(1, npanel)
              if (trend.type == "percent") {
                period.y <- rep (trend.value[k, 1, ind], npanel)
              }
            }
            if (i.period != 1) {
              # create the covariance matrix for jth design and ith end period
              tmp <- matrix (paneldsgn[[j]][, c(as.character(period.power[[j]][1:i.period])) ] , nrow=npanel)
              tmp2 <- matrix (nrepeats[[j]][,  c(as.character(period.power[[j]][1:i.period])) ], nrow=npanel)
              panel.cov <- cov.panel.dsgn (tmp, tmp2,
                                         unit.var = unit.var[ind], period.var = period.var[ind],
                                         unitperiod.var = unitperiod.var[ind], index.var = index.var[ind],
                                         unit.rho = unit.rho, period.rho = period.rho)

              # compute the power at ith monitoring time
              # define linear trend xmat for the panel
              period.diff <- period.power[[j]][i.period] - period.power[[j]][1]
              period.xmat <- c(period.xmat,  rep(period.diff + 1, npanel) )
              if (trend.type == "percent") {
                period.y <- c(period.y, rep (trend.value[k, i.period, ind], npanel) )
              }
              xmat <- cbind(rep(1, npanel * i.period), period.xmat )

              # remove time periods with no sample units
              indx <- as.vector(tmp) > 0
              xmat <- xmat[indx,]
              if (trend.type == "percent" ) {y <- period.y[indx] }

              # calculate inverse of covariance matrix and calculate trend covariance matrix
              phi.inv <- solve(panel.cov$cov[indx,indx])
              bhat.cov <- solve(t(xmat) %*% phi.inv %*% xmat)
              if (trend.type == "percent") {
                bhat <- bhat.cov %*% t(xmat) %*% phi.inv %*% y
              }

              # calculate trend slope standard error and power
              indexse <- sqrt(bhat.cov[2,2])
              ifelse(trend.type == "mean", bhat.std <- trend.delta/ indexse,
                     bhat.std <- bhat[2] / indexse)
              }
            else {
              bhat.std <- 0
            }

            # calculate power for all alpha
            for (m in 1:length(alpha.names)) {
             pout[j, as.character(period.power[[j]][i.period]), ind.names[ind],
                  trend.names[k], alpha.names[m]]  <-
                (pnorm (qnorm (alpha[m] / 2) - bhat.std )) +
                (1 - pnorm ( qnorm(1-(alpha[m] / 2)) - bhat.std ) )
            }
          }
        }
      }
  }

  # Fill in NAs for time poriods when no sampling occurs by repeating power from
  # last time period sampled, that is, has power calculated
  pwr.fill <- function (jnk) {
    keep <- jnk[1]
    for ( i in 2:length (jnk)) {
      ifelse ( is.na (jnk[i]), jnk[i] <- keep, keep <- jnk[i])
    }
    return (jnk)
  }
  if(n.trend > 1) {
    for (i in names(paneldsgn)) {
      for (j in 1:length(ind.names)) {
        for (k in 1:length(alpha.names)) {
          pout[i, , j, , k] <- apply (pout[i, , j, , k], 2,  pwr.fill)
        }
      }
    }
  }


  # output list
  dsgn.pwr <- vector("list", 11)
  names(dsgn.pwr) <- c("design", "period", "indicator", "trend", "alpha",
                       "trend.type", "ind.pct", "ind.tail", "trend.change", "dsgn.power", "call")
  dsgn.pwr$design <- names (paneldsgn)
  dsgn.pwr$period <- periods
  dsgn.pwr$indicator <- ind.names
  dsgn.pwr$trend <- trend
  dsgn.pwr$alpha <- alpha
  dsgn.pwr$trend.type <- trend.type
  dsgn.pwr$ind.pct <- ind.pct
  dsgn.pwr$ind.tail <- ind.tail
  dsgn.pwr$trend.change <- trend.value
  dsgn.pwr$dsgn.power <- pout
  dsgn.pwr$call <- sys.call()

  class(dsgn.pwr) <- "powerpaneldesign"

  return (dsgn.pwr)
}
