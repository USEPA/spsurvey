###############################################################################
# Function: plot.powerpaneldesign (exported)
# Programmer: Tony Olsen
# Date: March 14, 2019
#
#' Plot Power Curves for Panel Designs
#'
#' Plot power curves and relative power curves for trend detection for set of
#' panel designs, time periods, indicators, signifance levels and trend.  Trend
#' may be based on percent change per period in mean or percent change in
#' proportion of cumulative distribution function above or below a fixed cut
#' point. Types of plots are combinations of standard/relative, mean/percent,
#' period/change and design/indicator.  Input must be be of class
#' powerpaneldesign and is normally the output of function power_dsgn.
#'
#' @param x  List object of class \code{powerpaneldesign}. Object provides
#'   power calculated for a set of panel designs, set of indicators, set of
#'   trend values, and set of alpha values. Expect input as list as output from
#'   function \code{power_dsgn}.
#'
#' @param y \code{NULL} argument.
#'
#' @param plot_type   Default is \code{"standard"} which plots standard power curve. If
#'   equal to \code{"relative"}, then plot power of one panel design compared to one or
#'   more other panel designs.
#'
#' @param trend_type Character value for trend in mean (\code{"mean"}) or or percent
#'   change in proportion (\code{"percent"}) of cumulative distribution function above
#'   or below a fixed cut point.  Default is \code{"mean"}.
#'
#' @param xaxis_type Character value equal to \code{"period"} or \code{"change"} which
#'   designates the type of x-axis for power plot where power is plotted on
#'   y-axis.  For \code{xaxis_type = "period"}, x-axis is periods in \code{dsgnpower}. If
#'   \code{xaxis_type = "change"}, then x-axis is percent per period with secondary
#'   x-axises for total percent per period and associated change in mean.
#'   Default is \code{"period"}.  Note that \code{xaxis_type} controls how the input for
#'   \code{"period"} and \code{"trend"} paramenters is used.
#'
#' @param comp_type  Character value equal to \code{"design"} or \code{"Indicator"} which
#'   designates the type of power curve comparison that will occur on a single
#'   plot.  If \code{comp_type = "design"}, then on a single plot of power curves all
#'   panel designs specified in \code{"dsgns"} are plotted for a single indicator,
#'   single trend value and single alpha.  If \code{comp_type = "indicator"}, then on a
#'   single plot of power curves all indicators specified in \code{"indicator"} are
#'   plotted for a single panel design, single trend value and single alpha.
#'   Default is \code{"design"}.
#'
#' @param dsgns  Vector of names of panel designs that are to be plotted.  Names
#'   must be all, or a subset of, names of designs in \code{dsgnpower}. Default is \code{NULL}
#'   which results in only the first panel design in \code{dsgnpower} being used.
#'
#' @param indicator  Vector of indicator names contained in \code{dsgnpower} that are
#'   to be plotted.  Indicator names must be all, or a subset of, indicator
#'   names in \code{dsgnpower}. Default is \code{NULL} which results in only the first
#'   indicator in \code{dsgnpower} being used.
#'
#' @param trend  \code{NULL}. A single value or vector of values contained in \code{dsgnpower}
#'   that will be plotted. Values must be all, or a subset of, trend values in
#'   \code{dsgnpower}. If \code{xaxis_type} is equal to \code{"period"}, then \code{NULL} results in maximum
#'   trend value being used and a single value or vector of values results in a
#'   separate plot for each value specified.  If \code{xaxis_type} is equal to
#'   \code{"change"}, then \code{NULL} results in all trend values in \code{dsgnpower} being plotted
#'   on x-axis and a vector of values results in all trend values in \code{dsgnpower}
#'   from minimum value to maximum value specified being plotted on x-axis.
#'
#' @param period \code{NULL}, a single value or vector of values contained in \code{dsgnpower}
#'   that will be plotted. Values must be all, or a subset of, period values in
#'   \code{dsgnpower}. If \code{xaxis_type} is equal to \code{"period"}, then \code{NULL} results in all
#'   time periods in \code{dsgnpower} being plotted on x-axis and a vector of values
#'   results in all period values in \code{dsgnpower} from minimum value to maximum
#'   value specified being plotted on x-axis. If \code{xaxis_type} is equal to
#'   \code{"change"}, then \code{NULL} results in all time periods in \code{dsgnpower} being plotted
#'   in separate plots and a vector of values results in time periods specified
#'   being plotted in separate plots.
#'
#' @param alpha A single value or vector of significance levels (as proportion,
#'   e.g. \code{0.05}) contained in \code{dsgnpower} to used for power plots. Specifying more
#'   than a single value results in multiple plots. Default is \code{NULL} which
#'   results in the minimum significance level in \code{dsgnpower} being used.
#'
#' @details By default the plot function produces a standard power curve at end
#'   of each time period on the x-axis with y-axis as power. When more than one
#'   panel design is in \code{dsgnpower}, the first panel design is used. When more than
#'   one indicatoris in \code{dsgnpower}, the first indicator is used.  When more than
#'   one trend value is in \code{dsgnpower}, the maximum trend value is used. When more
#'   than one significance level, \code{alpha}, is in \code{dsgnpower}, the minimum
#'   significance level is used.
#'
#'   Control of the type of plot produced is governed by \code{plot_type}, \code{trend_type},
#'   \code{xaxis_type} and \code{comp_type}. The number of plots produced is governed by the
#'   number of panel designs (\code{dsgn}) specified, the number of indicators
#'   (\code{indicator}) specified, the number of time periods (\code{period}) specifice, the
#'   number of trend values (trend) specified and the number of significance
#'   levels (\code{alpha}) specified.
#'
#'   When the comparison type (\code{"comp_type"}) is equal to \code{"design"}, all power
#'   curves specified by dsgn are plotted on the same plot.  When \code{comp_type} is
#'   equal to \code{"indicator"}, all power curves specified by \code{"indicator"} are plotted
#'   on the same plot.  Typically, no more than 4-5 power curves should be
#'   plotted on same plot.
#'
#' @return  One or more power curve plots are created and plotted.  User must
#'   specify output graphical device if more than one plot is created.  See
#'   Devices for graphical output options.
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
#'     \item{\code{\link{power_dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov_panel_dsgn}}}{covariance matrix for a panel design}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Construct a rotating panel design with sample size of 60
#' R60N <- revisit_dsgn(20, panels = list(R60N = list(
#'   n = 60, pnl_dsgn = c(1, NA),
#'   pnl_n = NA, start_option = "None"
#' )), begin = 1)
#'
#' # Construct a fixed panel design with sample size of 60
#' F60 <- revisit_dsgn(20, panels = list(F60 = list(
#'   n = 60, pnl_dsgn = c(1, 0),
#'   pnl_n = NA, start_option = "None"
#' )), begin = 1)
#'
#' # Power for rotating panel with sample size 60
#' Power_tst <- power_dsgn("Variable_Name",
#'   ind_values = 43, unit_var = 280,
#'   period_var = 4, unitperiod_var = 40, index_var = 90,
#'   unit_rho = 1, period_rho = 0, paneldsgn = list(
#'     R60N = R60N, F60 = F60
#'   ), nrepeats = NULL,
#'   trend_type = "mean", trend = c(1.0, 2.0), alpha = 0.05
#' )
#' plot(Power_tst)
#' plot(Power_tst, dsgns = c("F60", "R60N"))
#' plot(Power_tst, dsgns = c("F60", "R60N"), trend = 1.0)
#' \dontrun{
#' pdf("Power_tst.pdf")
#' plot(Power_tst,
#'   plot_type = "relative", comp_type = "design",
#'   trend_type = "mean", trend = c(1, 2), dsgns = c("R60N", "F60"),
#'   indicator = "Variable_Name"
#' )
#' graphics.off()
#' }
#'
#' @export
###############################################################################
plot.powerpaneldesign <- function(x, y = NULL, plot_type = "standard",
                                  trend_type = "mean", xaxis_type = "period", comp_type = "design",
                                  dsgns = NULL, indicator = NULL, trend = NULL, period = NULL, alpha = NULL) {
  dsgnpower <- x
  # preserve current plot parameters
  oldpar <- par(mar = c(5.1, 4.1, 0.1, 0.1), oma = c(0, 0.1, 2.1, 0.1), xpd = TRUE)

  # extract names from dsgnpower
  dsgn_names <- dimnames(dsgnpower$dsgn_power)[[1]]
  period_names <- dimnames(dsgnpower$dsgn_power)[[2]]
  ind_names <- dimnames(dsgnpower$dsgn_power)[[3]]
  trend_names <- dimnames(dsgnpower$dsgn_power)[[4]]
  alpha_names <- dimnames(dsgnpower$dsgn_power)[[5]]

  # extract periods covered by panel designs
  period_values <- dsgnpower$period
  period_min <- min(period_values)
  period_max <- max(period_values)

  # check on plot_type to use
  if (!(plot_type %in% c("standard", "relative"))) {
    stop("\nplot_type is not standard or relative")
  }

  # check on trend_type
  if (!(trend_type %in% c("mean", "percent"))) {
    stop("\ntrend_type is not mean or percent")
  }

  # check on xaxis type
  if (!(xaxis_type %in% c("period", "change"))) {
    stop("\nType plot comparison must be period or change")
  }

  # check on comparison type for plot
  if (!(comp_type %in% c("design", "indicator"))) {
    stop("\nType plot comparison must be design or indicator.")
  }

  # check on designs to use
  if (!is.null(dsgns)) {
    if (any(!(dsgns %in% dsgn_names))) {
      stop("\nOne or more dsgn names not present in dsgnpower")
    }
    dsgn_plot <- dsgns
  }
  else {
    dsgn_plot <- dsgn_names[1]
  }

  # check on indicators to use
  if (!is.null(indicator)) {
    if (any(!(indicator %in% ind_names))) {
      stop("\nOne or more indicator names not present in dsgnpower")
    }
    ind_plot <- indicator
  }
  else {
    ind_plot <- ind_names[1]
  }

  # check on trend values to use
  if (!is.null(trend)) {
    # check to see that values are in dsgnpower
    if (!(all(round(trend, 1) %in% round(dsgnpower$trend, 1)))) {
      stop("\nOne or more trend values not present in dsgnpower")
    }
    if (xaxis_type == "period") {
      trend_value <- trend
    }
    if (xaxis_type == "change") {
      trend_value <- dsgnpower$trend[
        dsgnpower$trend >= min(trend) & dsgnpower$trend <= max(trend)
      ]
    }
  }
  else {
    # find maximum trend value to plot
    trend_value <- max(dsgnpower$trend)
  }
  trend_plot <- paste("Trend_", round(trend_value, 1), "%", sep = "")
  trend_names <- trend_plot

  # check on time periods to plot
  if (!is.null(period)) {
    if (any(!(period %in% period_values))) {
      stop("\nOne or more period values not present in dsgnpower")
    }
    if (xaxis_type == "period") {
      period_values <- dsgnpower$period[
        dsgnpower$period >= min(period) & dsgnpower$period <= max(period)
      ]
      period_plot <- period_values
      if (length(period) == 1) {
        period_values <- dsgnpower$period
      }
      period_min <- min(period_values)
      period_max <- max(period_values)
    }
    if (xaxis_type == "change") {
      period_plot <- period
    }
  }
  else {
    # extract periods covered by panel designs
    period_values <- dsgnpower$period
    period_min <- min(period_values)
    period_max <- max(period_values)
    period_plot <- period_max
  }

  # check on alphas to use
  if (!is.null(alpha)) {
    if (any(!(alpha %in% dsgnpower$alpha))) {
      stop("\nOne or more alpha values not present in dsgnpower")
    }
    alpha_value <- alpha
    alpha_plot <- paste("alpha_", alpha, sep = "")
  }
  else {
    # use minimum alpha value
    alpha_value <- min(dsgnpower$alpha)
    alpha_plot <- paste("alpha_", min(dsgnpower$alpha), sep = "")
  }

  n_dsgn <- length(dsgn_plot)
  n_period <- length(period_plot)
  n_ind <- length(ind_plot)
  n_trend <- length(trend_plot)
  n_alpha <- length(alpha_plot)

  ################################################################
  # Standard power plot section
  if (plot_type == "standard") {

    #########################
    # type of plot: type.comp: design
    if (comp_type == "design") {
      for (j in 1:n_alpha) {
        for (k in 1:n_ind) {
          if (xaxis_type == "period") {
            for (i in 1:n_trend) {
              # set xaxis values for period
              xset <- seq(period_min, by = 1, to = period_max)

              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(0, 1), xlim = c(period_min, period_max),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis(
                side = 2, line = 0, at = seq(0, 1, by = 0.2),
                labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = "Power for Trend", cex = 1.5, font = 3)

              # legend for panel design power plotted
              # text identifying indicator, trend and alpha used for power
              text(period_min, 0.98, font = 3, cex = .8, adj = 0, paste0("Trend Type = ", trend_type))
              text(period_min, 0.94, font = 3, cex = .8, adj = 0, paste0("Trend = ", trend_value[i], "%"))
              text(period_min, 0.90, font = 3, cex = .8, adj = 0, paste0("alpha =", alpha_value[j]))
              text(period_min, 0.86, font = 3, cex = .8, adj = 0, paste0("Indicator = ", ind_plot[k]))
              legend(period_min, 0.82, dsgn_plot, lty = 1:n_dsgn, lwd = 2, seg.len = 4, bty = "n")

              # plot power curve for mth panel design
              ltype <- 1
              for (m in dsgn_plot[1:n_dsgn]) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_values),
                  ind_plot[k], trend_plot[i], alpha_plot[j]
                ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(period_values[keep], pow, lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis_type == "change") {
            for (i in 1:n_period) {
              # set xaxis values for trend
              x <- trend_value
              xset <- trend_value
              xmin <- min(xset)
              xmax <- max(xset)
              xset_tot <- (period_plot[i] - period_min + 1) * xset
              xset_mean <- dsgnpower$trend.change[
                paste("Trend_", round(x, 1), "%", sep = ""),
                as.character(period_plot[i]), ind_plot[k]
              ]
              xlabel <- paste("Trend: %/period; Total % and Mean at Period ", period_plot[i], sep = "")
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(0, 1), xlim = c(xmin, xmax),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis(
                side = 1, line = 0, at = xset, tick = FALSE,
                labels = round(xset_tot, 1), adj = 0.5, font = 3, cex = 1
              )
              axis(
                side = 1, line = 1, at = xset, tick = FALSE,
                labels = round(xset_mean, 1), adj = 0.5, font = 3, cex = 1
              )
              axis(
                side = 2, line = 0, at = seq(0, 1, by = 0.2),
                labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 3.5, text = xlabel, cex = 1.5, font = 3)
              mtext(
                outer = F, side = 2, line = 2.5, text = "Power for Trend",
                cex = 1.5, font = 3, col = "Black", adj = 0.5
              )

              # text identifying indicator, trend and alpha used for power
              text(xmin, 0.98, font = 3, cex = .8, adj = 0, paste("Trend Type = ", trend_type, sep = ""))
              text(xmin, 0.94, font = 3, cex = .8, adj = 0, paste("End Period = ", period_plot[i], sep = ""))
              text(xmin, 0.90, font = 3, cex = .8, adj = 0, paste("alpha =", alpha_value[j]))
              text(xmin, 0.86, font = 3, cex = .8, adj = 0, paste("Indicator = ", ind_plot[k]))
              # legend for panel design power plotted
              legend(xmin, 0.82, dsgn_plot, lty = 1:n_dsgn, lwd = 2, seg.len = 4, bty = "n")

              # plot power curve for mth panel design
              ltype <- 1
              for (m in dsgn_plot[1:n_dsgn]) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_plot[i]),
                  ind_plot[k], trend_names, alpha_plot
                ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(x[keep], pow, lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end type.comp: design

    #######################################
    # type of plot: type.comp: indicator
    if (comp_type == "indicator") {
      for (j in 1:n_alpha) {
        for (k in 1:n_dsgn) {
          if (xaxis_type == "period") {
            for (i in 1:n_trend) {
              # set xaxis values for period
              xset <- seq(period_min, by = 1, to = period_max)

              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(0, 1), xlim = c(period_min, period_max),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis(
                side = 2, line = 0, at = seq(0, 1, by = 0.2),
                labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 1.8, text = "End period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = "Power for Trend", cex = 1.5, font = 3)

              # legend for panel design power plotted
              # text identifying indicator, trend and alpha used for power
              text(period_min, 0.98, font = 3, cex = .8, adj = 0, paste("Trend Type = ", trend_type, sep = ""))
              text(period_min, 0.95, font = 3, cex = .8, adj = 0, paste("Trend = ", trend_value[i], sep = ""))
              text(period_min, 0.92, font = 3, cex = .8, adj = 0, paste("Alpha = ", alpha_value[j], sep = ""))
              text(period_min, 0.88, font = 3, cex = .8, adj = 0, paste("Design = ", dsgn_plot[k], sep = ""))
              legend(period_min, 0.85, ind_plot, lty = 1:n_ind, lwd = 2, seg.len = 4, bty = "n")

              # plot power curve for mth indicator
              ltype <- 1
              for (m in ind_plot[1:n_ind]) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_values),
                  m, trend_plot[i], alpha_plot[j]
                ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(period_values[keep], pow, lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis_type == "change") {
            for (i in 1:n_period) {
              # set xaxis values for trend
              x <- trend_value
              xset <- trend_value
              xmin <- min(xset)
              xmax <- max(xset)
              xset_tot <- (period_plot[i] - period_min + 1) * xset
              if (n_ind == 1) {
                xset_mean <- dsgnpower$trend.change[
                  paste("Trend_", x, "%", sep = ""),
                  as.character(period_plot[i]), ind_plot[k]
                ]
                xlabel <- paste("Trend: %/period; Total % and Mean at Period ", period_plot[i], sep = "")
              }
              else {
                xlabel <- paste("Trend: %/period and Total % at Period ", period_plot[i], sep = "")
              }
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(0, 1), xlim = c(xmin, xmax),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis(
                side = 1, line = 0, at = xset, tick = FALSE,
                labels = round(xset_tot, 1), adj = 0.5, font = 3, cex = 1
              )
              if (n_ind == 1) {
                axis(
                  side = 1, line = 1, at = xset, tick = FALSE,
                  labels = round(xset_mean, 1), adj = 0.5, font = 3, cex = 1
                )
              }
              axis(
                side = 2, line = 0, at = seq(0, 1, by = 0.2),
                labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = FALSE, side = 1, line = 3.5, text = xlabel, cex = 1.5, font = 3)
              mtext(
                outer = FALSE, side = 2, line = 2.5, text = "Power for Trend",
                cex = 1.5, font = 3, col = c("Black"), adj = c(0.5)
              )

              # text identifying indicator, trend and alpha used for power
              text(xmin, 0.98, font = 3, cex = .8, adj = 0, paste("Trend Type = ", trend_type, sep = ""))
              text(xmin, 0.94, font = 3, cex = .8, adj = 0, paste("End Period = ", period_plot[i], sep = ""))
              text(xmin, 0.90, font = 3, cex = .8, adj = 0, paste("alpha =", alpha_value[j]))
              text(xmin, 0.86, font = 3, cex = .8, adj = 0, paste("Panel Design = ", dsgn_plot[k]))
              # legend for indicator power plotted
              legend(xmin, 0.82, ind_plot, lty = 1:n_ind, lwd = 2, seg.len = 4, bty = "n")

              # plot power curve for mth indicator
              ltype <- 1
              for (m in ind_plot[1:n_ind]) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_plot[i]),
                  m, trend_names, alpha_plot[j]
                ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(x[keep], pow, lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp_type: indicator
  }

  ##################################################
  # Relative power plot section
  if (plot_type == "relative") {
    # type of plot: type.comp: design
    if (comp_type == "design") {
      for (j in 1:n_alpha) {
        for (k in 1:n_ind) {
          if (xaxis_type == "period") {
            for (i in 1:n_trend) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[1], as.character(period_values),
                ind_plot[k], trend_plot[i], alpha_plot[j]
              ]
              for (m in dsgn_plot) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_values), ind_plot[k],
                  trend_plot[i], alpha_plot[j]
                ]
                ymin <- min(ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling(-ymin * 10)
              ifelse(ylow %% 2 == 0, ylow <- -ylow / 10, ylow <- -ylow / 10 - 0.1)
              # set axix values for period
              xset <- seq(period_min, by = 1, to = period_max)
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(ylow, 1), xlim = c(period_min, period_max),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis(
                side = 2, line = 0, at = seq(ylow, 1, by = 0.2),
                labels = round(seq(ylow, 1, by = 0.2), 1), adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(
                outer = F, side = 2, line = 2.5, text = c("Relative Power", "Power for Trend"),
                cex = 1.5, font = 3, col = c("Black", "Blue"), adj = c(0.1, 0.8)
              )

              # text identifying indicator, trend and alpha used for power
              text(period_min, 0.98, font = 3, cex = .8, adj = 0, paste0("Trend Type = ", trend_type))
              text(period_min, 0.94, font = 3, cex = .8, adj = 0, paste0("Trend = ", trend_value[i]))
              text(period_min, 0.90, font = 3, cex = .8, adj = 0, paste0("alpha =", alpha_value[j]))
              text(period_min, 0.86, font = 3, cex = .8, adj = 0, paste0("Indicator = ", ind_plot[k]))
              # legend for panel design power plotted
              legend(period_min, 0.82, c(dsgn_plot[1], dsgn_plot),
                lty = c(1, 1:n_dsgn),
                col = c("blue", rep("black", length(dsgn_plot))), lwd = 2, seg.len = 5, bty = "n"
              )

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[1], as.character(period_values),
                ind_plot[k], trend_plot[i], alpha_plot[j]
              ]
              keep <- !is.na(base_pow)
              lines(period_values[keep], base_pow[keep], lty = 1, col = "blue", lwd = 2)

              ltype <- 1
              # plot relative power
              for (m in dsgn_plot) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_values), ind_plot[k],
                  trend_plot[i], alpha_plot[j]
                ]
                pow_diff <- pow - base_pow
                keep <- !is.na(pow_diff)
                lines(period_values[keep], pow_diff[keep], lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis_type == "change") {
            for (i in 1:n_period) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[1], as.character(period_plot[i]),
                ind_plot[k], trend_names, alpha_plot[j]
              ]
              for (m in dsgn_plot) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_plot[i]),
                  ind_plot[k], trend_names, alpha_plot[j]
                ]
                ymin <- min(ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling(-ymin * 10)
              ifelse(ylow %% 2 == 0, ylow <- -ylow / 10, ylow <- -ylow / 10 - 0.1)
              # set xaxis values for trend
              x <- trend_value
              xset <- trend_value
              xmin <- min(xset)
              xmax <- max(xset)
              xset_tot <- (period_plot[i] - period_min + 1) * xset
              xset_mean <- dsgnpower$trend.change[trend_names, as.character(period_plot[i]), ind_plot[k]]
              xlabel <- paste("Trend: %/Period; Total % and Mean at Period ", period_plot[i], sep = "")
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(ylow, 1), xlim = c(xmin, xmax),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis(
                side = 1, line = 0, at = xset, tick = FALSE,
                labels = round(xset_tot, 1), adj = 0.5, font = 3, cex = 1
              )
              axis(
                side = 1, line = 1, at = xset, tick = FALSE,
                labels = round(xset_mean, 1), adj = 0.5, font = 3, cex = 1
              )
              axis(
                side = 2, line = 0, at = seq(ylow, 1, by = 0.2),
                labels = round(seq(ylow, 1, by = 0.2), 1), adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 3.2, text = xlabel, cex = 1.5, font = 3)
              mtext(
                outer = F, side = 2, line = 2.5, text = c("Relative Power", "Power for Trend"),
                cex = 1.5, font = 3, col = c("Black", "Blue"), adj = c(0.1, 0.8)
              )

              # text identifying indicator, trend and alpha used for power
              text(xmin, 0.98, font = 3, cex = .8, adj = 0, paste0("Trend Type = ", trend_type))
              text(xmin, 0.94, font = 3, cex = .8, adj = 0, paste0("End  Period = ", period_plot[i]))
              text(xmin, 0.90, font = 3, cex = .8, adj = 0, paste0("alpha =", alpha_value[j]))
              text(xmin, 0.86, font = 3, cex = .8, adj = 0, paste0("Indicator = ", ind_plot[k]))
              # legend for panel design power plotted
              legend(xmin, 0.82, c(dsgn_plot[1], dsgn_plot),
                lty = c(1, 1:n_dsgn),
                col = c("blue", rep("black", length(dsgn_plot))), lwd = 2, seg.len = 5, bty = "n"
              )

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[1], as.character(period_plot[i]),
                ind_plot[k], trend_names, alpha_plot[j]
              ]
              lines(x[!is.na(base_pow)], base_pow[!is.na(base_pow)], lty = 1, col = "blue", lwd = 2)
              # plot relative power
              ltype <- 1
              for (m in dsgn_plot) {
                pow <- dsgnpower$dsgn_power[
                  m, as.character(period_plot[i]),
                  ind_plot[k], trend_names, alpha_plot[j]
                ]
                pow_diff <- pow - base_pow
                lines(x[!is.na(pow_diff)], pow_diff[!is.na(pow_diff)], lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp_type: dsgn

    #######################################
    # type of plot: type.comp: indicator
    if (comp_type == "indicator") {
      for (j in 1:n_alpha) {
        for (k in 1:n_dsgn) {
          if (xaxis_type == "period") {
            for (i in 1:n_trend) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[k], as.character(period_values),
                ind_plot[1], trend_plot[i], alpha_plot[j]
              ]
              for (m in ind_plot) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_values),
                  m, trend_plot[i], alpha_plot[j]
                ]
                ymin <- min(ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling(-ymin * 10)
              ifelse(ylow %% 2 == 0, ylow <- -ylow / 10, ylow <- -ylow / 10 - 0.1)
              # set axix values for period
              xset <- seq(period_min, by = 1, to = period_max)
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(ylow, 1), xlim = c(period_min, period_max),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis(
                side = 2, line = 0, at = seq(ylow, 1, by = 0.2),
                labels = round(seq(ylow, 1, by = 0.2), 1), adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(
                outer = F, side = 2, line = 2.5, text = c("Relative Power", "Power for Trend"),
                cex = 1.5, font = 3, col = c("Black", "Blue"), adj = c(0.1, 0.8)
              )

              # text identifying indicator, trend and alpha used for power
              text(period_min, 0.98, font = 3, cex = .8, adj = 0, paste0("Trend Type = ", trend_type))
              text(period_min, 0.94, font = 3, cex = .8, adj = 0, paste0("Trend = ", round(trend_value[i], 1), "%"))
              text(period_min, 0.90, font = 3, cex = .8, adj = 0, paste0("alpha =", alpha_value[j]))
              text(period_min, 0.86, font = 3, cex = .8, adj = 0, paste0("Panel Design = ", dsgn_plot[k]))
              # legend for panel design power plotted
              legend(period_min, 0.82, c(ind_plot[1], ind_plot),
                lty = c(1, 1:n_ind),
                col = c("blue", rep("black", length(ind_plot))), lwd = 2, seg.len = 5, bty = "n"
              )

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[k], as.character(period_values),
                ind_plot[1], trend_plot[i], alpha_plot[j]
              ]
              keep <- !is.na(base_pow)
              lines(period_values[keep], base_pow[keep], lty = 1, col = "blue", lwd = 2)

              ltype <- 1
              # plot relative power
              for (m in ind_plot) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_values),
                  m, trend_plot[i], alpha_plot[j]
                ]
                pow_diff <- pow - base_pow
                keep <- !is.na(pow_diff)
                lines(period_values[keep], pow_diff[keep], lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis_type == "change") {
            for (i in 1:n_period) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[k], as.character(period_plot[i]),
                ind_plot[1], trend_names, alpha_plot[j]
              ]
              for (m in ind_plot) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_plot[i]),
                  m, trend_names, alpha_plot[j]
                ]
                ymin <- min(ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling(-ymin * 10)
              ifelse(ylow %% 2 == 0, ylow <- -ylow / 10, ylow <- -ylow / 10 - 0.1)
              # set xaxis values for trend
              x <- trend_value
              xset <- trend_value
              xmin <- min(xset)
              xmax <- max(xset)
              xset_tot <- (period_plot[i] - period_min + 1) * xset
              xlabel <- paste("Trend: %/Period; Total % at Period ", period_plot[i], sep = "")
              # set up initial plot area,  xaxis and yaxis
              plot(xset, seq(0, 1, length = length(xset)),
                ylim = c(ylow, 1), xlim = c(xmin, xmax),
                ylab = "", xlab = "", type = "n", axes = F
              )
              axis(side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis(
                side = 1, line = 0, at = xset, tick = FALSE,
                labels = round(xset_tot, 1), adj = 0.5, font = 3, cex = 1
              )
              axis(
                side = 2, line = 0, at = seq(ylow, 1, by = 0.2),
                labels = round(seq(ylow, 1, by = 0.2), 1), adj = 0.85, font = 3, cex = 1
              )
              mtext(outer = F, side = 1, line = 2.2, text = xlabel, cex = 1.5, font = 3)
              mtext(
                outer = F, side = 2, line = 2.5, text = c("Relative Power", "Power for Trend"),
                cex = 1.5, font = 3, col = c("Black", "Blue"), adj = c(0.1, 0.8)
              )

              # text identifying indicator, trend and alpha used for power
              text(xmin, 0.98, font = 3, cex = .8, adj = 0, paste0("Trend Type = ", trend_type))
              text(xmin, 0.94, font = 3, cex = .8, adj = 0, paste0("End Period = ", period_plot[i]))
              text(xmin, 0.90, font = 3, cex = .8, adj = 0, paste0("alpha =", alpha_value[j]))
              text(xmin, 0.86, font = 3, cex = .8, adj = 0, paste0("Panel Design = ", dsgn_plot[k]))
              # legend for indicator power plotted
              legend(xmin, 0.82, c(ind_plot[1], ind_plot),
                lty = c(1, 1:n_ind),
                col = c("blue", rep("black", length(ind_plot))), lwd = 2, seg.len = 5, bty = "n"
              )

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn_power[
                dsgn_plot[k], as.character(period_plot[i]),
                ind_plot[1], trend_names, alpha_plot[j]
              ]
              lines(x[!is.na(base_pow)], base_pow[!is.na(base_pow)], lty = 1, col = "blue", lwd = 2)
              # plot relative power
              ltype <- 1
              for (m in ind_plot) {
                pow <- dsgnpower$dsgn_power[
                  dsgn_plot[k], as.character(period_plot[i]),
                  m, trend_names, alpha_plot[j]
                ]
                pow_diff <- pow - base_pow
                lines(x[!is.na(pow_diff)], pow_diff[!is.na(pow_diff)], lty = ltype, lwd = 2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp_type: indicator
  }

  # end of relative power plot section

  # reset plot parameters
  par(oldpar)
}
