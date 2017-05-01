#########################################################################
# Function: plot.powerpaneldesign.R
# Purpose: plot power functions associated with panel designs
# Programmer: Tony Olsen
# Date: March, 2017
#
#' Plot Power Curves for Panel Designs
#'
#' Plot power curves and relative power curves for trend detection for set of
#' panel designs, time periods, indicators, signifance levels and trend.  Trend
#' may be based on percent change per period in mean or percent change in
#' proportion of cumulative distribution function above or below a fixed cut
#' point. Types of plots are combinations of standard/relative, mean/percent,
#' period/change and design/indicator.  Input must be be of class
#' powerpaneldesign and is normally the output of function power.dsgn.
#'
#' @param dsgnpower  List object of class powerpaneldesign. Object provides
#'   power calculated for a set of panel designs, set of indicators, set of
#'   trend values, and set of alpha values. Expect input as list as output from
#'   function power.dsgn.
#'
#' @param plot.type   Default is "standard" which plots standard power curve. If
#'   equal to "relative", then plot power of one panel design compared to one or
#'   more other panel designs.
#'
#' @param trend.type Character value for trend in mean ("mean") or or percent
#'   change in proportion ("percent") of cumulative distribution function above
#'   or below a fixed cut point.  Default is "mean".
#'
#' @param xaxis.type Character value equal to "period" or "change" which
#'   designates the type of x-axis for power plot where power is plotted on
#'   y-axis.  For xaxis.type = "period", x-axis is periods in dsgnpower. If
#'   xaxis.type = "change", then x-axis is percent per period with secondary
#'   x-axises for total percent per period and associated change in mean.
#'   Default is "period".  Note that xaxis.type controls how the input for
#'   "period" and "trend" paramenters is used.
#'
#' @param comp.type  Character value equal to "design" or "Indicator" which
#'   designates the type of power curve comparison that will occur on a single
#'   plot.  If comp.type = "design", then on a single plot of power curves all
#'   panel designs specified in "dsgns" are plotted for a single indicator,
#'   single trend value and single alpha.  If comp.type = "indicator", then on a
#'   single plot of power curves all indicators specified in "indicator" are
#'   plotted for a single panel design, single trend value and single alpha.
#'   Default is "design".
#'
#' @param dsgns  Vector of names of panel designs that are to be plotted.  Names
#'   must be all, or a subset of, names of designs in dsgnpower. Default is NULL
#'   which results in only the first panel design in dsgnpower being used.
#'
#' @param indicator  Vector of indicator names contained in dsgnpower that are
#'   to be plotted.  Indicator names must be all, or a subset of, indicator
#'   names in dsgnpower. Default is NULL which results in only the first
#'   indicator in dsgnpower being used.
#'
#' @param trend  NULL, a single value or vector of values contained in dsgnpower
#'   that will be plotted. Values must be all, or a subset of, trend values in
#'   dsgnpower. If xaxis.type is equal to "period", then NULL results in maximum
#'   trend value being used and a single value or vector of values results in a
#'   separate plot for each value specified.  If xaxis.type is equal to
#'   "change", then NULL results in all trend values in dsgnpower being plotted
#'   on x-axis and a vector of values results in all trend values in dsgnpower
#'   from minimum value to maximum value specified being plotted on x-axis.
#'
#' @param period NULL, a single value or vector of values contained in dsgnpower
#'   that will be plotted. Values must be all, or a subset of, period values in
#'   dsgnpower. If xaxis.type is equal to "period", then NULL results in all
#'   time periods in dsgnpower being plotted on x-axis and a vector of values
#'   results in all period values in dsgnpower from minimum value to maximum
#'   value specified being plotted on x-axis. If xaxis.type is equal to
#'   "change", then NULL results in all time periods in dsgnpower being plotted
#'   in separate plots and a vector of values results in time periods specified
#'   being plotted in separate plots.
#'
#' @param alpha A single value or vector of significance levels (as proportion,
#'   e.g. 0.05) contained in dsgnpower to used for power plots. Specifying more
#'   than a single value results in multiple plots. Default is NULL which
#'   results in the minimum significance level in dsgnpower being used.
#'
#' @details By default the plot function produces a standard power curve at end
#'   of each time period on the x-axis with y-axis as power. When more than one
#'   panel design is in dsgnpower, the first panel design is used When more than
#'   one indicatoris in dsgnpower, the first indicator is used  When more than
#'   one trend value is in dsgnpower, the maximum trend value is used. When more
#'   than one significance level, alpha, is in dsgnpower, the minimum
#'   significance level is used.
#'
#'   Control of the type of plot produced is governed by plot.type, trend.type,
#'   xaxis.type and comp.type. The number of plots produced is governed by the
#'   number of panel designs (dsgn) specified, the number of indicators
#'   (indicator) specified, the number of time periods (period) specifice, the
#'   number of trend values (trend) specified and the number of significance
#'   levels (alpha) specified.
#'
#'   When the comparison type ("comp.type") is equal to "design", all power curves specified
#'   by dsgn are plotted on the same plot.  When comp.type is equal to "indicator",
#'   all power curves specified by "indicator" are plotted on the same plot.
#'   Typically, no more than 4-5 power curves should be plotted on same plot.
#'
#' @return  One or more power curve plots are created and plotted.  User must
#'   specify output graphical device if more than one plot is created.  See
#'   Devices for graphical output options.
#'
#' @examples
#'
#' ## construct rotating panel design with sample size of 60
#' R60N <- revisit_dsgn (20, panels=list(R60N=list(n=60, pnl_dsgn = c(1, NA), pnl_n=NA,
#'                                                start_option="None")), begin=1 )
#' ## construct fixed panel design with sample size of 60
#' F60 <- revisit_dsgn (20, panels=list(F60=list(n=60, pnl_dsgn = c(1, 0), pnl_n=NA,
#'                                                start_option="None")), begin=1 )
#' ## power for rotating panel with sample size 60
#' Power.tst <- power.dsgn("Variable_Name", ind.values = 43, unit.var = 280,
#'                         period.var = 4, unitperiod.var = 40, index.var = 90, unit.rho = 1,
#'                         period.rho = 0, paneldsgn = list(R60N=R60N, F60=F60), nrepeats = NULL,
#'                         trend.type = "mean", trend= c(1.0, 2.0), alpha=0.05 )
#' plot (Power.tst)
#' plot (Power.tst, dsgns = c("F60", "R60N"))
#' plot (Power.tst, dsgns = c("F60", "R60N"), trend = 1.0)
#' ## Not Run
#' # pdf("Power.tst.pdf")
#' # plot(Power.tst,  plot.type = "relative", comp.type = "design",
#' #         trend.type = "mean", trend = c(1, 2),
#' #        dsgns = c("R60N", "F60"), indicator="Variable_Name")
#' # graphics.off()
#'
#' @export
#'
#############################################################################


plot.powerpaneldesign <- function(dsgnpower, plot.type = "standard", trend.type = "mean",
                                  xaxis.type = "period", comp.type = "design",
                                  dsgns = NULL, indicator = NULL, trend = NULL,
                                  period = NULL, alpha = NULL){

  # preserve current plot parameters
  oldpar <- par (mar = c(5.1, 4.1, 0.1, 0.1), oma = c(0, 0.1, 2.1, 0.1), xpd = TRUE)

  # extract names from dsgnpower
  dsgn.names <- dimnames (dsgnpower$dsgn.power)[[1]]
  period.names <- dimnames (dsgnpower$dsgn.power)[[2]]
  ind.names <- dimnames (dsgnpower$dsgn.power)[[3]]
  trend.names <- dimnames (dsgnpower$dsgn.power)[[4]]
  alpha.names <- dimnames (dsgnpower$dsgn.power)[[5]]

  # extract periods covered by panel designs
  period.values <- dsgnpower$period
  period.min <- min (period.values)
  period.max <- max (period.values)

  # check on plot.type to use
  if ( !(plot.type %in% c("standard", "relative")) ) {
    stop ("\nplot.type is not standard or relative")
  }

  # check on trend.type
  if ( !(trend.type %in% c("mean", "percent")) ) {
    stop ("\ntrend.type is not mean or percent")
  }

  # check on xaxis type
  if (!(xaxis.type  %in% c("period", "change"))) {
    stop ("\nType plot comparison must be period or change")
  }

  # check on comparison type for plot
  if (!(comp.type  %in% c("design", "indicator"))) {
      stop ("\nType plot comparison must be design or indicator.")
    }

  # check on designs to use
  if (!is.null (dsgns)) {
    if (any (!(dsgns %in% dsgn.names)) ) {
      stop ("\nOne or more dsgn names not present in dsgnpower")
    }
    dsgn.plot <- dsgns
  }
  else {
    dsgn.plot <- dsgn.names[1]
  }

  # check on indicators to use
  if (!is.null (indicator)) {
    if (any (!(indicator %in% ind.names)) ) {
      stop ("\nOne or more indicator names not present in dsgnpower")
    }
    ind.plot <- indicator
  }
  else {
    ind.plot <- ind.names[1]
  }

  # check on trend values to use
  if (!is.null (trend) ) {
    # check to see that values are in dsgnpower
    if ( ! (all (round (trend, 1) %in% round (dsgnpower$trend, 1) ) )) {
      stop ("\nOne or more trend values not present in dsgnpower")
    }
    if (xaxis.type == "period") {
      trend.value <- trend
    }
    if (xaxis.type == "change") {
      trend.value <- dsgnpower$trend[
        dsgnpower$trend >= min (trend) & dsgnpower$trend <= max (trend)]
    }
  }
  else {
    # find maximum trend value to plot
    trend.value <-  max (dsgnpower$trend)
  }
  trend.plot <- paste ("Trend_", round (trend.value, 1), "%", sep = "")
  trend.names <- trend.plot

  # check on time periods to plot
  if (!is.null (period)) {
    if (any (!(period %in% period.values)) ) {
      stop ("\nOne or more period values not present in dsgnpower")
    }
    if (xaxis.type == "period") {
      period.values <- dsgnpower$period[
        dsgnpower$period >= min (period) & dsgnpower$period <= max (period)]
      period.plot <- period.values
      if (length (period) == 1) { period.values <- dsgnpower$period}
      period.min <- min (period.values)
      period.max <- max (period.values)

    }
    if (xaxis.type == "change") {
      period.plot <- period
    }
  }
  else {
    # extract periods covered by panel designs
    period.values <- dsgnpower$period
    period.min <- min (period.values)
    period.max <- max (period.values)
    period.plot <- period.max
  }

  # check on alphas to use
  if (!is.null (alpha)) {
    if (any (!(alpha %in% dsgnpower$alpha)) ) {
      stop ("\nOne or more alpha values not present in dsgnpower")}
    alpha.value <- alpha
    alpha.plot <- paste ("alpha_", alpha, sep="")
  }
  else {
    # use minimum alpha value
    alpha.value <- min (dsgnpower$alpha)
    alpha.plot <- paste ("alpha_", min (dsgnpower$alpha), sep="")
  }

  n.dsgn <- length (dsgn.plot)
  n.period <- length (period.plot)
  n.ind <- length(ind.plot)
  n.trend <- length (trend.plot)
  n.alpha <- length (alpha.plot)

  ################################################################
  # Standard power plot section
  if (plot.type == "standard" ) {

    #########################
    # type of plot: type.comp: design
    if(comp.type == "design") {
      for (j in 1:n.alpha) {
        for (k in 1:n.ind) {

          if (xaxis.type == "period") {
            for (i in 1:n.trend) {
              # set xaxis values for period
              xset <- seq (period.min, by=1, to=period.max)

              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(0,1), xlim=c(period.min, period.max),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (0, 1, by = 0.2),
                    labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                    adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = "Power for Trend", cex = 1.5, font = 3)

              # legend for panel design power plotted
              # text identifying indicator, trend and alpha used for power
              text (period.min, 0.98, font=3, cex=.8, adj=0, paste0('Trend Type = ', trend.type ) )
              text (period.min, 0.94, font=3, cex=.8, adj=0, paste0('Trend = ', trend.value[i], "%" ) )
              text (period.min, 0.90, font=3, cex=.8, adj=0, paste0('alpha =', alpha.value[j] ) )
              text (period.min, 0.86, font=3, cex=.8, adj=0, paste0('Indicator = ', ind.plot[k] ) )
              legend (period.min, 0.82, dsgn.plot, lty=1:n.dsgn, lwd=2, seg.len=4, bty="n")

              # plot power curve for mth panel design
              ltype <- 1
              for (m in dsgn.plot[1:n.dsgn]) {
                pow <- dsgnpower$dsgn.power[m , as.character(period.values),
                                            ind.plot[k], trend.plot[i], alpha.plot[j] ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(period.values[keep], pow, lty=ltype, lwd=2)
                ltype = ltype + 1
              }
            }
          }
          #######
          if (xaxis.type == "change") {
            for (i in 1:n.period) {
              # set xaxis values for trend
              x <- trend.value
              xset <- trend.value
              xmin <- min (xset)
              xmax <- max (xset)
              xset.tot <- (period.plot[i] -  period.min + 1) * xset
              xset.mean <- dsgnpower$trend.change[paste ("Trend_", round (x, 1), "%", sep = "") ,
                                            as.character(period.plot[i]), ind.plot[k]]
              xlabel <- paste ("Trend: %/period; Total % and Mean at Period ", period.plot[i], sep="")
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(0, 1), xlim=c(xmin,xmax),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  0, at = xset, tick = FALSE,
                    labels = round(xset.tot, 1), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  1, at = xset, tick = FALSE,
                      labels = round(xset.mean, 1), adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (0, 1, by = 0.2),
                    labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                    adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 3.5, text = xlabel, cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = "Power for Trend",
                    cex = 1.5, font = 3, col= "Black", adj = 0.5)

              # text identifying indicator, trend and alpha used for power
              text (xmin, 0.98, font=3, cex=.8, adj=0, paste('Trend Type = ', trend.type, sep="" ) )
              text (xmin, 0.94, font=3, cex=.8, adj=0, paste('End Period = ', period.plot[i], sep="" ) )
              text (xmin, 0.90, font=3, cex=.8, adj=0, paste('alpha =', alpha.value[j] ) )
              text (xmin, 0.86, font=3, cex=.8, adj=0, paste('Indicator = ', ind.plot[k] ) )
              # legend for panel design power plotted
              legend (xmin, 0.82, dsgn.plot, lty=1:n.dsgn, lwd=2, seg.len=4, bty="n")

              # plot power curve for mth panel design
              ltype <- 1
              for (m in dsgn.plot[1:n.dsgn]) {
                pow <- dsgnpower$dsgn.power[m , as.character(period.plot[i]),
                                            ind.plot[k], trend.names, alpha.plot]
                keep <- !is.na (pow)
                pow <- pow[keep]
                lines(x[keep], pow, lty=ltype, lwd=2)
                ltype = ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end type.comp: design

    #######################################
    # type of plot: type.comp: indicator
    if(comp.type == "indicator") {
      for (j in 1:n.alpha) {
        for (k in 1:n.dsgn) {

          if (xaxis.type == "period") {
            for (i in 1:n.trend) {
              # set xaxis values for period
              xset <- seq (period.min, by=1, to=period.max)

              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(0,1), xlim=c(period.min, period.max),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (0, 1, by = 0.2),
                    labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                    adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 1.8, text = "End period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = "Power for Trend", cex = 1.5, font = 3)

              # legend for panel design power plotted
              # text identifying indicator, trend and alpha used for power
              text (period.min, 0.98, font=3, cex=.8, adj=0, paste ('Trend Type = ', trend.type, sep="" ) )
              text (period.min, 0.95, font=3, cex=.8, adj=0, paste ("Trend = ", trend.value[i], sep="" ))
              text (period.min, 0.92, font=3, cex=.8, adj=0, paste ("Alpha = ", alpha.value[j], sep="" ))
              text (period.min, 0.88, font=3, cex=.8, adj=0, paste ("Design = ", dsgn.plot[k], sep="" ))
              legend (period.min, 0.85, ind.plot, lty=1:n.ind, lwd=2, seg.len=4, bty="n")

              # plot power curve for mth indicator
              ltype <- 1
              for (m in ind.plot[1:n.ind]) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k] , as.character(period.values),
                                            m , trend.plot[i], alpha.plot[j] ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(period.values[keep], pow, lty=ltype, lwd=2)
                ltype = ltype + 1
              }
            }
          }
          #######
          if (xaxis.type == "change") {
            for (i in 1:n.period) {
              # set xaxis values for trend
              x <- trend.value
              xset <- trend.value
              xmin <- min (xset)
              xmax <- max (xset)
              xset.tot <- (period.plot[i] -  period.min + 1) * xset
              if (n.ind == 1) {
                xset.mean <- dsgnpower$trend.change[paste ("Trend_", x, "%", sep = "") ,
                                            as.character(period.plot[i]), ind.plot[k]]
                xlabel <- paste ("Trend: %/period; Total % and Mean at Period ", period.plot[i], sep="")
              }
              else {
                xlabel <- paste ("Trend: %/period and Total % at Period ", period.plot[i], sep="")
              }
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(0, 1), xlim=c(xmin,xmax),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  0, at = xset, tick = FALSE,
                    labels = round(xset.tot, 1), adj = 0.5, font = 3, cex = 1)
              if (n.ind == 1) {
                axis (side = 1, line =  1, at = xset, tick = FALSE,
                      labels = round(xset.mean, 1), adj = 0.5, font = 3, cex = 1)
              }
              axis (side = 2, line = 0, at = seq (0, 1, by = 0.2),
                    labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
                    adj=0.85, font=3, cex=1)
              mtext(outer = FALSE, side = 1, line = 3.5, text = xlabel, cex = 1.5, font = 3)
              mtext(outer = FALSE, side = 2, line = 2.5, text = "Power for Trend",
                    cex = 1.5, font = 3, col= c("Black"), adj = c( 0.5))

              # text identifying indicator, trend and alpha used for power
              text (xmin, 0.98, font=3, cex=.8, adj=0, paste('Trend Type = ', trend.type, sep="" ) )
              text (xmin, 0.94, font=3, cex=.8, adj=0, paste('End Period = ', period.plot[i], sep="" ) )
              text (xmin, 0.90, font=3, cex=.8, adj=0, paste('alpha =', alpha.value[j] ) )
              text (xmin, 0.86, font=3, cex=.8, adj=0, paste('Panel Design = ', dsgn.plot[k] ) )
              # legend for indicator power plotted
              legend (xmin, 0.82, ind.plot, lty=1:n.ind, lwd=2, seg.len=4, bty="n")

              # plot power curve for mth indicator
              ltype <- 1
              for (m in ind.plot[1:n.ind]) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k] , as.character(period.plot[i]),
                                            m, trend.names, alpha.plot[j] ]
                keep <- !is.na(pow)
                pow <- pow[keep]
                lines(x[keep], pow, lty=ltype, lwd=2)
                ltype = ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp.type: indicator

  }

  ##################################################
  # Relative power plot section
  if (plot.type == "relative") {
    # type of plot: type.comp: design
    if(comp.type == "design") {
      for (j in 1:n.alpha) {
        for (k in 1:n.ind) {

          if (xaxis.type == "period") {
            for (i in 1:n.trend) {
              # Determine y-axis minimum
              ymin <- - 0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[1], as.character(period.values),
                                               ind.plot[k], trend.plot[i], alpha.plot[j] ]
              for (m in dsgn.plot) {
                pow <- dsgnpower$dsgn.power[m, as.character(period.values), ind.plot[k],
                                            trend.plot[i], alpha.plot[j] ]
                ymin <- min (ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling ( - ymin * 10)
              ifelse (ylow %% 2 == 0, ylow <- - ylow / 10, ylow <- - ylow / 10 - 0.1)
              # set axix values for period
              xset <- seq (period.min, by=1, to=period.max)
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(ylow, 1), xlim=c(period.min, period.max),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (ylow, 1, by = 0.2),
                    labels =  round (seq (ylow, 1, by = 0.2), 1), adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = c("Relative Power","Power for Trend"),
                    cex = 1.5, font = 3, col= c("Black", "Blue"), adj = c(0.1, 0.8))

              # text identifying indicator, trend and alpha used for power
              text (period.min, 0.98, font=3, cex=.8, adj=0, paste0('Trend Type = ', trend.type) )
              text (period.min, 0.94, font=3, cex=.8, adj=0, paste0('Trend = ', trend.value[i]) )
              text (period.min, 0.90, font=3, cex=.8, adj=0, paste0('alpha =', alpha.value[j] ) )
              text (period.min, 0.86, font=3, cex=.8, adj=0, paste0('Indicator = ', ind.plot[k] ) )
              # legend for panel design power plotted
              legend (period.min, 0.82, c(dsgn.plot[1], dsgn.plot), lty=c(1,1:n.dsgn),
                      col = c("blue", rep("black", length(dsgn.plot))), lwd=2, seg.len=5, bty="n")

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[1], as.character(period.values),
                                               ind.plot[k], trend.plot[i], alpha.plot[j] ]
              keep <- !is.na(base_pow)
              lines(period.values[keep], base_pow[keep], lty=1, col="blue", lwd=2)

              ltype <- 1
              # plot relative power
              for (m in dsgn.plot) {
                pow <- dsgnpower$dsgn.power[m, as.character(period.values), ind.plot[k],
                                            trend.plot[i], alpha.plot[j] ]
                pow_diff <- pow - base_pow
                keep <- !is.na(pow_diff)
                lines(period.values[keep], pow_diff[keep], lty=ltype, lwd=2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis.type == "change") {
            for (i in 1:n.period) {
              # Determine y-axis minimum
              ymin <- - 0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[1], as.character(period.plot[i]),
                                               ind.plot[k], trend.names, alpha.plot[j] ]
              for (m in dsgn.plot) {
                pow <- dsgnpower$dsgn.power[m, as.character(period.plot[i]),
                                            ind.plot[k], trend.names, alpha.plot[j] ]
                ymin <- min (ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling ( - ymin * 10)
              ifelse (ylow %% 2 == 0, ylow <- - ylow / 10, ylow <- - ylow / 10 - 0.1)
              # set xaxis values for trend
              x <- trend.value
              xset <- trend.value
              xmin <- min (xset)
              xmax <- max (xset)
              xset.tot <- (period.plot[i] -  period.min + 1) * xset
              xset.mean <- dsgnpower$trend.change[trend.names , as.character(period.plot[i]), ind.plot[k]]
              xlabel <- paste ("Trend: %/Period; Total % and Mean at Period ", period.plot[i], sep="")
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(ylow, 1), xlim=c(xmin,xmax),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  0, at = xset, tick = FALSE,
                    labels = round(xset.tot, 1), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  1, at = xset, tick = FALSE,
                    labels = round(xset.mean, 1), adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (ylow, 1, by = 0.2),
                    labels = round (seq (ylow, 1, by = 0.2), 1), adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 3.2, text = xlabel, cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = c("Relative Power","Power for Trend"),
                    cex = 1.5, font = 3, col= c("Black", "Blue"), adj = c(0.1, 0.8))

              # text identifying indicator, trend and alpha used for power
              text (xmin, 0.98, font=3, cex=.8, adj=0, paste0('Trend Type = ', trend.type) )
              text (xmin, 0.94, font=3, cex=.8, adj=0, paste0('End  Period = ', period.plot[i] ) )
              text (xmin, 0.90, font=3, cex=.8, adj=0, paste0('alpha =', alpha.value[j] ) )
              text (xmin, 0.86, font=3, cex=.8, adj=0, paste0('Indicator = ', ind.plot[k] ) )
              # legend for panel design power plotted
              legend (xmin, 0.82, c(dsgn.plot[1], dsgn.plot), lty=c(1,1:n.dsgn),
                      col = c("blue", rep("black", length(dsgn.plot))), lwd=2, seg.len=5, bty="n")

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[1], as.character(period.plot[i]),
                                               ind.plot[k], trend.names, alpha.plot[j] ]
              lines(x[!is.na(base_pow)], base_pow[!is.na(base_pow)], lty=1, col="blue", lwd=2)
              # plot relative power
              ltype <- 1
              for (m in dsgn.plot) {
                pow <- dsgnpower$dsgn.power[m, as.character(period.plot[i]),
                                            ind.plot[k], trend.names, alpha.plot[j] ]
                pow_diff <- pow - base_pow
                lines(x[!is.na(pow_diff)], pow_diff[!is.na(pow_diff)], lty=ltype, lwd=2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp.type: dsgn

    #######################################
    # type of plot: type.comp: indicator
    if(comp.type == "indicator") {
      for (j in 1:n.alpha) {
        for (k in 1:n.dsgn) {

          if (xaxis.type == "period") {
            for (i in 1:n.trend) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.values),
                                               ind.plot[1], trend.plot[i], alpha.plot[j] ]
              for (m in ind.plot) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.values),
                                            m, trend.plot[i], alpha.plot[j] ]
                ymin <- min (ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling ( - ymin * 10)
              ifelse (ylow %% 2 == 0, ylow <- - ylow / 10, ylow <- - ylow / 10 - 0.1)
              # set axix values for period
              xset <- seq (period.min, by=1, to=period.max)
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(ylow, 1), xlim=c(period.min, period.max),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -0.75, at = xset, labels = xset, adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (ylow, 1, by = 0.2),
                    labels = round (seq (ylow, 1, by = 0.2), 1), adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 1.8, text = "End Period", cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = c("Relative Power","Power for Trend"),
                    cex = 1.5, font = 3, col= c("Black", "Blue"), adj = c(0.1, 0.8))

              # text identifying indicator, trend and alpha used for power
              text (period.min, 0.98, font=3, cex=.8, adj=0, paste0('Trend Type = ', trend.type ) )
              text (period.min, 0.94, font=3, cex=.8, adj=0, paste0('Trend = ', round(trend.value[i],1), "%") )
              text (period.min, 0.90, font=3, cex=.8, adj=0, paste0('alpha =', alpha.value[j] ) )
              text (period.min, 0.86, font=3, cex=.8, adj=0, paste0('Panel Design = ', dsgn.plot[k] ) )
              # legend for panel design power plotted
              legend (period.min, 0.82, c(ind.plot[1], ind.plot), lty=c(1,1:n.ind),
                      col = c("blue", rep("black", length(ind.plot))), lwd=2, seg.len=5, bty="n")

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.values),
                                               ind.plot[1], trend.plot[i], alpha.plot[j] ]
              keep <- !is.na(base_pow)
              lines(period.values[keep], base_pow[keep], lty=1, col="blue", lwd=2)

              ltype <- 1
              # plot relative power
              for (m in ind.plot) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.values),
                                            m, trend.plot[i], alpha.plot[j] ]
                pow_diff <- pow - base_pow
                keep <- !is.na(pow_diff)
                lines(period.values[keep], pow_diff[keep], lty=ltype, lwd=2)
                ltype <- ltype + 1
              }
            }
          }
          #######
          if (xaxis.type == "change") {
            for (i in 1:n.period) {
              # Determine y-axis minimum
              ymin <- -0.2
              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.plot[i]),
                                               ind.plot[1], trend.names, alpha.plot[j] ]
              for (m in ind.plot) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.plot[i]),
                                            m, trend.names, alpha.plot[j] ]
                ymin <- min (ymin, pow - base_pow, na.rm = TRUE)
              }
              ylow <- ceiling ( - ymin * 10)
              ifelse (ylow %% 2 == 0, ylow <- - ylow / 10, ylow <- - ylow / 10 - 0.1)
              # set xaxis values for trend
              x <- trend.value
              xset <- trend.value
              xmin <- min (xset)
              xmax <- max (xset)
              xset.tot <- (period.plot[i] -  period.min + 1) * xset
              xlabel <- paste ("Trend: %/Period; Total % at Period ", period.plot[i], sep="")
              # set up initial plot area,  xaxis and yaxis
              plot (xset, seq(0,1,length=length(xset)), ylim=c(ylow, 1), xlim=c(xmin,xmax),
                    ylab="", xlab="", type="n", axes=F)
              axis (side = 1, line = -1, at = xset, labels = round(xset, 2), adj = 0.5, font = 3, cex = 1)
              axis (side = 1, line =  0, at = xset, tick = FALSE,
                    labels = round(xset.tot, 1), adj = 0.5, font = 3, cex = 1)
              axis (side = 2, line = 0, at = seq (ylow, 1, by = 0.2),
                    labels = round( seq (ylow, 1, by = 0.2), 1), adj=0.85, font=3, cex=1)
              mtext(outer = F, side = 1, line = 2.2, text = xlabel, cex = 1.5, font = 3)
              mtext(outer = F, side = 2, line = 2.5, text = c("Relative Power","Power for Trend"),
                    cex = 1.5, font = 3, col= c("Black", "Blue"), adj = c(0.1, 0.8))

              # text identifying indicator, trend and alpha used for power
              text (xmin, 0.98, font=3, cex=.8, adj=0, paste0('Trend Type = ', trend.type) )
              text (xmin, 0.94, font=3, cex=.8, adj=0, paste0('End Period = ', period.plot[i] ) )
              text (xmin, 0.90, font=3, cex=.8, adj=0, paste0('alpha =', alpha.value[j] ) )
              text (xmin, 0.86, font=3, cex=.8, adj=0, paste0('Panel Design = ', dsgn.plot[k] ) )
              # legend for indicator power plotted
              legend (xmin, 0.82, c(ind.plot[1], ind.plot), lty=c(1,1:n.ind),
                      col = c("blue", rep("black", length(ind.plot))), lwd=2, seg.len=5, bty="n")

              # find panel_base power and plot power curve
              base_pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.plot[i]),
                                               ind.plot[1], trend.names, alpha.plot[j] ]
              lines(x[!is.na(base_pow)], base_pow[!is.na(base_pow)], lty=1, col="blue", lwd=2)
              # plot relative power
              ltype <- 1
              for (m in ind.plot) {
                pow <- dsgnpower$dsgn.power[dsgn.plot[k], as.character(period.plot[i]),
                                            m, trend.names, alpha.plot[j] ]
                pow_diff <- pow - base_pow
                lines(x[!is.na(pow_diff)], pow_diff[!is.na(pow_diff)], lty=ltype, lwd=2)
                ltype <- ltype + 1
              }
            }
          }
        }
      }
    }
    ####### end comp.type: indicator
  }

  # end of relative power plot section

  # reset plot parameters
  par(oldpar)

}
#########################################################################
