################################################################################
# Function: cdf_plot (not exported)
# Programmer Tom Kincaid
# Date: May 5, 2021
#'
#' Plot a Cumulatiave Distribution Function
#'
#' This function creates a CDF plot.  Input data for the plots is provided by a
#' data frame utilizing the same structure as the data frame named "CDF" that is
#' included in the output object produced by function cont.analysis, but the
#' data frame includes only the values for a single CDF.  Confidence limits for
#' the CDF also are plotted.
#'
#' @param cdfest Data frame utilizing the same structure as the data frame
#'   named "CDF" that is included in the output object produced by function
#'   cont.analysis.  The data frame must contain only a single cdf estimate.
#'
#' @param units_cdf Indicator for the type of units in which the CDF is
#'   plotted, where "Percent" means the plot is in terms of percent of the
#'   population, and "Units" means the plot is in terms of units of the
#'   population.  The default is "Percent".
#'
#' @param type_cdf Character string consisting of the value "Continuous" or
#'   "Ordinal" that controls the type of CDF plot for each indicator.  The
#'   default is "Continuous".
#'
#' @param logx Character string consisting of the value "" or "x" that
#'   controls whether the x axis uses the original scale ("") or the base 10
#'   logarithmic scale ("x").  The default is "".
#'
#' @param xlbl Character string providing the x-axis label.  If this argument
#'   equals NULL, then the indicator name is used as the label.  The default is
#'   NULL.
#'
#' @param ylbl Character string providing the y-axis label.  The default is
#'   "Percent".
#'
#' @param ylbl_r Character string providing the label for the right side
#'   y-axis, where NULL means a label is not created, and "Same" means the label
#'   is the same as the left side label (i.e., argument ylbl).  The default is
#'   NULL.
#'
#' @param figlab Character string providing the plot title.  The default is
#'   NULL.
#'
#' @param legloc  Indicator for location of the plot legend, where "BR" means
#'   bottom right, "BL" means bottom left, "TR" means top right, and "TL" means
#'   top left.  The default is "BR".
#'
#' @param confcut Numeric value that controls plotting confidence limits at
#'   the CDF extremes.  Confidence limits for CDF values (percent scale) less
#'   than confcut or greater than 100 minus confcut are not plotted.  A value of
#'   zero means confidence limits are plotted for the complete range of the CDF.
#'   The default is 5.
#'
#' @param conflev Numeric value of the confidence level used for confidence
#'   limits. The default is 95.
#'
#' @param cex.main Expansion factor for the plot title.  The default is 1.2.
#'
#' @param  ... Additional arguments passed to the plot function.
#'
#' @return A plot of the CDF and its associated confidence limits.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' dframe <- data.frame(
#'   siteID = paste0("Site", 1:100),
#'   wgt = runif(100, 10, 100),
#'   xcoord = runif(100),
#'   ycoord = runif(100),
#'   stratum = rep(c("Stratum1", "Stratum2"), 50),
#'   ContVar = rnorm(100, 10, 1),
#'   All_Sites = rep("All Sites", 100),
#'   Resource_Class = rep(c("Good", "Poor"), c(55, 45))
#' )
#' myvars <- c("ContVar")
#' mysubpops <- c("All_Sites", "Resource_Class")
#' mypopsize <- data.frame(
#'   Resource_Class = c("Good", "Poor"),
#'   Total = c(4000, 1500)
#' )
#' myanalysis <- cont_analysis(dframe,
#'   vars = myvars, subpops = mysubpops,
#'   siteID = "siteID", weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
#'   stratumID = "stratum", popsize = mypopsize
#' )
#' keep <- with(myanalysis$CDF, Type == "Resource_Class" &
#'   Subpopulation == "Good")
#' par(mfrow = c(2, 1))
#' cdf_plot(myanalysis$CDF[keep, ],
#'   xlbl = "ContVar",
#'   ylbl = "Percent of Stream Length", ylbl_r = "Stream Length (km)",
#'   figlab = "Estimates for Resource Class: Good"
#' )
#' cdf_plot(myanalysis$CDF[keep, ],
#'   xlbl = "ContVar",
#'   ylbl = "Percent of Stream Length", ylbl_r = "Same",
#'   figlab = "Estimates for Resource Class: Good"
#' )
#' @noRd
################################################################################

cdf_plot <- function(cdfest, units_cdf = "Percent", type_cdf = "Continuous",
                     logx = "", xlbl = NULL, ylbl = "Percent", ylbl_r = NULL, figlab = NULL,
                     legloc = "BR", confcut = 5, conflev = 95, cex.main = 1.2, ...) {

  # Set graphical parameter values

  op <- par(mgp = c(1.7, 0.6, 0), mar = c(3, 3, 2, 4) + 0.1)

  # Create the data frame of values to be plotted and set the y-axis limits
  # The data frame structure follows: column 1: x-axis values
  #                                   column 2: CDF estimates for left y-axis
  #                                   column 3: lower confidence limit values
  #                                   column 4: upper confidence limit values
  #                                   column 5: CDF estimates for right y-axis

  if (units_cdf == "Percent") {
    cdfdata <- cdfest[, c(4, 6, 9, 10, 11)]
  } else if (units_cdf == "Units") {
    cdfdata <- cdfest[, c(4, 11, 14, 15, 6)]
  } else {
    stop(paste("\nThe choice of units for the CDF must be either \"Percent\" or \"Units\". The value \nsupplied for argument units_cdf was: \"", units_cdf, "\".\n", sep = ""))
  }

  # Restrict confidence limits to lie between confcut and 100-confcut percent

  pctval <- c(confcut, 100 - confcut)
  tvalue <- cdfest[, 6] >= pctval[1] & cdfest[, 6] <= pctval[2]
  x <- interp_cdf(pctval, cdfest[, 6], cdfdata[, 1])
  ylow <- interp_cdf(pctval, cdfest[, 6], cdfdata[, 3])
  yhi <- interp_cdf(pctval, cdfest[, 6], cdfdata[, 4])

  # Set the left side y-axis limits

  if (units_cdf == "Percent") {
    ylimit <- c(0, 100)
  } else if (units_cdf == "Units") {
    ylimit <- pretty(c(min(c(cdfdata[, 2], ylow)), max(c(cdfdata[, 2], yhi))))
    ylimit <- ylimit[c(1, length(ylimit))]
  }

  # Plot the CDF for a continuous indicator

  if (type_cdf == "Continuous") {
    plot(cdfdata[, 1], cdfdata[, 2],
      type = "l", ylim = ylimit, xlab = xlbl, ylab = ylbl,
      log = logx, ...
    )

    # Plot confidence limits

    value <- c(x[1], cdfdata[, 1][tvalue], x[2])
    lower <- c(ylow[1], cdfdata[, 3][tvalue], ylow[2])
    upper <- c(yhi[1], cdfdata[, 4][tvalue], yhi[2])
    lines(value, lower, lty = 3, lwd = 1.5)
    lines(value, upper, lty = 3, lwd = 1.5)

    # Plot the CDF for an ordinal (count) indicator
  } else if (type_cdf == "Ordinal") {
    x <- rep(cdfdata[, 1], each = 2)[-1]
    y <- rep(cdfdata[, 2], each = 2)
    tmp <- cbind(matrix(c(x, x[length(x)]), ncol = 2, byrow = TRUE), rep(NA, nrow(cdfdata)))
    x <- as.vector(t(tmp))
    tmp <- cbind(matrix(y, ncol = 2, byrow = TRUE), rep(NA, nrow(cdfdata)))
    y <- as.vector(t(tmp))
    plot(x, y, type = "l", ylim = ylimit, xlab = xlbl, ylab = ylbl, ...)

    # Plot confidence limits

    len <- length(cdfdata[, 1][tvalue])
    if (len > 1) {
      value <- rep(cdfdata[, 1][tvalue], each = 2)[-1]
      tmp <- cbind(
        matrix(c(value, value[length(value)]), ncol = 2, byrow = TRUE),
        rep(NA, len)
      )
      value <- as.vector(t(tmp))
      len <- length(cdfdata[, 4][tvalue])
      if (len > 1) {
        lower <- rep(cdfdata[, 3][tvalue], each = 2)
        tmp <- cbind(matrix(lower, ncol = 2, byrow = TRUE), rep(NA, len))
        lower <- as.vector(t(tmp))
        upper <- rep(cdfdata[, 4][tvalue], each = 2)
        tmp <- cbind(matrix(upper, ncol = 2, byrow = TRUE), rep(NA, len))
        upper <- as.vector(t(tmp))
        lines(value, lower, lty = 3, lwd = 1.5)
        lines(value, upper, lty = 3, lwd = 1.5)
      }
    }
  } else {
    stop(paste("\nThe type of CDF must be either \"Continuous\" or \"Ordinal\". The value supplied \nfor argument type_cdf was: \"", type_cdf, "\".\n", sep = ""))
  }

  # Create the plot title

  title(figlab, line = 1, cex.main = cex.main)

  # Create the plot legend

  rx <- range(par("usr")[1:2], cdfdata[, 1])
  ry <- range(par("usr")[3:4], cdfdata[, 2])
  if (legloc == "BR") {
    xjust <- 1
    yjust <- 0
    legx <- rx[2]
    legy <- ry[1]
  } else if (legloc == "BL") {
    xjust <- 0
    yjust <- 0
    legx <- rx[1]
    legy <- ry[1]
  } else if (legloc == "TR") {
    xjust <- 1
    yjust <- 1
    legx <- rx[2]
    legy <- ry[2]
  } else if (legloc == "TL") {
    xjust <- 0
    yjust <- 1
    legx <- rx[1]
    legy <- ry[2]
  }
  legend(
    x = legx, y = legy, xjust = xjust, yjust = yjust,
    legend = c("CDF Estimate", paste(conflev, "% Confidence Limits", sep = "")),
    lty = c(1, 3), lwd = c(1, 1.5), bty = "n", cex = 1
  )

  # If requested, create the right side y-axis labels

  if (!is.null(ylbl_r)) {
    yl.lab <- seq(par("yaxp")[1], par("yaxp")[2], len = par("yaxp")[3] + 1)
    if (ylbl_r == "Same") {
      axis(side = 4, at = yl.lab, labels = yl.lab)
      mtext(ylbl, side = 4, line = 2, cex = par("cex"))
    } else {
      yr.lab <- interp_axis(yl.lab, cdfdata[, 2], cdfdata[, 5])
      axis(side = 4, at = yl.lab, labels = as.character(round(yr.lab)))
      mtext(ylbl_r, side = 4, line = 2, cex = par("cex"))
    }
  }

  # Reset graphical parameter values

  par(op)

  invisible(NULL)
}
