################################################################################
# Function: cont_cdfplot (exported)
# Programmer Tom Kincaid
# Date: May 5, 2021
# Revised: May 6, 2021 to correct an error when specifying argument type_cdf for
#          function cdf_plot
# Revised: August 31, 2021 to improve documentation
#
#' Create a PDF file containing cumulative distribution functions (CDF) plots
#'
#' This function creates a PDF file containing CDF plots.  Input data for the
#' plots is provided by a data frame utilizing the same structure as the data
#' frame named "CDF" that is included in the output object produced by function
#' \code{cont_analysis}.  Plots are produced for every combination of Type of
#' population, subpopulation within Type, and indicator.
#'
#' @param pdffile Name of the PDF file.  The default is "cdf2x2.pdf".
#'
#' @param cdfest Data frame utilizing the same structure as the data frame
#'   named "CDF" that is included in the output object produced by function
#'   \code{cont_analysis}.
#'
#' @param units_cdf Indicator for the label utilized for the left side y-axis
#'   and the values used for the left side y-axis tick marks, where "Percent"
#'   means the label and values are in terms of percent of the population, and
#'   "Units" means the label and values are in terms of units (count, length,
#'   or area) of the population.  The default is "Percent".
#'
#' @param ind_type Character vector consisting of the values "Continuous" or
#'   "Ordinal" that controls the type of CDF plot for each indicator.  The
#'   default is "Continuous" for every indicator.
#'
#' @param log Character vector consisting of the values "" or "x" that
#'   controls whether the x axis uses the original scale ("") or the base 10
#'   logarithmic scale ("x") for each indicator.  The default is "" for every
#'   indicator.
#'
#' @param xlab Character vector consisting of the x-axis label for each
#'   indicator. If this argument equals NULL, then indicator names are used as
#'   the labels. The default is NULL.
#'
#' @param ylab Character string providing the left side y-axis label.  If
#'   argument units_cdf equals "Units", a value should be provided for this
#'   argument.  Otherwise, the label will be "Percent".  The default is
#'   "Percent".
#'
#' @param ylab_r Character string providing the label for the right side y-axis
#'   (and, hence, determining the values used for the right side y-axis tick
#'   marks), where NULL means a right side y-axis is not created.  If this
#'   argument equals "Same", the right side y-axis will have the same label and
#'   tick mark values as the left side y-axis.  If this argument equals a
#'   character string other than "Same", the right side y-axis label will be the
#'   value provided for argument ylab_r, and the right side y-axis tick mark
#'   values will be determined by the choice not utilized for argument
#'   units_cdf, which means that the default value of argument units_cdf (i.e.,
#'   "Percent") will result in the right side y-axis tick mark values being
#'   expressed  in terms of units of the population (i.e., count, length, or
#'   area).  The default is NULL.
#'
#' @param legloc Indicator for location of the plot legend, where "BR" means
#'   bottom right, "BL" means bottom left, "TR" means top right, and "TL" means
#'   top left.  The default is "BR".
#'
#' @param cdf_page  Number of CDF plots on each page, which must be chosen from
#'   the values: 1, 2, 4, or 6.  The default is 4.
#'
#' @param width Width of the graphic region in inches.  The default is 10.
#'
#' @param height Height of the graphic region in inches.  The default is 8.
#'
#' @param confcut Numeric value that controls plotting confidence limits at
#'   the CDF extremes.  Confidence limits for CDF values (percent scale) less
#'   than confcut or greater than 100 minus confcut are not plotted.  A value of
#'   zero means confidence limits are plotted for the complete range of the CDF.
#'   The default is 5.
#'
#' @param cex.main Expansion factor for the plot title.  The default is 1.2.
#'
#' @param cex.legend Expansion factor for the legend title. The default is 1.
#'
#' @param ... Additional arguments passed to the \code{cdf_plot} function.
#'
#' @return A PDF file containing the CDF plots.
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{cdf_plot}}}{for plotting a cumulative distribution
#'     function (CDF)}
#'   \item{\code{\link{cont_cdftest}}}{for CDF hypothesis testing}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey plot
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
#' \dontrun{
#' cont_cdfplot("myanalysis.pdf", myanalysis$CDF, ylab_r = "Stream Length (km)")
#' }
#'
#' @export
################################################################################

cont_cdfplot <- function(
  pdffile = "cdf2x2.pdf", cdfest, units_cdf = "Percent",
  ind_type = rep("Continuous", nind), log = rep("", nind), xlab = NULL,
  ylab = NULL, ylab_r = NULL, legloc = NULL, cdf_page = 4, width = 10,
  height = 8, confcut = 5, cex.main = 1.2, cex.legend = 1, ...
) {

  # Open the PDF file

  pdf(file = pdffile, width = width, height = height)

  # Set up the number of plots per page

  if (cdf_page == 1) {
    mf <- c(1, 1)
  } else if (cdf_page == 2) {
    mf <- c(2, 1)
  } else if (cdf_page == 4) {
    mf <- c(2, 2)
  } else if (cdf_page == 6) {
    mf <- c(3, 2)
  } else {
    mf <- c(2, 2)
  }

  # Set graphical parameter values

  op <- par(mfrow = mf, mgp = c(1.5, 0.7, 0))

  # Assign the vectors of population type names and indicator names

  typenames <- unique(cdfest$Type)
  indnames <- unique(cdfest$Indicator)
  nind <- length(indnames)

  # If not supplied, set up the x-axis labels

  if (is.null(xlab)) {
    xlab <- as.character(indnames)
    names(xlab) <- as.character(indnames)
  }

  # Obtain the confidence level

  conflev <- as.numeric(substr(names(cdfest)[9], 4, 5))

  # Create the plots

  for (itype in 1:length(typenames)) {
    tsttype <- cdfest$Type == typenames[itype]
    subnames <- unique(cdfest$Subpopulation[tsttype])
    for (jsub in 1:length(subnames)) {
      tstsub <- tsttype & cdfest$Subpopulation == subnames[jsub]
      temp <- match(unique(cdfest$Indicator[tstsub]), indnames)
      for (kin in temp) {
        tstind <- tstsub & cdfest$Indicator == indnames[kin]
        cdf_plot(
          cdfest[tstind, ], units_cdf = units_cdf, type_cdf = ind_type[kin],
          log = log[kin], xlab = xlab[indnames[kin]], ylab = ylab,
          ylab_r = ylab_r, main = paste0(typenames[itype], " - ",
            subnames[jsub], ": ", indnames[kin]),
          legloc = legloc, confcut = confcut, conflev = conflev,
          cex.main = cex.main, cex.legend = cex.legend, ...
        )
      }
    }
  }


  # Reset graphical parameter values

  par(op)

  # Close the PDF file

  graphics.off()

  # Return an invisible NULL object

  invisible(NULL)
}
