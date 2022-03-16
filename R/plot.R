###############################################################################
# Function: plot (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Plot sampling frames, design sites, and analysis data.
#'
#' This function plots sampling frames, design sites, and analysis data.
#' If the left-hand side of the formula is empty, plots
#' are of the distributions of the right-hand side variables. If the left-hand side
#' of the variable contains a variable, plots are of the left-hand size variable
#' for each level of each right-hand side variable.
#' This function is largely built on \code{plot.sf()}, and all spsurvey plotting
#' methods can supply additional arguments to \code{plot.sf()}. For more information on
#' plotting in \code{sf}, run \code{?sf::plot.sf()}. Equivalent to \code{sp_plot()}; both
#' are currently maintained for backwards compatibility.
#'
#' @param x An object to plot. When plotting sampling frames an \code{sf} object
#' given the appropriate class using \code{sp_frame}.
#'  When plotting design sites, an object created by \code{grts()} or
#' \code{irs()} (which has class \code{sp_design}). When plotting analysis data, a data frame
#' or an \code{sf} object given the appropriate class using \code{sp_frame}.
#'
#' @param sframe The sampling frame (an \code{sf} object) to plot alongside design
#' sites. This argument is only used when \code{object} corresponds to the design sites.
#'
#' @param formula A formula. One-sided formulas are used to summarize the
#' distribution of numeric or categorical variables. For one-sided formulas,
#' variable names are placed to the right of \code{~} (a right-hand side variable).
#' Two sided formulas are
#' used to summarize the distribution of a left-hand side variable
#' for each level of each right-hand side categorical variable in the formula.
#' Note that only for two-sided formulas are numeric right-hand side variables
#' coerced to a categorical variables. If an intercept
#' is included as a right-hand side variable (whether the formula is one-sided or
#' two-sided), the total will also be summarized. When plotting sampling frames
#' or analysis data, the default formula is \code{~ 1}. When plotting design sites,
#' \code{siteuse} should be used in the formula, and the default formula is
#' \code{~ siteuse}.
#'
#' @param siteuse A character vector of site types to include when plotting design sites.
#' It can only take on values \code{"sframe"} (sampling frame),
#' \code{"Legacy"} (for legacy sites), \code{"Base"} (for base sites),
#' \code{"Over"} (for \code{n_over} replacement sites), and \code{"Near"}
#' (for \code{n_near} replacement sites). The order of sites represents the
#' layering in the plot (e.g. \code{siteuse = c("Base", "Legacy")} will plot
#' legacy sites on top of base sites. Defaults to all non-\code{NULL} elements
#' in \code{x} and \code{y} with plot order \code{"sframe"}, \code{"Legacy"},
#' \code{"Base"}, \code{"Over"}, \code{"Near"}.
#'
#' @param var_args A named list. The name of each list element corresponds to a
#' right-hand side variable in \code{formula}. Values in the list are composed of
#' graphical arguments that are to be passed to \strong{every} level of the
#' variable. To see all graphical arguments available, run \code{?plot.sf}.
#'
#' @param varlevel_args A named list. The name of each list element corresponds to a
#' right-hand side variable in \code{formula}. The first element in this list
#' should be \code{"levels"} and contain all levels of the particular right-hand side variable. Subsequent
#' names correspond to graphical arguments that are to be passed to
#' the specified levels (in order) of the right-hand side variable. Values for each
#' graphical argument must be specified for each level of the right-hand side variable,
#' but applicable sf defaults will be matched by inputting the value \code{NA}.
#' To see all graphical arguments available, run \code{?plot.sf}
#'
#' @param geom Should separate geometries for each level of the right-hand
#' side \code{formula} variables be plotted? Defaults to \code{FALSE}.
#'
#' @param onlyshow A string indicating the single level of the single right-hand side
#' variable for which a summary is requested. This argument is only used when
#' a single right-hand side variable is provided.
#'
#' @param fix_bbox Should the geometry bounding box be fixed across plots?
#' If a length-four vector with names "xmin", "ymin", "xmax", and "ymax" and values
#' indicating bounding box edges, the bounding box will be fixed as \code{fix_bbox}
#' across plots. If \code{TRUE}, the bounding box will be fixed across plots as the
#' bounding box of \code{object}. If \code{FALSE}, the bounding box will vary across
#' plots according to the unique geometry for each plot. Defaults to \code{TRUE}.
#'
#' @param xcoord Name of the x-coordinate (east-west) in \code{object} (only required if
#' \code{object} is not an \code{sf} object).
#'
#' @param ycoord Name of y (north-south)-coordinate in \code{object} (only required if
#' \code{object} is not an \code{sf} object).
#'
#' @param crs Projection code for \code{xcoord} and \code{ycoord} (only
#' required if \code{object} is not an \code{sf} object).
#'
#' @param ... Additional arguments to pass to \code{plot.sf()}.
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @name plot
#' @method plot sp_frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("NE_Lakes")
#' NE_Lakes <- sp_frame(NE_Lakes)
#' plot(NE_Lakes, formula = ~ELEV_CAT)
#' sample <- grts(NE_Lakes, 30)
#' plot(sample, NE_Lakes)
#' }
plot.sp_frame <- function(x, formula = ~1, xcoord, ycoord, crs,
                          var_args = NULL, varlevel_args = NULL,
                          geom = FALSE, onlyshow = NULL, fix_bbox = TRUE, ...) {

  x <- sp_unframe(x)
  sp_plot(x, formula, xcoord, ycoord, crs,
          var_args, varlevel_args,
          geom, onlyshow, fix_bbox, ...)
  
}

#' @name plot
#' @method plot sp_design
#' @export
plot.sp_design <- function(x, sframe = NULL, formula = ~siteuse, siteuse = NULL,
                           var_args = NULL, varlevel_args = NULL, geom = FALSE, onlyshow = NULL,
                           fix_bbox = TRUE, ...) {
  
  sp_plot.sp_design(x, sframe, formula, siteuse,
                    var_args, varlevel_args, geom, onlyshow,
                    fix_bbox, ...)
}

################################################################################
# Function: plot.sp_CDF (exported)
# Programmer Tom Kincaid
# Date: May 5, 2021
# Revised: August 31, 2021 to improve documentation
#'
#' Plot a cumulative distribution function (CDF)
#'
#' This function creates a CDF plot.  Input data for the plots is provided by a
#' data frame from the "CDF" output given by  \code{cont_analysis}.
#' Confidence limits for the CDF also are plotted. Equivalent to \code{cdf_plot()}; 
#' both are currently maintained for backwards compatibility.
#' 
#' @name plot.sp_CDF
#' @method plot sp_CDF
#'
#' @param x Data frame from the "CDF" output given by
#'   \code{cont_analysis}.
#'
#' @param var If \code{cdfest} has multiple variables in the "Indicator" column,
#'   then \code{var} is the single variable to be plotted. The default is
#'   \code{NULL}, which assumes that only one variable is in the "Indicator"
#'   column of \code{cdfest}.
#'
#' @param subpop If \code{cdfest} has multiple variables in the "Type" column,
#'   then \code{subpop} is the single variable to be plotted. The default is
#'   \code{NULL}, which assumes that only one variable is in the "Type"
#'   column of \code{cdfest}.
#'
#' @param subpop_level If \code{cdfest} has multiple levels of \code{subpop}
#'   in the "Subpopulation" column,
#'   then \code{subpop_level} is the single level to be plotted. The default is
#'   \code{NULL}, which assumes that only one level is in the "Subpopulation"
#'   column of \code{cdfest}.
#'
#' @param units_cdf Indicator for the label utilized for the left side y-axis
#'   and the values used for the left side y-axis tick marks, where "Percent"
#'   means the label and values are in terms of percent of the population, and
#'   "Units" means the label and values are in terms of units (count, length,
#'   or area) of the population.  The default is "Percent".
#'
#' @param type_cdf Character string consisting of the value "Continuous" or
#'   "Ordinal" that controls the type of CDF plot.  The default is "Continuous".
#'
#' @param log Character string consisting of the value "" or "x" that
#'   controls whether the x axis uses the original scale ("") or the base 10
#'   logarithmic scale ("x").  The default is "".
#'
#' @param xlab Character string providing the x-axis label.  If this argument
#'   equals NULL, then the indicator name is used as the label.  The default is
#'   NULL.
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
#' @param main Character string providing the plot title.  The default is
#'   NULL.
#'
#' @param legloc Indicator for location of the plot legend, where "BR" means
#'   bottom right, "BL" means bottom left, "TR" means top right, "TL" means
#'   top left, and NULL means no legend.  The default is NULL.
#'
#' @param confcut Numeric value that controls plotting confidence limits at
#'   the CDF extremes.  Confidence limits for CDF values (percent scale) less
#'   than confcut or greater than 100 minus confcut are not plotted.  A value of
#'   zero means confidence limits are plotted for the complete range of the CDF.
#'   The default is 0.
#'
#' @param conflev Numeric value of the confidence level used for confidence
#'   limits. The default is 95.
#'
#' @param cex.main Expansion factor for the plot title.  The default is 1.2.
#'
#' @param cex.legend Expansion factor for the legend title. The default is 1.
#'
#' @param  ... Additional arguments passed to the \code{plot.default} function
#'   (aside from those already used and \code{ylim}).
#'
#' @return A plot of a variable's CDF estimates associated confidence limits.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{cont_cdfplot}}}{for creating a PDF file containing CDF
#'     plots}
#'   \item{\code{\link{cont_cdftest}}}{for CDF hypothesis testing}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
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
#' plot(myanalysis$CDF[keep, ],
#'   xlab = "ContVar",
#'   ylab = "Percent of Stream Length", ylab_r = "Stream Length (km)",
#'   main = "Estimates for Resource Class: Good"
#' )
#' plot(myanalysis$CDF[keep, ],
#'   xlab = "ContVar",
#'   ylab = "Percent of Stream Length", ylab_r = "Same",
#'   main = "Estimates for Resource Class: Good"
#' )
#' }
#' @export
################################################################################
plot.sp_CDF <- function(x, var = NULL, subpop = NULL, subpop_level = NULL,
                        units_cdf = "Percent", type_cdf = "Continuous", log = "",
                        xlab = NULL, ylab = NULL, ylab_r = NULL, main = NULL, legloc = NULL,
                        confcut = 0, conflev = 95, cex.main = 1.2, cex.legend = 1, ...) {
  cdf_plot(x, var, subpop, subpop_level, units_cdf, type_cdf, log,
           xlab, ylab, ylab_r, main, legloc, confcut, conflev, cex.main, 
           cex.legend, ...)
}



