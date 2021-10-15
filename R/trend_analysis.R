################################################################################
# Function: trend_analysis (exported)
# Programmer: Tom Kincaid
# Date: March 3, 2021
# Revised March 5, 2021 to add argument jointprob that is required for
#         additional variance estimators
# Revised: March 10, 2021 to assign zero to missing year estimates when fitting
#          the SLR and WLR models for categorical variables
# Revised: April 7, 2021 to ensure that the dframe argument does not contain
#          zero rows
# Revised: April 29, 2021 to ensure that the dframe argument only belongs to
#          class "data.frame"
# Revised: May 4, 2021 to avoid warning messages being generated during creation
#          of help files
# Revised: May 6, 2021 to ensure that sf objects do not belong to class tbl_df
# Revised: June 8, 2021 to simplify specification of the values required for
#          calculation of the finite population correction factor and to
#          eliminate use of the finite population correction factor with the
#          local mean variance estimator
# Revised: June 14, 2021 to use the new function named mean_est for calculation
#          of mean estimates
# Revised: July 28, 2021 to ensure that data is available for a sufficient
#          number of years to perform linear regression for categorical
#          variables
# Revised: August 9, 2021 to correct an error when appending values to the
#          contsum data frame
# Revised: September 9, 2021 to revise the documentation for argument popsize
# Revised: September 17, 2021 to allow the user to specify the formula for a
#          linear mixed-effects model and to allow use of generalized linear
#          mixed-effects models for categorical data
#
#' Trend analysis
#'
#' This function organizes input and output for estimation of trend across time
#' for a series of samples (for categorical and continuous variables). Trend is estimated using the
#' analytical procedure identified by the model arguments.  For categorical
#' variables, the choices for the \code{model_cat} argument are: (1) simple linear
#' regression, (2) weighted linear regression, and (3) generalized linear
#' mixed-effects model. For continuous variables, the choices for the
#' \code{model_cont} argument are: (1) simple linear regression, (2) weighted
#' linear regression, and (3)  linear mixed-effects model.  The analysis data,
#' \code{dframe}, can be either a data frame or a simple features (\code{sf}) object.  If an
#' \code{sf} object is used, coordinates are extracted from the geometry column in the
#' object, arguments \code{xcoord} and \code{ycoord} are assigned values
#' \code{"xcoord"} and \code{"ycoord"}, respectively, and the geometry column is
#' dropped from the object.
#'
#' @param dframe Data to be analyzed (analysis data). A data frame or
#'   \code{sf} object containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param vars_cat Vector composed of character values that identify the names
#'   of categorical response variables in \code{dframe}. If
#'   argument \code{model_cat} equals "GLMM", the categorical variables in the
#'   \code{dframe} data frame must be factors each of which has two levels,
#'   where the second level will be assumed to specify "success".  The default
#'   value is \code{NULL}.
#'
#' @param vars_cont Vector composed of character values that identify the
#'   names of continuous response variables in \code{dframe}.
#'   The default value is \code{NULL}.
#'
#' @param subpops Vector composed of character values that identify the
#'   names of subpopulation (domain) variables in \code{dframe}.
#'   If a value is not provided, the value \code{"All_Sites"} is assigned to the
#'   subpops argument and a factor variable named \code{"All_Sites"} that takes
#'   the value \code{"All Sites"} is added to \code{dframe}.  The
#'   default value is \code{NULL}.
#'
#' @param model_cat Character value identifying the analytical procedure used
#'   for trend estimation for categorical variables.  The choices are:
#'   \code{"SLR"} (simple linear regression), \code{"WLR"} (weighted linear
#'   regression), and \code{"GLMM"} (generalized linear mixed-effects model).
#'   The default value is \code{"SLR"}.
#'
#' @param cat_rhs Character value specifying the right hand side of the formula
#'   for a generalized linear mixed-effects model.  If a value is not provided,
#'   the argument is assigned a value that specifies the Piepho and Ogutu (2002)
#'   model.  The default value is \code{NULL}.
#'
#' @param model_cont Character value identifying the analytical procedure used
#'   for trend estimation for continuous variables.  The choices are:
#'   \code{"SLR"} (simple linear regression), \code{"WLR"} (weighted linear
#'   regression), and \code{"LMM"} (linear mixed-effects model).  The default
#'   value is \code{"LMM"}.
#'
#' @param cont_rhs Character value specifying the right hand side of the
#'   formula for a linear mixed-effects model.  If a value is not provided, the
#'   argument is assigned a value that specifies the Piepho and Ogutu (2002)
#'   model.  The default value is \code{NULL}.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   \code{dframe}.  If repeat visit sites are present, the site
#'   ID value for each revisit site will be the same for each survey.  For a
#'   two-stage sample, the site ID variable identifies stage two site IDs.  The
#'   default value is \code{"siteID"}.
#'
#' @param yearID Character value providing name of the time period variable in
#'   \code{dframe}, which must be numeric and will be forced to
#'   numeric if it is not.  The default assumption is that the time period
#'   variable is years.  The default value is \code{"year"}.
#'
#' @param weight Character value providing name of the design weight
#'   variable in \code{dframe}.  For a two-stage sample, the
#'   weight variable identifies stage two weights.  The default value is
#'   \code{"weight"}.
#'
#' @param xcoord Character value providing name of the x-coordinate variable in
#'   \code{dframe}.  For a two-stage sample, the x-coordinate
#'   variable identifies stage two x-coordinates.  Note that x-coordinates are
#'   required  for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the x-coordinate). The
#'   default value is \code{NULL}.
#'
#' @param ycoord Character value providing name of the y-coordinate variable in
#'   \code{dframe}.  For a two-stage sample, the y-coordinate
#'   variable identifies stage two y-coordinates.  Note that y-coordinates are
#'   required for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the y-coordinate). The default
#'   value is \code{NULL}.
#'
#' @param stratumID Character value providing name of the stratum ID variable in
#'   \code{dframe}.  The default value is \code{NULL}.
#'
#' @param clusterID Character value providing name of the cluster (stage one) ID
#'   variable in \code{dframe}.  Note that cluster IDs are
#'   required for a two-stage sample.  The default value is \code{NULL}.
#'
#' @param weight1 Character value providing name of the stage one weight
#'   variable in \code{dframe}.  The default value is \code{NULL}.
#'
#' @param xcoord1 Character value providing name of the stage one x-coordinate
#'   variable in \code{dframe}.  Note that x-coordinates are
#'   required for calculation of the local mean variance estimator.  The default
#'   value is \code{NULL}.
#'
#' @param ycoord1 Character value providing name of the stage one y-coordinate
#'   variable in \code{dframe}.  Note that y-coordinates are
#'   required for calculation of the local mean variance estimator.  The default
#'   value is \code{NULL}.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation, where \code{TRUE} = use size weights and
#'   \code{FALSE} = do not use size weights. To employ size weights for a
#'   single-stage sample, a value must be supplied for argument weight.  To
#'   employ size weights for a two-stage sample, values must be supplied for
#'   arguments \code{weight} and \code{weight1}. The default value is
#'   \code{FALSE}.
#'
#' @param sweight Character value providing name of the size weight variable in
#'   \code{dframe}.  For a two-stage sample, the size weight
#'   variable identifies stage two size weights.  The default value is
#'   \code{NULL}.
#'
#' @param sweight1 Character value providing name of the stage one size weight
#'   variable in \code{dframe}.  The default value is
#'   \code{NULL}.
#'
#' @param fpc Object that specifies values required for calculation of the
#'   finite population correction factor used during variance estimation. The
#'   object must match the survey design in terms of stratification and whether
#'   the design is single-stage or two-stage.  For an unstratified design, the
#'   object is a vector.  The vector is composed of a single numeric value for a
#'   single-stage design.  For a two-stage unstratified design, the object is a
#'   named vector containing one more than the number of clusters in the sample,
#'   where the first item in the vector specifies the number of clusters in the
#'   population and each subsequent item specifies the number of stage two units
#'   for the cluster.  The name for the first item in the vector is arbitrry.
#'   Subsequent names in the vector identify clusters and must match the cluster
#'   IDs.  For a stratified design, the object is a named list of vectors, where
#'   names must match the strata IDs.  For each stratum, the format of the
#'   vector is identical to the format described for unstratified single-stage
#'   and two-stage designs.  Note that the finite population correction factor
#'   is not used with the local mean variance estimator.
#'
#'   Example fpc for a single-stage unstratified survey design:
#'
#'   \verb{fpc <- 15000}
#'
#'   Example fpc for a single-stage stratified survey design:
#'
#'   \verb{fpc <- list(
#'     Stratum_1 = 9000,
#'     Stratum_2 = 6000)
#'    }
#'
#'   Example fpc for a two-stage unstratified survey design:
#'
#'   \verb{fpc <- c(
#'     Ncluster = 150,
#'     Cluster_1 = 150,
#'     Cluster_2 = 75,
#'     Cluster_3 = 75,
#'     Cluster_4 = 125,
#'     Cluster_5 = 75)
#'   }
#'
#'   Example fpc for a two-stage stratified survey design:
#'
#'   \verb{fpc <- list(
#'     Stratum_1 = c(
#'       Ncluster_1 = 100,
#'       Cluster_1 = 125,
#'       Cluster_2 = 100,
#'       Cluster_3 = 100,
#'       Cluster_4 = 125,
#'       Cluster_5 = 50),
#'     Stratum_2 = c(
#'       Ncluster_2 = 50,
#'       Cluster_1 = 75,
#'       Cluster_2 = 150,
#'       Cluster_3 = 75,
#'       Cluster_4 = 75,
#'       Cluster_5 = 125))
#'   }
#'
#' @param popsize Object that provides values for the population argument of the
#'   \code{calibrate} or \code{postStratify} functions in the survey package. If
#'   a value is provided for popsize, then either the \code{calibrate} or
#'   \code{postStratify} function is used to modify the survey design object
#'   that is required by functions in the survey package.  Whether to use the
#'   \code{calibrate} or \code{postStratify} function is dictated by the format
#'   of popsize, which is discussed below.  Post-stratification adjusts the
#'   sampling and replicate weights so that the joint distribution of a set of
#'   post-stratifying variables matches the known population joint distribution.
#'   Calibration, generalized raking, or GREG estimators generalize
#'   post-stratification and raking by calibrating a sample to the marginal
#'   totals of variables in a linear regression model. For the \code{calibrate}
#'   function, the object is a named list, where the names identify factor
#'   variables in \code{dframe}.  Each element of the list is a
#'   named vector containing the population total for each level of the
#'   associated factor variable.  For the \code{postStratify} function, the
#'   object is either a data frame, table, or xtabs object that provides the
#'   population total for all combinations of selected factor variables in the
#'   \code{dframe} data frame.  If a data frame is used for \code{popsize}, the
#'   variable containing population totals must be the last variable in the data
#'   frame.  If a table is used for \code{popsize}, the table must have named
#'   \code{dimnames} where the names identify factor variables in the
#'   \code{dframe} data frame.  If the popsize argument is equal to \code{NULL},
#'   then neither calibration nor post-stratification is performed.  The default
#'   value is \code{NULL}.
#'
#'   Example popsize for calibration:
#'
#'   \verb{popsize <- list(
#'     Ecoregion = c(
#'       East = 750,
#'       Central = 500,
#'       West = 250),
#'     Type = c(
#'       Streams = 1150,
#'       Rivers = 350))
#'   }
#'
#'   Example popsize for post-stratification using a data frame:
#'
#'   \verb{popsize <- data.frame(
#'     Ecoregion = rep(c("East", "Central", "West"),
#'       rep(2, 3)),
#'     Type = rep(c("Streams", "Rivers"), 3),
#'     Total = c(575, 175, 400, 100, 175, 75))
#'   }
#'
#'   Example popsize for post-stratification using a table:
#'
#'   \verb{popsize <- with(MySurveyFrame,
#'     table(Ecoregion, Type))}
#'
#'   Example popsize for post-stratification using an xtabs object:
#'
#'   \verb{popsize <- xtabs(~Ecoregion + Type,
#'     data = MySurveyFrame)}
#'
#' @param invprboot Logical value that indicates whether the inverse probability
#'   bootstrap procedure is used to calculate trend parameter estimates.  This
#'   bootstrap procedure is only available for the "LMM" option for continuous
#'   variables.  Inverse probability references the design weights, which
#'   are the inverse of the sample inclusion probabilities.  The default value
#'   is \code{TRUE}.
#'
#' @param nboot Numeric value for the number of bootstrap iterations.  The
#'   default is \code{1000}.
#'
#' @param vartype Character value providing choice of the variance estimator,
#'   where \code{"Local"} = the local mean estimator, \code{"SRS"} = the simple
#'   random sampling estimator, \code{"HT"} = the Horvitz-Thompson estimator,
#'   and \code{"YG"} = the Yates-Grundy estimator.  The default value is
#'   \code{"Local"}.
#'
#' @param jointprob Character value providing choice of joint inclusion
#'   probability approximation for use with Horvitz-Thompson and Yates-Grundy
#'   variance estimators, where \code{"overton"} indicates the Overton
#'   approximation, \code{"hr"} indicates the Hartley_Rao approximation, and
#'   \code{"brewer"} equals the Brewer approximation.  The default value is
#'   \code{"overton"}.
#'
#' @param conf Numeric value for the Gaussian-based confidence level.  The default is
#'   \code{95}.
#'
#' @param All_Sites A logical variable used when \code{subpops} is not
#'   \code{NULL}. If \code{All_Sites} is \code{TRUE}, then alongside the
#'   subpopulation output, output for all sites (ignoring subpopulations) is
#'   returned for each variable in \code{vars}. If \code{All_Sites} is
#'   \code{FALSE}, then alongside the subpopulation output, output for all sites
#'   (ignoring subpopulations) is not returned for each variable in \code{vars}.
#'   The default is \code{FALSE}.
#'
#' @section Details:
#' For the simple linear regression (SLR) model, a design-based estimate of the
#' category proportion (categorical variables) or the mean (continuous
#' variables) is calculated for each time period (year).  Four choices of
#' variance estimator are available for calculating variance of the design-based
#' estimates: (1) the local mean estimator, (2) the simple random sampling
#' estimator, (3) the Horvitz-Thompson estimator, and (4) the Yates-Grundy
#' estimator.  For the Horvitz-Thompson and Yates-Grundy estimators, there are
#' three choices for calculating joint inclusion probabilities: (1) the Overton
#' approximation, (2) the Hartley-Rao approximation, and (3) the Brewer
#' approximation.  The \code{lm} function in the stats package is used to fit a
#' linear model using a \code{formula} argument that specifies the proportion or
#' mean estimates as the response variable and years as the regressor variable.
#' For fitting the SLR model, the \code{yearID} variable from the \code{dframe}
#' argument is modified by subtracting the minimum value of years from all
#' values of the variable.  Parameter estimates are extracted from the object
#' returned by the \code{lm} function.  For the weighted linear regression (WLR)
#' model, the process is the same as the SLR model except that the inverse of
#' the variances of the proportion or mean estimates is used as the
#' \code{weights} argument in the call to the \code{lm} function.  For the LMM
#' option, the \code{lmer} function in the lme4 package is used to fit a linear
#' mixed-effects model for trend across years.  For both the GLMM and LMM
#' options, the default Piepho and Ogutu (PO) model includes fixed effects for
#' intercept and trend (slope) and random effects for intercept and trend for
#' individual sites, where the \code{siteID} variable from the \code{dframe}
#' argument identifies sites.  Correlation between the random effects for site
#' intercepts and site trends is included in the model. Finally, the PO model
#' contains random effects for year variance and residual variance. For the GLMM
#' and LMM options, arguments \code{cat_rhs} and \code{cont_rhs}, respectively,
#' can be used to specify the right hand side of the model formula. Internally,
#' a variable named \code{Wyear} is created that is useful for specifying the
#' \code{cat_rhs} and \code{cont_rhs} arguments.  The \code{Wyear} variable is
#' created by subtracting the minimum value of the \code{yearID} variable from
#' all values of the variable.  If argument \code{invprboot} is \code{FALSE},
#' parameter estimates are extracted from the object returned by the \code{lmer}
#' function. If argument \code{invprboot} is \code{TRUE}, the \code{boot}
#' function in the boot package is used to generate bootstrap replicates using a
#' function named \code{bootfcn} as the \code{statistic} argument passed to the
#' \code{boot} function.  For each bootstrap replicate, \code{bootfcn} calls the
#' \code{glmer} or \code{lmer} function, as appropriate, using the specified
#' model.  design weights identified by the \code{weight} argument for
#' the \code{trend_analysis} function are passed as the \code{weights} argument
#' for the \code{boot} function, which specifies importance weights.  Using the
#' design weights as the \code{weights} argument ensures that bootstrap
#' replicates are representative of the survey population.  Parameter estimates
#' are calculated using the object returned by the \code{boot} function.
#'
#' @return The analysis results. A list composed of two data frames containing trend estimates for all
#'   combinations of population Types, subpopulations within Types, and response
#'   variables.  For categorical variables, trend estimates are calculated for
#'   each category of the variable.  The two data frames in the output list are:
#'   \describe{
#'     \item{\code{catsum}}{data frame containing trend estimates for
#'       categorical variables}
#'     \item{\code{contsum}}{data frame containing trend
#'       estimates for continuous variables}
#'    }
#'
#'   For the SLR and WLR model options, the data frame contains the following
#'   variables:
#'   \describe{
#'     \item{Type}{subpopulation (domain) name}
#'     \item{Subpopulation}{subpopulation name within a domain}
#'     \item{Indicator}{response variable}
#'     \item{Trend_Estimate}{trend estimate}
#'     \item{Trend_Std_Error}{trend standard error}
#'     \item{Trend_LCBxxPct}{trend xx\% (default 95\%) lower confidence bound}
#'     \item{Trend_UCBxxPct}{trend xx\% (default 95\%) upper confidence bound}
#'     \item{Trend_p_Value}{trend p-value}
#'     \item{Intercept_Estimate}{intercept estimate}
#'     \item{Intercept_Std_Error}{intercept standard error}
#'     \item{Intercept_LCBxxPct}{intercept xx\% (default 95\%) lower confidence
#'       bound}
#'     \item{Intercept_UCBxxPct}{intercept xx\% (default 95\%) upper confidence
#'       bound}
#'     \item{Intercept_p_Value}{intercept p-value}
#'     \item{R_Squared}{R-squared value}
#'     \item{Adj_R_Squared}{adjusted R-squared value}
#'   }
#'
#'   For the GLMM and LMM model options, contents of the data frames will vary
#'   depending on the model specified by arguments \code{cat_rhs} and
#'   \code{cont_rhs}.  For the default PO model, the data frame contains the
#'   following variables:
#'   \describe{
#'     \item{Type}{subpopulation (domain) name}
#'     \item{Subpopulation}{subpopulation name within a domain}
#'     \item{Indicator}{response variable}
#'     \item{Trend_Estimate}{trend estimate}
#'     \item{Trend_Std_Error}{trend standard error}
#'     \item{Trend_LCBxxPct}{trend xx\% (default 95\%) lower confidence bound}
#'     \item{Trend_UCBxxPct}{trend xx\% (default 95\%) upper confidence bound}
#'     \item{Trend_p_Value}{trend p-value}
#'     \item{Intercept_Estimate}{intercept estimate}
#'     \item{Intercept_Std_Error}{intercept standard error}
#'     \item{Intercept_LCBxxPct}{intercept xx\% (default 95\%) lower confidence
#'       bound}
#'     \item{Intercept_UCBxxPct}{intercept xx\% (default 95\%) upper confidence
#'       bound}
#'     \item{Intercept_p_Value}{intercept p-value}
#'     \item{Var_SiteInt}{variance of the site intercepts}
#'     \item{Var_SiteTrend}{variance of the site trends}
#'     \item{Corr_SiteIntSlope}{correlation of site intercepts and site trends}
#'     \item{Var_Year}{year variance}
#'     \item{Var_Residual}{residual variance}
#'     \item{AIC}{generalized Akaike Information Criterion}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{change_analysis}}}{for change analysis}
#'   }
#'
#' @examples
#' # Example using a categorical variable with three resource classes and a
#' # continuous variable
#' mydframe <- data.frame(
#'   siteID = rep(paste0("Site", 1:40), rep(5, 40)),
#'   yearID = rep(seq(2000, 2020, by = 5), 40),
#'   wgt = rep(runif(40, 10, 100), rep(5, 40)),
#'   xcoord = rep(runif(40), rep(5, 40)),
#'   ycoord = rep(runif(40), rep(5, 40)),
#'   All_Sites = rep("All Sites", 200),
#'   Region = sample(c("North", "South"), 200, replace = TRUE),
#'   Resource_Class = sample(c("Good", "Fair", "Poor"), 200, replace = TRUE),
#'   ContVar = rnorm(200, 10, 1)
#' )
#' myvars_cat <- c("Resource_Class")
#' myvars_cont <- c("ContVar")
#' mysubpops <- c("All_Sites", "Region")
#' trend_analysis(
#'   dframe = mydframe,
#'   vars_cat = myvars_cat,
#'   vars_cont = myvars_cont,
#'   subpops = mysubpops,
#'   model_cat = "WLR",
#'   model_cont = "SLR",
#'   siteID = "siteID",
#'   yearID = "yearID",
#'   weight = "wgt",
#'   xcoord = "xcoord",
#'   ycoord = "ycoord"
#' )
#' @export
################################################################################

trend_analysis <- function(dframe, vars_cat = NULL, vars_cont = NULL, subpops = NULL, model_cat = "SLR",
                           cat_rhs = NULL, model_cont = "LMM", cont_rhs = NULL, siteID = "siteID",
                           yearID = "year", weight = "weight", xcoord = NULL, ycoord = NULL,
                           stratumID = NULL, clusterID = NULL, weight1 = NULL, xcoord1 = NULL,
                           ycoord1 = NULL, sizeweight = FALSE, sweight = NULL, sweight1 = NULL,
                           fpc = NULL, popsize = NULL, invprboot = TRUE, nboot = 1000, vartype = "Local",
                           jointprob = "overton", conf = 95, All_Sites = FALSE) {

  # Create a vector for error messages

  error_ind <- FALSE
  error_vec <- NULL

  # Create a data frame for warning messages

  warn_ind <- FALSE
  warn_df <- NULL
  fname <- "trend_analysis"

  # Ensure that the dframe argument was provided

  if (missing(dframe)) {
    stop("\nThe dframe argument must be provided.\n")
  }

  # If the dframe argument is an sf object, extract coordinates from the
  # geometry column, assign values "xcoord" and "ycoord" to arguments xcoord and
  # ycoord, respectively, and drop the geometry column from the object

  if ("sf" %in% class(dframe)) {
    temp <- st_coordinates(dframe)
    xcoord <- "xcoord"
    dframe$xcoord <- temp[, "X"]
    ycoord <- "ycoord"
    dframe$ycoord <- temp[, "Y"]
    dframe <- st_set_geometry(dframe, NULL)
  }

  # If the dframe argument is a tibble or does not belong to class
  # "data.frame", coerce the argument to class "data.frame"

  if ("tbl_df" %in% class(dframe) | !("data.frame" %in% class(dframe))) {
    dframe <- as.data.frame(dframe)
  }

  # Ensure that the dframe argument does not contain zero rows

  if (nrow(dframe) == 0) {
    stop("\nThe dframe argument contains zero rows.\n")
  }

  # Ensure that unused levels are dropped from factor variables in the dframe
  # data frame

  dframe <- droplevels(dframe)

  # Ensure that the dframe data frame contains the site ID variable

  if (!(siteID %in% names(dframe))) {
    ind <- FALSE
    error_ind <- TRUE
    msg <- paste0("The name provided for the siteID argument, \"", siteID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {
    ind <- TRUE
  }

  # Create a variable containing the original values of the site ID variable in
  # the dframe data frame and a variable containing a unique set of values
  # created from the values of the site ID variable in the dframe data frame

  if (ind) {
    dframe$siteID_orginal <- dframe[, siteID]
    dframe$siteID_unique <- dframe[, siteID]
    temp <- sapply(split(dframe$siteID_unique, dframe$siteID_unique), length)
    if (any(temp > 1)) {
      dframe$siteID_unique <- uniqueID(dframe$siteID_unique)
    }
  }

  # Ensure that the dframe data frame contains the time period identifier
  # variable and ensure that the variable is numeric

  if (!(yearID %in% names(dframe))) {
    error_ind <- TRUE
    msg <- paste0("The name provided for the yearID argument, \"", yearID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {
    if (!is.numeric(dframe[, yearID])) {
      warn_ind <- TRUE
      warn <- paste0("The variable in the dframe data frame identified by argument yearID, \"", yearID, "\", was coerced \nto class numeric.\n")
      act <- "Variable coerced to class numeric\n"
      warn_df <- rbind(warn_df, data.frame(
        func = I(fname), subpoptype = NA,
        subpop = NA, indicator = NA, stratum = NA, warning = I(warn), action = I(act)
      ))
      dframe[, yearID] <- as.numeric(dframe[, yearID])
    }
  }

  # Ensure that the dframe data frame contains the survey weight variable

  if (!(weight %in% names(dframe))) {
    error_ind <- TRUE
    msg <- paste0("The name provided for the weight argument, \"", weight, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  }

  # Assign names to the variables required for calculation of the finite
  # population correction factor

  if (is.null(fpc)) {
    fpcfactor_ind <- FALSE
    fpcsize <- NULL
    Ncluster <- NULL
    stage1size <- NULL
  } else {
    fpcfactor_ind <- TRUE
    if (is.null(clusterID)) {
      fpcsize <- "fpcsize"
      Ncluster <- NULL
      stage1size <- NULL
    } else {
      fpcsize <- NULL
      Ncluster <- "Ncluster"
      stage1size <- "stage1size"
    }
  }

  # Create a list containing names of survey design variables

  design_names <- list(
    siteID = siteID,
    weight = weight,
    xcoord = xcoord,
    ycoord = ycoord,
    stratumID = stratumID,
    clusterID = clusterID,
    weight1 = weight1,
    xcoord1 = xcoord1,
    ycoord1 = ycoord1,
    sweight = sweight,
    sweight1 = sweight1,
    fpcsize = fpcsize,
    Ncluster = Ncluster,
    stage1size = stage1size
  )

  # Ensure that a value was provided for at least one of the vars_cat
  # (categoroical response variable names) or vars_cont (continuous response
  # variable names) arguments

  if (missing(vars_cat) & missing(vars_cont)) {
    error_ind <- TRUE
    msg <- "A value must be provided for at least one of the vars_cat (categoroical response \nvariable names) or vars_cont (continuous response variable names) arguments.\n"
    error_vec <- c(error_vec, msg)
  }

  # If a value was not provided for the subpops (subpopulation names) argument,
  # assign the value "All_Sites" to the subpops argument and create a factor
  # named "All_Sites" in the dframe data frame that takes the value "All Sites"

  if (is.null(subpops)) {
    subpops <- "All_Sites"
    dframe$All_Sites <- "All Sites"
    dframe$All_Sites <- factor(dframe$All_Sites)
  }

  # If the user wants information for all sites together in addition to the
  # subpops, add the value "All_Sites" to the subpops argument and create a
  # factor named "All_Sites" in the dframe data frame that takes the value
  # "All Sites"

  if (!is.null(subpops) && All_Sites) {
    subpops <- c(subpops, "All_Sites")
    dframe$All_Sites <- "All Sites"
    dframe$All_Sites <- factor(dframe$All_Sites)
  }

  # Ensure that arguments model_cat and model_cont contain valid values

  if (!(model_cat %in% c("SLR", "WLR", "GLMM"))) {
    error_ind <- TRUE
    msg <- paste0("The value provided for argument model_cat, \"", model_cat, "\", is not a valid value.\n")
    error_vec <- c(error_vec, msg)
  }

  if (!(model_cont %in% c("SLR", "WLR", "LMM"))) {
    error_ind <- TRUE
    msg <- paste0("The value provided for argument model_cont, \"", model_cont, "\", is not a valid value.\n")
    error_vec <- c(error_vec, msg)
  }

  # As necessary, assign a value to the cat_rhs and cont_rhs arguments

  PO_ind_cat <- FALSE
  if (model_cat == "GLMM" & is.null(cat_rhs)) {
    PO_ind_cat <- TRUE
    cat_rhs <- paste0("Wyear + (1 + Wyear | ", siteID, ") + (1 | ", yearID, ")")
  }
  PO_ind_cont <- FALSE
  if (model_cont == "LMM" & is.null(cont_rhs)) {
    PO_ind_cont <- TRUE
    cont_rhs <- paste0(
      "Wyear + (1 + Wyear | ", siteID, ") + (1 | ", yearID,
      ")"
    )
  }

  # As necessary, to ensure that the input_check function does not produce an
  # invalid error message, assign "SRS" to argument vartype

  if (is.null(vars_cat) & model_cont == "LMM" |
    !is.null(vars_cat) & model_cat == "GLMM" & model_cont == "LMM") {
    vartype <- "SRS"
  }

  # Check input arguments

  temp <- input_check(dframe, design_names, vars_cat, vars_cont, NULL, NULL,
    subpops, sizeweight, fpc, popsize, vartype, jointprob, conf,
    error_ind = error_ind, error_vec = error_vec
  )
  dframe <- temp$dframe
  vars <- temp$vars_cat
  subpops <- temp$subpops
  popsize <- temp$popsize
  vartype <- temp$vartype
  jointprob <- temp$jointprob
  error_ind <- temp$error_ind
  error_vec <- temp$error_vec

  # As necessary, ensure that variables identified by the vars_cat argument
  # contain two levels

  if (!is.null(vars_cat) & model_cat == "GLMM") {
    tst <- logical(length(vars_cat))
    for (i in 1:length(vars_cat)) {
      tst[i] <- length(levels(dframe[, vars_cat[i]])) != 2
    }
    if (any(tst)) {
      error_ind <- TRUE
      temp_str <- vecprint(vars_cat[tst])
      msg <- paste0("For the \"GLMM\" option, categorical variables must be composed of only two levels.  The \nfollowing variables do not have only two levels:\n", temp_str)
      error_vec <- c(error_vec, msg)
    }
  }

  # As necessary, output a message indicating that error messages were generated
  # during execution of the program

  if (error_ind) {
    error_vec <<- error_vec
    if (length(error_vec) == 1) {
      cat("During execution of the program, an error message was generated.  The error \nmessage is stored in a vector named 'error_vec'.  Enter the following command \nto view the error message: errorprnt()\n")
    } else {
      cat(paste("During execution of the program,", length(error_vec), "error messages were generated.  The error \nmessages are stored in a vector named 'error_vec'.  Enter the following \ncommand to view the error messages: errorprnt()\n"))
    }

    if (warn_ind) {
      warn_df <<- warn_df
      if (nrow(warn_df) == 1) {
        cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn_df'.  Enter the following command \nto view the warning message: warnprnt()\n")
      } else {
        cat(paste("During execution of the program,", nrow(warn_df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn_df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
      }
    }
    stop("See the preceding message(s).")
  }

  # Assign a logical value to the indicator variable for a stratified sample

  stratum_ind <- !is.null(stratumID)

  # For a stratified sample, remove strata that contain a single site

  if (stratum_ind) {
    dframe[, stratumID] <- factor(dframe[, stratumID])
    stratum_levels <- levels(dframe[, stratumID])
    nstrata <- length(stratum_levels)
    ind <- FALSE
    for (i in 1:nstrata) {
      tst <- dframe[, stratumID] == stratum_levels[i]
      if (sum(tst) == 1) {
        warn_ind <- TRUE
        warn <- paste0("The stratum named \"", stratum_levels[i], "\" contains a single value and was removed from the analysis.\n")
        act <- "Stratum was removed from the analysis.\n"
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = NA,
          subpop = NA, indicator = NA, stratum = NA, warning = I(warn), action = I(act)
        ))
        dframe <- dframe[!tst, ]
        ind <- TRUE
      }
    }
    if (ind) {
      dframe[, stratumID] <- factor(dframe[, stratumID])
      stratum_levels <- levels(dframe[, stratumID])
      nstrata <- length(stratum_levels)
    }
  }

  # Assign a logical value to the indicator variable for a two-stage sample

  cluster_ind <- !is.null(clusterID)

  # Create the survey design object

  design <- survey_design(
    dframe, siteID, weight, stratum_ind, stratumID, cluster_ind, clusterID,
    weight1, sizeweight, sweight, sweight1, fpcfactor_ind, fpcsize, Ncluster,
    stage1size, vartype, jointprob
  )

  # If popsize is not equal to NULL, then call either the postStratify or
  # calibrate function, as appropriate

  if (!is.null(popsize)) {
    if (all(class(popsize) %in% c("data.frame", "table", "xtabs"))) {
      if ("data.frame" %in% class(popsize)) {
        pnames <- names(popsize)[-ncol(popsize)]
      } else {
        pnames <- names(dimnames(popsize))
      }
      design <- postStratify(design, make.formula(pnames), popsize)
    } else {
      cnames <- cal_names(make.formula(names(popsize)), design)
      pop.totals <- numeric(length(cnames))
      names(pop.totals) <- cnames
      pop.totals[1] <- sum(popsize[[1]])
      k <- 2
      for (i in names(popsize)) {
        temp <- popsize[[i]]
        for (j in 2:length(temp)) {
          pop.totals[k] <- temp[j]
          k <- k + 1
        }
      }
      design <- calibrate(design, make.formula(cnames), pop.totals)
    }
  }

  # If popsize is not equal to NULL and vartype equals "Local", then assign
  # adjusted weights to the appropriate weight variable(s) in the
  # design$variables data frame

  if (!is.null(popsize) && vartype == "Local") {
    if (cluster_ind) {
      design$variables$wgt2 <- weights(design) / design$variables$wgt1
    } else {
      design$variables$wgt <- weights(design)
    }
  }

  # If invprboot equals TRUE, then assign the bootstrap weights

  if (invprboot == TRUE) {
    if (cluster_ind) {
      bootwgt <- dframe$wgt1 * dframe$wgt2
    } else {
      bootwgt <- dframe$wgt
    }
  }

  # Create a year variable for use in modelling

  Wyear <- "Wyear"
  dframe$Wyear <- dframe[, yearID]
  dframe$Wyear <- dframe$Wyear - min(dframe$Wyear)

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Create the output object

  trendsum <- list(catsum = NULL, contsum = NULL)

  #
  # Begin the section for categorical response variables
  #

  if (!is.null(vars_cat)) {

    # Assign values to the site ID variable in the dframe data frame

    if (model_cat %in% c("SLR", "WLR")) {
      dframe[, siteID] <- dframe$siteID_unique
    } else {
      dframe[, siteID] <- dframe$siteID_orginal
    }

    # Loop through all subpopulations (domains)

    for (itype in subpops) {
      lev_itype <- levels(dframe[, itype])
      nlev_itype <- length(lev_itype)

      # Loop through all  response variables

      for (ivar in vars_cat) {
        lev_ivar <- levels(dframe[, ivar])
        nlev_ivar <- length(lev_ivar)

        # Loop through all levels of a subpopulation

        for (isubpop in lev_itype) {

          # Section for the simple linear regression and weighted linear
          # regression models

          if (model_cat %in% c("SLR", "WLR")) {

            # Determine the set of years for this subpopulation

            subpop_ind <- dframe[, itype] %in% isubpop
            years <- sort(unique(dframe[subpop_ind, Wyear]))
            nyears <- length(years)

            # Create matrices to contain category estimates and variance
            # estimates for each time period

            catest <- matrix(NA, nlev_ivar, nyears)
            varest <- matrix(NA, nlev_ivar, nyears)

            # Loop through all time periods for this subpopulation

            for (iyear in 1:nyears) {

              # Select sites in a year for this subpopulation

              subpop_ind <- dframe[, itype] %in% isubpop &
                dframe$Wyear %in% years[iyear]

              # Determine whether the time period for this subpopulation is empty

              if (all(is.na(dframe[subpop_ind, ivar]))) {
                warn_ind <- TRUE
                warn <- paste0("Year ", years[iyear], " of Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator\n \"", ivar, "\" contains no data.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              }

              # Determine whether the subpopulation contains a single value

              tst <- !is.na(dframe[subpop_ind, ivar])
              if (sum(tst) == 1) {
                warn_ind <- TRUE
                warn <- paste0("Year ", years[iyear], " of Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator\n \"", ivar, "\" contains a single value.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              }

              # Estimate category proportions for the response variable

              tempdf <- subset(dframe, subpop_ind)
              templev <- levels(tempdf[, ivar])
              temp <- category_est(
                NULL, tempdf, itype, isubpop, 1, ivar, templev,
                length(templev), subset(design, subpop_ind), design_names,
                vartype, conf, mult, warn_ind, warn_df
              )
              temp_cat <- temp$catsum
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

              # Assign the category estimate and variance estimate for each trend
              # category

              for (icat in 1:nlev_ivar) {
                if (lev_ivar[icat] %in% temp_cat$Category) {
                  tst <- temp_cat$Category == lev_ivar[icat]
                  catest[icat, iyear] <- temp_cat$Estimate.P[tst]
                  varest[icat, iyear] <- (temp_cat$StdError.P[tst])^2
                } else {
                  catest[icat, iyear] <- 0
                  varest[icat, iyear] <- 0
                }
              }

              # End of the loop for years
            }

            # Perform linear regression and assign results

            for (icat in 1:nlev_ivar) {
              tst <- !is.na(catest[icat, ])
              if (all(tst == FALSE)) {
                warn_ind <- TRUE
                warn <- paste0("Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator \"", ivar, "\" \ncontains no data for the \"", lev_ivar[icat], "\" category.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              } else if (sum(tst) < 3) {
                warn_ind <- TRUE
                warn <- paste0("Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator \"", ivar, "\" \ncontains less than three years of data for the \"", lev_ivar[icat], "\" category.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              }
              if (model_cat == "SLR") {
                regest <- lm(catest[icat, tst] ~ years[tst])
              } else {
                regest <- lm(catest[icat, tst] ~ years[tst],
                  weights = 1 / varest[icat, tst]
                )
              }
              coeff <- summary(regest)$coefficients
              cint <- confint(regest, level = conf / 100)
              trend <- coeff[, "Estimate"][2]
              t_stderror <- coeff[, "Std. Error"][2]
              t_lcb <- cint["years[tst]", ][1]
              t_ucb <- cint["years[tst]", ][2]
              t_pval <- coeff[, "Pr(>|t|)"][2]
              intercept <- coeff[, "Estimate"][1]
              i_stderror <- coeff[, "Std. Error"][1]
              i_lcb <- cint["(Intercept)", ][1]
              i_ucb <- cint["(Intercept)", ][2]
              i_pval <- coeff[, "Pr(>|t|)"][1]
              rsq <- summary(regest)$r.squared
              adjrsq <- summary(regest)$adj.r.squared

              # Assign estimates for the response variable to a data frame

              trendsum$catsum <- rbind(trendsum$catsum, data.frame(
                itype, isubpop, ivar, lev_ivar[icat], trend, t_stderror, t_lcb,
                t_ucb, t_pval, intercept, i_stderror, i_lcb, i_ucb, i_pval, rsq,
                adjrsq
              ))
            }

            # Section for a generalized linear mixed-effects model
          } else if (invprboot == TRUE) {

            # Fit the model

            if (stratum_ind) {
              eval(parse(text = paste0(
                "regest <- glmer(", ivar, " ~ ",
                cat_rhs, " + ", stratumID, ", data = dframe",
                ", family = binomial",
                ", control = glmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            } else {
              eval(parse(text = paste0(
                "regest <- glmer(", ivar, " ~ ",
                cat_rhs, ", data = dframe, family = binomial",
                ", control = glmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            }

            # Create objects that are required for assigning names to the output
            # data frame

            coeff <- summary(regest)$coefficients
            cint <- confint(regest,
              parm = "beta_", level = conf / 100,
              method = "Wald"
            )
            pval <- coeff[, "Pr(>|z|)"]
            fixed <- matrix(0, 5, nrow(coeff))
            fixed[1, ] <- coeff[, "Estimate"]
            fixed[2, ] <- coeff[, "Std. Error"]
            fixed[3, ] <- cint[, 1]
            fixed[4, ] <- cint[, 2]
            fixed[5, ] <- pval
            vcor <- as.data.frame(VarCorr(regest))
            varcomp <- vector("numeric", nrow(vcor) + sum(!is.na(vcor$vars)))
            for (i in 1:length(varcomp)) {
              if (is.na(vcor$var2[i])) {
                varcomp[i] <- vcor$vcov[i]
              } else {
                varcomp[i] <- vcor$sdcor[i]
              }
            }
            varcomp <- c(varcomp, 1.0)

            # Call the bootstrap function

            if (stratum_ind) {
              bootest <- boot(dframe, bootfcn, nboot,
                strata = dframe[, stratumID], weights = bootwgt,
                model = model_cat, ivar = ivar, rhs = cat_rhs, conf = conf
              )
            } else {
              bootest <- boot(dframe, bootfcn, nboot,
                weights = bootwgt,
                model = model_cat, ivar = ivar, rhs = cat_rhs, conf = conf
              )
            }

            # Calculate bootstrap estimates

            estimates <- apply(bootest$t, 2, mean)

            # Assign estimates for the response variable to a data frame

            trendsum$catsum <- rbind(trendsum$catsum, data.frame(t(c(
              itype, isubpop, ivar, estimates
            ))))
          } else {

            # Fit the model

            if (stratum_ind) {
              eval(parse(text = paste0(
                "regest <- glmer(", ivar, " ~ ",
                cat_rhs, " + ", stratumID, ", data = dframe",
                ", family = binomial",
                ", control = glmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            } else {
              eval(parse(text = paste0(
                "regest <- glmer(", ivar, " ~ ",
                cat_rhs, ", data = dframe, family = binomial",
                ", control = glmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            }

            # Calculate estimates

            coeff <- summary(regest)$coefficients
            cint <- confint(regest,
              parm = "beta_", level = conf / 100,
              method = "Wald"
            )
            pval <- coeff[, "Pr(>|z|)"]
            fixed <- matrix(0, 5, nrow(coeff))
            fixed[1, ] <- coeff[, "Estimate"]
            fixed[2, ] <- coeff[, "Std. Error"]
            fixed[3, ] <- cint[, 1]
            fixed[4, ] <- cint[, 2]
            fixed[5, ] <- pval
            vcor <- as.data.frame(VarCorr(regest))
            varcomp <- vector("numeric", nrow(vcor) + sum(!is.na(vcor$vars)))
            for (i in 1:length(varcomp)) {
              if (is.na(vcor$var2[i])) {
                varcomp[i] <- vcor$vcov[i]
              } else {
                varcomp[i] <- vcor$sdcor[i]
              }
            }
            varcomp <- c(varcomp, 1.0)
            AIC <- extractAIC(regest)[2]
            if (attr(terms(regest), "intercept") == 1) {
              if (ncol(fixed) > 2) {
                estimates <- c(
                  fixed[, 2],
                  as.vector(fixed[, c(1, 3:ncol(fixed))]), varcomp, AIC
                )
              } else {
                estimates <- c(fixed[, 2], fixed[, 1], varcomp, AIC)
              }
            } else {
              estimates <- c(as.vector(fixed), varcomp, AIC)
            }

            # Assign estimates for the response variable to a data frame

            trendsum$catsum <- rbind(trendsum$catsum, data.frame(t(c(
              itype, isubpop, ivar, estimates
            ))))
          }

          # End of the loop for levels of a subpopulation
        }

        # End of the loop for response variables
      }

      # End of the loop for subpopulations
    }

    # End of the section for categorical response variables
  }

  #
  # Begin the section for continuous response variables
  #

  if (!is.null(vars_cont)) {

    # Assign values to the site ID variable in the dframe data frame

    if (model_cont %in% c("SLR", "WLR")) {
      dframe[, siteID] <- dframe$siteID_unique
    } else {
      dframe[, siteID] <- dframe$siteID_orginal
    }

    # Loop through all subpopulations (domains)

    for (itype in subpops) {
      lev_itype <- levels(dframe[, itype])
      nlev_itype <- length(lev_itype)

      # Loop through all  response variables

      for (ivar in vars_cont) {

        # Loop through all levels of a subpopulation

        for (isubpop in lev_itype) {

          # Section for the simple linear regression and weighted linear
          # regression models

          if (model_cont %in% c("SLR", "WLR")) {

            # Determine the set of years for this subpopulation

            subpop_ind <- dframe[, itype] %in% isubpop
            years <- sort(unique(dframe[subpop_ind, Wyear]))
            nyears <- length(years)

            # Create vectors to contain mean estimates and variance estimates
            # for each time period

            contest <- rep(NA, nyears)
            varest <- rep(NA, nyears)

            # Loop through all time periods for this subpopulation

            for (iyear in 1:nyears) {

              # Select sites in a year for this subpopulation

              subpop_ind <- dframe[, itype] %in% isubpop &
                dframe$Wyear %in% years[iyear]

              # Determine whether the time period for this subpopulation is
              # empty

              if (all(is.na(dframe[subpop_ind, ivar]))) {
                warn_ind <- TRUE
                warn <- paste0("Year ", years[iyear], " of Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator\n \"", ivar, "\" contains no data.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              }

              # Determine whether the subpopulation contains a single value

              tst <- !is.na(dframe[subpop_ind, ivar])
              if (sum(tst) == 1) {
                warn_ind <- TRUE
                warn <- paste0("Year ", years[iyear], " of Subpopulation \"", isubpop, "\" of population type \"", itype, "\" for indicator\n \"", ivar, "\" contains a single value.\n")
                act <- "None.\n"
                warn_df <- rbind(warn_df, data.frame(
                  func = I(fname), subpoptype = I(itype), subpop = I(isubpop),
                  indicator = I(ivar), stratum = NA, warning = I(warn), action = I(act)
                ))
                next
              }

              # Estimate the mean for the response variable

              temp <- mean_est(
                NULL, droplevels(subset(dframe, subpop_ind)),
                itype, isubpop, 1, ivar, subset(design, subpop_ind),
                design_names, NULL, vartype, conf, mult, warn_ind, warn_df
              )
              temp_cont <- temp$meansum
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

              # Assign the mean estimate and variance estimate

              contest[iyear] <- temp_cont$Estimate
              varest[iyear] <- (temp_cont$StdError)^2

              # End of the loop for years
            }

            # Perform linear regression and assign results

            if (model_cont == "SLR") {
              regest <- lm(contest ~ years)
            } else {
              regest <- lm(contest ~ years, weights = 1 / varest)
            }
            coeff <- summary(regest)$coefficients
            cint <- confint(regest, level = conf / 100)
            trend <- coeff[, "Estimate"][2]
            t_stderror <- coeff[, "Std. Error"][2]
            t_lcb <- cint["years", ][1]
            t_ucb <- cint["years", ][2]
            t_pval <- coeff[, "Pr(>|t|)"][2]
            intercept <- coeff[, "Estimate"][1]
            i_stderror <- coeff[, "Std. Error"][1]
            i_lcb <- cint["(Intercept)", ][1]
            i_ucb <- cint["(Intercept)", ][2]
            i_pval <- coeff[, "Pr(>|t|)"][1]
            rsq <- summary(regest)$r.squared
            adjrsq <- summary(regest)$adj.r.squared

            # Assign estimates for the response variable to a data frame

            trendsum$contsum <- rbind(
              trendsum$contsum,
              data.frame(
                itype, isubpop, ivar, trend, t_stderror, t_lcb, t_ucb,
                t_pval, intercept, i_stderror, i_lcb, i_ucb, i_pval, rsq, adjrsq
              )
            )

            # Section for a linear mixed-effects model
          } else if (invprboot == TRUE) {

            # Fit the model

            if (stratum_ind) {
              eval(parse(text = paste0(
                "regest <- lmer(", ivar, " ~ ",
                cont_rhs, " + ", stratumID, ", data = dframe",
                ", control = lmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            } else {
              eval(parse(text = paste0(
                "regest <- lmer(", ivar, " ~ ",
                cont_rhs, ", data = dframe",
                ", control = lmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            }

            # Create objects that are required for assigning names to the output
            # data frame

            coeff <- summary(regest)$coefficients
            cint <- confint(regest,
              parm = "beta_", level = conf / 100,
              method = "Wald"
            )
            dfval <- df.residual(regest)
            tvalue <- coeff[, "t value"]
            pval <- 2 * (1 - pt(abs(tvalue), dfval))
            fixed <- matrix(0, 5, nrow(coeff))
            fixed[1, ] <- coeff[, "Estimate"]
            fixed[2, ] <- coeff[, "Std. Error"]
            fixed[3, ] <- cint[, 1]
            fixed[4, ] <- cint[, 2]
            fixed[5, ] <- pval
            vcor <- as.data.frame(VarCorr(regest))
            varcomp <- vector("numeric", nrow(vcor))
            for (i in 1:length(varcomp)) {
              if (is.na(vcor$var2[i])) {
                varcomp[i] <- vcor$vcov[i]
              } else {
                varcomp[i] <- vcor$sdcor[i]
              }
            }

            # Call the bootstrap function

            if (stratum_ind) {
              bootest <- boot(dframe, bootfcn, nboot,
                strata = dframe[, stratumID], weights = bootwgt,
                model = model_cont, ivar = ivar, rhs = cont_rhs, conf = conf
              )
            } else {
              bootest <- boot(dframe, bootfcn, nboot,
                weights = bootwgt,
                model = model_cont, ivar = ivar, rhs = cont_rhs, conf = conf
              )
            }

            # Calculate bootstrap estimates

            estimates <- apply(bootest$t, 2, mean)

            # Assign estimates for the response variable to a data frame

            trendsum$contsum <- rbind(
              trendsum$contsum,
              data.frame(t(c(
                itype, isubpop, ivar, estimates
              )))
            )
          } else {

            # Fit the model

            if (stratum_ind) {
              eval(parse(text = paste0(
                "regest <- lmer(", ivar, " ~ ",
                cont_rhs, " + ", stratumID, ", data = dframe",
                ", control = lmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            } else {
              eval(parse(text = paste0(
                "regest <- lmer(", ivar, " ~ ",
                cont_rhs, ", data = dframe",
                ", control = lmerControl(check.nobs.vs.nRE = 'warning'",
                ", check.conv.singular = lme4::.makeCC(action = 'warning',",
                " tol = formals(lme4::isSingular)$tol)))"
              )))
            }

            # Calculate estimates

            coeff <- summary(regest)$coefficients
            cint <- confint(regest,
              parm = "beta_", level = conf / 100,
              method = "Wald"
            )
            dfval <- df.residual(regest)
            tvalue <- coeff[, "t value"]
            pval <- 2 * (1 - pt(abs(tvalue), dfval))
            fixed <- matrix(0, 5, nrow(coeff))
            fixed[1, ] <- coeff[, "Estimate"]
            fixed[2, ] <- coeff[, "Std. Error"]
            fixed[3, ] <- cint[, 1]
            fixed[4, ] <- cint[, 2]
            fixed[5, ] <- pval
            vcor <- as.data.frame(VarCorr(regest))
            varcomp <- vector("numeric", nrow(vcor))
            for (i in 1:length(varcomp)) {
              if (is.na(vcor$var2[i])) {
                varcomp[i] <- vcor$vcov[i]
              } else {
                varcomp[i] <- vcor$sdcor[i]
              }
            }
            AIC <- extractAIC(regest)[2]
            if (attr(terms(regest), "intercept") == 1) {
              if (ncol(fixed) > 2) {
                estimates <- c(
                  fixed[, 2],
                  as.vector(fixed[, c(1, 3:ncol(fixed))]), varcomp, AIC
                )
              } else {
                estimates <- c(fixed[, 2], fixed[, 1], varcomp, AIC)
              }
            } else {
              estimates <- c(as.vector(fixed), varcomp, AIC)
            }

            # Assign estimates for the response variable to a data frame

            trendsum$contsum <- rbind(
              trendsum$contsum,
              data.frame(t(c(
                itype, isubpop, ivar, estimates
              )))
            )
          }

          # End of the loop for levels of a subpopulation
        }

        # End of the loop for response variables
      }

      # End of the loop for subpopulations
    }

    # End of the section for continuous response variables
  }

  # Assign row names and column names to the output data frames

  if (!is.null(trendsum$catsum)) {
    nrows <- nrow(trendsum$catsum)
    if (model_cat %in% c("SLR", "WLR")) {
      dimnames(trendsum$catsum) <- list(1:nrows, c(
        "Type", "Subpopulation",
        "Indicator", "Category", "Trend_Estimate", "Trend_Std_Error",
        paste0("Trend_LCB", conf, "Pct"), paste0("Trend_UCB", conf, "Pct"),
        "Trend_p_Value", "Intercept_Estimate", "Intercept_Std_Error",
        paste0("Intercept_LCB", conf, "Pct"), paste0(
          "Intercept_UCB", conf,
          "Pct"
        ), "Intercept_p_Value", "R_Squared", "Adj_R_Squared"
      ))
    } else {
      if (PO_ind_cat == TRUE) {
        dimnames(trendsum$catsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error", paste0(
            "Trend_LCB",
            conf, "Pct"
          ), paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          "Intercept_Estimate", "Intercept_Std_Error", paste0(
            "Intercept_LCB",
            conf, "Pct"
          ), paste0("Intercept_UCB", conf, "Pct"),
          "Intercept_p_Value", "Var_SiteID_Int", "Var_SiteID_Trend",
          "Corr_SiteID_Int_Trend", "Var_YearID", "Var_Residual", "AIC"
        ))
      } else if (attr(terms(regest), "intercept") == 1) {
        fixed_names <- NULL
        if (ncol(fixed) > 2) {
          fnames <- rownames(coeff)
          for (i in 3:ncol(fixed)) {
            temp <- fnames[i]
            fixed_names <- c(
              fixed_names, paste0(temp, "_Estimate"),
              paste0(temp, "_Std_Error"), paste0(temp, "_LCB", conf, "Pct"),
              paste0(temp, "_UCB", conf, "Pct"), paste0(temp, "_p_Value")
            )
          }
        }
        varcomp_names <- NULL
        vnames <- vcor$grp
        for (i in 1:nrow(vcor)) {
          if (is.na(vcor$var2[i])) {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                "Int",
                sep = "_"
              ))
            } else {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                vcor$var1[i],
                sep = "_"
              ))
            }
          } else {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(
                varcomp_names,
                paste("Corr", vnames[i], "Int", vcor$var2[i], sep = "_")
              )
            } else {
              varcomp_names <- c(varcomp_names, paste("Corr", vnames[1],
                vcor$var1[i], vcor$var2[i],
                sep = "_"
              ))
            }
          }
        }
        varcomp_names <- gsub("Wyear", "Trend", varcomp_names)
        dimnames(trendsum$catsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error",
          paste0("Trend_LCB", conf, "Pct"),
          paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          "Intercept_Estimate", "Intercept_Std_Error",
          paste0("Intercept_LCB", conf, "Pct"),
          paste0("Intercept_UCB", conf, "Pct"), "Intercept_p_Value",
          fixed_names, varcomp_names, "Var_Residual", "AIC"
        ))
      } else {
        fixed_names <- NULL
        if (ncol(fixed) > 1) {
          fnames <- rownames(coeff)
          for (i in 2:ncol(fixed)) {
            temp <- fnames[i]
            fixed_names <- c(
              fixed_names, paste0(temp, "_Estimate"),
              paste0(temp, "_Std_Error"), paste0(temp, "_LCB", conf, "Pct"),
              paste0(temp, "_UCB", conf, "Pct"), paste0(temp, "_p_Value")
            )
          }
        }
        varcomp_names <- NULL
        vnames <- vcor$grp
        for (i in 1:nrow(vcor)) {
          if (is.na(vcor$var2[i])) {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                "Int",
                sep = "_"
              ))
            } else {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                vcor$var1[i],
                sep = "_"
              ))
            }
          } else {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(
                varcomp_names,
                paste("Corr", vnames[i], "Int", vcor$var2[i], sep = "_")
              )
            } else {
              varcomp_names <- c(varcomp_names, paste("Corr", vnames[1],
                vcor$var1[i], vcor$var2[i],
                sep = "_"
              ))
            }
          }
        }
        varcomp_names <- gsub("Wyear", "Trend", varcomp_names)
        dimnames(trendsum$catsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error",
          paste0("Trend_LCB", conf, "Pct"),
          paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          fixed_names, varcomp_names, "Var_Residual", "AIC"
        ))
      }
    }
  }

  if (!is.null(trendsum$contsum)) {
    nrows <- nrow(trendsum$contsum)
    if (model_cont %in% c("SLR", "WLR")) {
      dimnames(trendsum$contsum) <- list(1:nrows, c(
        "Type", "Subpopulation",
        "Indicator", "Trend_Estimate", "Trend_Std_Error", paste0(
          "Trend_LCB",
          conf, "Pct"
        ), paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
        "Intercept_Estimate", "Intercept_Std_Error", paste0(
          "Intercept_LCB",
          conf, "Pct"
        ), paste0("Intercept_UCB", conf, "Pct"),
        "Intercept_p_Value", "R_Squared", "Adj_R_Squared"
      ))
    } else {
      if (PO_ind_cont == TRUE) {
        dimnames(trendsum$contsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error", paste0(
            "Trend_LCB",
            conf, "Pct"
          ), paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          "Intercept_Estimate", "Intercept_Std_Error", paste0(
            "Intercept_LCB",
            conf, "Pct"
          ), paste0("Intercept_UCB", conf, "Pct"),
          "Intercept_p_Value", "Var_SiteID_Int", "Var_SiteID_Trend",
          "Corr_SiteID_Int_Trend", "Var_YearID", "Var_Residual", "AIC"
        ))
      } else if (attr(terms(regest), "intercept") == 1) {
        fixed_names <- NULL
        if (ncol(fixed) > 2) {
          fnames <- rownames(coeff)
          for (i in 3:ncol(fixed)) {
            temp <- fnames[i]
            fixed_names <- c(
              fixed_names, paste0(temp, "_Estimate"),
              paste0(temp, "_Std_Error"), paste0(temp, "_LCB", conf, "Pct"),
              paste0(temp, "_UCB", conf, "Pct"), paste0(temp, "_p_Value")
            )
          }
        }
        varcomp_names <- NULL
        vnames <- vcor$grp
        for (i in 1:(nrow(vcor) - 1)) {
          if (is.na(vcor$var2[i])) {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                "Int",
                sep = "_"
              ))
            } else {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                vcor$var1[i],
                sep = "_"
              ))
            }
          } else {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(
                varcomp_names,
                paste("Corr", vnames[i], "Int", vcor$var2[i], sep = "_")
              )
            } else {
              varcomp_names <- c(varcomp_names, paste("Corr", vnames[1],
                vcor$var1[i], vcor$var2[i],
                sep = "_"
              ))
            }
          }
        }
        varcomp_names <- gsub("Wyear", "Trend", varcomp_names)
        dimnames(trendsum$contsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error",
          paste0("Trend_LCB", conf, "Pct"),
          paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          "Intercept_Estimate", "Intercept_Std_Error",
          paste0("Intercept_LCB", conf, "Pct"),
          paste0("Intercept_UCB", conf, "Pct"), "Intercept_p_Value",
          fixed_names, varcomp_names, "Var_Residual", "AIC"
        ))
      } else {
        fixed_names <- NULL
        if (ncol(fixed) > 1) {
          fnames <- rownames(coeff)
          for (i in 2:ncol(fixed)) {
            temp <- fnames[i]
            fixed_names <- c(
              fixed_names, paste0(temp, "_Estimate"),
              paste0(temp, "_Std_Error"), paste0(temp, "_LCB", conf, "Pct"),
              paste0(temp, "_UCB", conf, "Pct"), paste0(temp, "_p_Value")
            )
          }
        }
        varcomp_names <- NULL
        vnames <- vcor$grp
        for (i in 1:(nrow(vcor) - 1)) {
          if (is.na(vcor$var2[i])) {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                "Int",
                sep = "_"
              ))
            } else {
              varcomp_names <- c(varcomp_names, paste("Var", vnames[i],
                vcor$var1[i],
                sep = "_"
              ))
            }
          } else {
            if (vcor$var1[i] %in% "(Intercept)") {
              varcomp_names <- c(
                varcomp_names,
                paste("Corr", vnames[i], "Int", vcor$var2[i], sep = "_")
              )
            } else {
              varcomp_names <- c(varcomp_names, paste("Corr", vnames[1],
                vcor$var1[i], vcor$var2[i],
                sep = "_"
              ))
            }
          }
        }
        varcomp_names <- gsub("Wyear", "Trend", varcomp_names)
        dimnames(trendsum$contsum) <- list(1:nrows, c(
          "Type", "Subpopulation",
          "Indicator", "Trend_Estimate", "Trend_Std_Error",
          paste0("Trend_LCB", conf, "Pct"),
          paste0("Trend_UCB", conf, "Pct"), "Trend_p_Value",
          fixed_names, varcomp_names, "Var_Residual", "AIC"
        ))
      }
    }
  }

  # As necessary, output a message indicating that warning messages were
  # generated during execution of the program

  if (warn_ind) {
    warn_df <<- warn_df
    if (nrow(warn_df) == 1) {
      cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn_df'.  Enter the following command \nto view the warning message: warnprnt()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn_df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn_df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # Return the output object

  trendsum
}
