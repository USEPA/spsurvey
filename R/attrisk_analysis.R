################################################################################
# Function: attrisk_analysis (exported)
# Programmer: Tom Kincaid
# Date: July 16, 2020
# Revised: August 14, 2020 to allow use of an sf object as the input data
#          argument (dframe)
# Revised: December 16, 2020 to use a new function named survey_design to create
#          the survey design object
# Revised: January 28, 2021 to replace "pcfactor.ind" with "pcfactor_ind"
# Revised: March 2, 2021 to revise the process for creating unique site ID
#          values
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
# Revised: June 11, 2021 to modify the approach for specification of arguments
#          response_levels and stressor_levels
# Revised: July 28, 2021 to improve documentation
# Revised: September 9, 2021 to revise the documentation for argument popsize
#
#' Attributable risk analysis
#'
#' This function organizes input and output for the analysis of attributable risk (for
#' categorical variables).  The analysis data,
#' \code{dframe}, can be either a data frame or a simple features (\code{sf}) object.  If an
#' \code{sf} object is used, coordinates are extracted from the geometry column in the
#' object, arguments \code{xcoord} and \code{ycoord} are assigned values
#' \code{"xcoord"} and \code{"ycoord"}, respectively, and the geometry column is
#' dropped from the object.
#'
#' @param dframe Data to be analyzed (analysis data). A data frame or
#'   \code{sf} object containing survey design
#'   variables, response variables, stressor variables, and subpopulation
#'   (domain) variables.
#'
#' @param vars_response Vector composed of character values that identify the
#'   names of response variables in \code{dframe}. Each response
#'   variable must have two category values (levels), where one level is
#'   associated with poor condition and the other level is associated with good
#'   condition.
#'
#' @param vars_stressor Vector composed of character values that identify the
#'   names of stressor variables in \code{dframe}. Each stressor
#'   variable must have two category values (levels), where one level is
#'   associated with poor condition and the other level is associated with good
#'   condition.
#'
#' @param response_levels List providing the category values (levels) for each
#'   element in the \code{vars_response} argument.  Each element in the list
#'   must contain two values, where the first value identifies poor condition,
#'   and the second value identifies good condition.  This argument must be
#'   named and must be the same length as argument \code{vars_response}.  Names
#'   for this argument must match the values in the \code{vars_response}
#'   argument. If this argument equals NULL, then a named list is created that
#'   contains the values \code{"Poor"} and \code{"Good"} for the first and
#'   second levels, respectively, of each element in the \code{vars_response}
#'   argument and that uses values in the \code{vars_response} argument as names
#'   for the list.  The default value is NULL.
#'
#' @param stressor_levels List providing the category values (levels) for each
#'   element in the \code{vars_stressor} argument.  Each element in the list
#'   must contain two values, where the first value identifies poor condition,
#'   and the second value identifies good condition.  This argument must be
#'   named and must be the same length as argument \code{vars_stressor}.  Names
#'   for this argument must match the values in the \code{vars_stressor}
#'   argument. If this argument equals NULL, then a named list is created that
#'   contains the values \code{"Poor"} and \code{"Good"} for the first and
#'   second levels, respectively, of each element in the \code{vars_stressor}
#'   argument and that uses values in the \code{vars_stressor} argument as names
#'   for the list.  The default value is NULL.
#'
#' @param subpops Vector composed of character values that identify the
#'   names of subpopulation (domain) variables in \code{dframe}.
#'   If a value is not provided, the value \code{"All_Sites"} is assigned to the
#'   subpops argument and a factor variable named \code{"All_Sites"} that takes
#'   the value \code{"All Sites"} is added to \code{dframe}.  The
#'   default value is \code{NULL}.
#'
#' @param siteID Character value providing the name of the site ID variable in
#'   \code{dframe}.  For a two-stage sample, the site ID variable
#'   identifies stage two site IDs.  The default value is \code{NULL}, which
#'   assumes that each row in \code{dframe} represents a unique site.
#'
#' @param weight Character value providing the name of the design weight
#'   variable in \code{dframe}.  For a two-stage sample, the
#'   weight variable identifies stage two weights.  The default value is
#'   \code{"weight"}.
#'
#' @param xcoord Character value providing name of the x-coordinate variable in
#'   \code{dframe}.  For a two-stage sample, the x-coordinate
#'   variable identifies stage two x-coordinates.  Note that x-coordinates are
#'   required for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the x-coordinate). The default
#'   value is \code{NULL}.
#'
#' @param ycoord Character value providing name of the y-coordinate variable in
#'   \code{dframe}.  For a two-stage sample, the y-coordinate
#'   variable identifies stage two y-coordinates.  Note that y-coordinates are
#'   required for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the t-coordinate). The default
#'   value is \code{NULL}.
#'
#' @param stratumID Character value providing the name of the stratum ID
#'   variable in \code{dframe}.  The default value is
#'   \code{NULL}.
#'
#' @param clusterID Character value providing the name of the cluster
#'   (stage one) ID variable in \code{dframe}.  Note that cluster
#'   IDs are required for a two-stage sample.  The default value is \code{NULL}.
#'
#' @param weight1 Character value providing the name of the stage one weight
#'   variable in \code{dframe}.  The default value is
#'   \code{NULL}.
#'
#' @param xcoord1 Character value providing the name of the stage one
#'   x-coordinate variable in \code{dframe}.  Note that x
#'   coordinates are required for calculation of the local mean variance
#'   estimator.  The default value is \code{NULL}.
#'
#' @param ycoord1 Character value providing the name of the stage one
#'   y-coordinate variable in \code{dframe}.  Note that
#'   y-coordinates are required for calculation of the local mean variance
#'   estimator.  The default value is \code{NULL}.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation, where \code{TRUE} uses size weights and
#'   \code{FALSE} does not use size weights. To employ size weights for a
#'   single-stage sample, a value must be supplied for argument weight.  To
#'   employ size weights for a two-stage sample, values must be supplied for
#'   arguments \code{weight} and \code{weight1}. The default value is
#'   \code{FALSE}.
#'
#' @param sweight Character value providing the name of the size weight variable
#'   in \code{dframe}.  For a two-stage sample, the size weight
#'   variable identifies stage two size weights.  The default value is
#'   \code{NULL}.
#'
#' @param sweight1 Character value providing the name of the stage one size
#' weight variable in \code{dframe}.  The default value is
#' \code{NULL}.
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
#'   for the cluster.  The name for the first item in the vector is arbitrary.
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
#' @param vartype Character value providing the choice of the variance
#'   estimator, where \code{"Local"} indicates the local mean estimator and \code{"SRS"} indicates the
#'   simple random sampling estimator.  The default value is \code{"Local"}.
#'
#' @param conf Numeric value providing the Gaussian-based confidence level.  The default value
#'   is \code{95}.
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
#' Attributable risk measures the proportional reduction in the extent of poor
#' condition of a response variable that presumably would result from
#' eliminating a stressor variable, where the response and stressor variables
#' are classified as either good (i.e., reference condition) or poor (i.e.,
#' different from reference condition).  Attributable risk is defined as one
#' minus the ratio of two probabilities.  The numerator of the ratio is the
#' conditional probability that the response variable is in poor condition given
#' that the stressor variable is in good condition.   The denominator of the
#' ratio is the probability that the response variable is in poor condition.
#' Attributable risk values close to zero indicate that removing the stressor
#' variable will have little or no impact on the probability that the response
#' variable is in poor condition.  Attributable risk values close to one
#' indicate that removing the stressor variable will result in extensive
#' reduction of the probability that the response variable is in poor condition.
#'
#' @return The analysis results. A data frame of population estimates for all combinations of
#'   subpopulations, categories within each subpopulation, response variables,
#'   and categories within each response variable.  Estimates are provided for
#'   proportion and size of the population plus standard error, margin of
#'   error, and confidence interval estimates. The data frame contains the following
#'   variables:
#'   \describe{
#'     \item{Type}{subpopulation (domain) name}
#'     \item{Subpopulation}{subpopulation name within a domain}
#'     \item{Response}{response variable}
#'     \item{Stressor}{stressor variable}
#'     \item{nResp}{sample size}
#'     \item{Estimate}{attributable risk estimate}
#'     \item{StdError_log}{attributable risk standard error (on the log scale)}
#'     \item{MarginofError_log}{attributable risk margin of error (on the log scale)}
#'     \item{LCBxxPct}{xx\% (default 95\%) lower confidence bound}
#'     \item{UCBxxPct}{xx\% (default 95\%) upper confidence bound}
#'     \item{WeightTotal}{sum of design weights}
#'     \item{Count_RespPoor_StressPoor}{number of observations in the poor response and poor stressor group}
#'     \item{Count_RespPoor_StressGood}{number of observations in the poor response and good stressor group}
#'     \item{Count_RespGood_StressPoor}{number of observations in the good response and poor stressor group}
#'     \item{Count_RespGood_StressGood}{number of observations in the good response and good stressor group}
#'     \item{Prop_RespPoor_StressPoor}{weighted proportion of observations in the poor response and poor stressor group}
#'     \item{Prop_RespPoor_StressGood}{weighted proportion of observations in the poor response and good stressor group}
#'     \item{Prop_RespGood_StressPoor}{weighted proportion of observations in the good response and poor stressor group}
#'     \item{Prop_RespGood_StressGood}{weighted proportion of observations in the good response and good stressor group}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey univar
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{relrisk_analysis}}}{ for relative risk analysis}
#'   \item{\code{\link{diffrisk_analysis}}}{ for risk difference analysis}
#'   }
#'
#' @references
#'   Sickle, J. V., & Paulsen, S. G. (2008). Assessing the attributable risks,
#'    relative risks, and regional extents of aquatic stressors.
#'    \emph{Journal of the North American Benthological Society}, 27(4), 920-931.
#'
#' @examples
#' dframe <- data.frame(
#'   siteID = paste0("Site", 1:100),
#'   wgt = runif(100, 10, 100),
#'   xcoord = runif(100),
#'   ycoord = runif(100),
#'   stratum = rep(c("Stratum1", "Stratum2"), 50),
#'   RespVar1 = sample(c("Poor", "Good"), 100, replace = TRUE),
#'   RespVar2 = sample(c("Poor", "Good"), 100, replace = TRUE),
#'   StressVar = sample(c("Poor", "Good"), 100, replace = TRUE),
#'   All_Sites = rep("All Sites", 100),
#'   Resource_Class = rep(c("Agr", "Forest"), c(55, 45))
#' )
#' myresponse <- c("RespVar1", "RespVar2")
#' mystressor <- c("StressVar")
#' mysubpops <- c("All_Sites", "Resource_Class")
#' attrisk_analysis(dframe,
#'   vars_response = myresponse,
#'   vars_stressor = mystressor, subpops = mysubpops, siteID = "siteID",
#'   weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
#'   stratumID = "stratum"
#' )
#' @export
################################################################################

attrisk_analysis <- function(dframe, vars_response, vars_stressor, response_levels = NULL,
                             stressor_levels = NULL, subpops = NULL, siteID = NULL, weight = "weight",
                             xcoord = NULL, ycoord = NULL, stratumID = NULL, clusterID = NULL,
                             weight1 = NULL, xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE,
                             sweight = NULL, sweight1 = NULL, fpc = NULL, popsize = NULL,
                             vartype = "Local", conf = 95, All_Sites = FALSE) {

  # Create a vector for error messages

  error_ind <- FALSE
  error_vec <- NULL

  # Create a data frame for warning messages

  warn_ind <- FALSE
  warn_df <- NULL
  fname <- "attrisk_analysis"

  # Ensure that the dframe argument was provided

  if (missing(dframe) | is.null(dframe)) {
    stop("\nThe dframe argument must be provided.\n")
  }

  # If the dframe argument is an sf object, extract coordinates from the
  # geometry column, assign values "xcoord" and "ycoord" to arguments xcoord
  # and ycoord, respectively, and drop the geometry column from the object

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

  # If no siteID is provided, set one that assumes each row is a unique site

  if (is.null(siteID)) {
    siteID <- "siteID"
    dframe$siteID <- paste("site", seq_len(nrow(dframe)), sep = "-")
  }

  # Ensure that the dframe data frame contains the site ID variable

  if (!(siteID %in% names(dframe))) {
    ind <- FALSE
    error_ind <- TRUE
    msg <- paste0("The name provided for the siteID argument, \"", siteID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {
    ind <- TRUE
  }

  # Check site IDs for repeat values and, as necessary, create unique site IDs
  # and output a warning message

  if (ind) {
    IDs <- dframe[, siteID]
    temp <- sapply(split(IDs, IDs), length)
    if (any(temp > 1)) {
      warn_ind <- TRUE
      temp_str <- vecprint(names(temp)[temp > 1])
      warn <- paste("The following site ID values occur more than once among the values that were \ninput to the function:\n", temp_str)
      act <- "Unique site ID values were created.\n"
      warn_df <- rbind(warn_df, data.frame(
        func = I(fname), subpoptype = NA,
        subpop = NA, indicator = NA, stratum = NA, warning = I(warn), action = I(act)
      ))
      dframe[, siteID] <- uniqueID(dframe[, siteID])
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

  # Ensure that a value was provided for the vars_response (response variable
  # names) argument

  if (missing(vars_response)) {
    error_ind <- TRUE
    msg <- "A value must be provided for the vars_response (response variable names) argument.\n"
    error_vec <- c(error_vec, msg)
  }

  # Ensure that a value was provided for the vars_stressor (stressor variable
  # names) argument

  if (missing(vars_stressor)) {
    error_ind <- TRUE
    msg <- "A value must be provided for the vars_stressor (stressor variable names) argument.\n"
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

  # Check input arguments

  temp <- input_check(dframe, design_names, vars_response, NULL, vars_stressor,
    NULL, subpops, sizeweight, fpc, popsize, vartype, NULL, conf,
    error_ind = error_ind, error_vec = error_vec
  )
  dframe <- temp$dframe
  vars_response <- temp$vars_cat
  vars_stressor <- temp$vars_stressor
  subpops <- temp$subpops
  popsize <- temp$popsize
  vartype <- temp$vartype
  error_ind <- temp$error_ind
  error_vec <- temp$error_vec

  # Check arguments response_levels and stressor_levels

  if (is.null(response_levels)) {
    response_levels <- rep(list(c("Poor", "Good")), length(vars_response))
    names(response_levels) <- vars_response
  } else {
    if (!is.list(response_levels)) {
      error_ind <- TRUE
      msg <- "Argument response_levels must be a list.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (length(response_levels) != length(vars_response)) {
        error_ind <- TRUE
        msg <- "Argument response_levels must be the same length as argument vars_response.\n"
        error_vec <- c(error_vec, msg)
      }
      if (any(sapply(response_levels, function(x) length(x) != 2))) {
        error_ind <- TRUE
        msg <- "Each element of argument response_levels must contain only two values.\n"
        error_vec <- c(error_vec, msg)
      }
      if (any(sapply(response_levels, function(x) !is.character(x)))) {
        error_ind <- TRUE
        msg <- "Each element of argument response_levels must contain character values.\n"
        error_vec <- c(error_vec, msg)
      }
      if (all(vars_response %in% names(dframe))) {
        tst <- logical(length(vars_response))
        for (i in 1:length(vars_response)) {
          if (!all(response_levels[[i]] %in% levels(dframe[, vars_response[i]]))) {
            tst[i] <- TRUE
          }
        }
        if (any(tst)) {
          temp_str <- vecprint(vars_response[tst])
          error_ind <- TRUE
          msg <- paste0("\nCategory names for the following response variables do not match category names \nin the response_levels argument:\n", temp_str)
          error_vec <- c(error_vec, msg)
        } else {
          if (!all(names(response_levels) %in% vars_response)) {
            error_ind <- TRUE
            msg <- "Names for the response_levels list do not match the values in the vars_response \nargument."
            error_vec <- c(error_vec, msg)
          } else {
            indx <- match(vars_response, names(response_levels))
            response_levels <- response_levels[indx]
          }
        }
      }
    }
  }

  if (is.null(stressor_levels)) {
    stressor_levels <- rep(list(c("Poor", "Good")), length(vars_stressor))
    names(stressor_levels) <- vars_stressor
  } else {
    if (!is.list(stressor_levels)) {
      error_ind <- TRUE
      msg <- "Argument stressor_levels must be a list.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (length(stressor_levels) != length(vars_stressor)) {
        error_ind <- TRUE
        msg <- "Argument stressor_levels must be the same length as argument vars_stressor.\n"
        error_vec <- c(error_vec, msg)
      }
      if (any(sapply(stressor_levels, function(x) length(x) != 2))) {
        error_ind <- TRUE
        msg <- "Each element of argument stressor_levels must contain only two values.\n"
        error_vec <- c(error_vec, msg)
      }
      if (any(sapply(stressor_levels, function(x) !is.character(x)))) {
        error_ind <- TRUE
        msg <- "Each element of argument stressor_levels must contain character values.\n"
        error_vec <- c(error_vec, msg)
      }
      if (all(vars_stressor %in% names(dframe))) {
        tst <- logical(length(vars_stressor))
        for (i in 1:length(vars_stressor)) {
          if (!all(stressor_levels[[i]] %in% levels(dframe[, vars_stressor[i]]))) {
            tst[i] <- TRUE
          }
        }
        if (any(tst)) {
          temp_str <- vecprint(vars_stressor[tst])
          error_ind <- TRUE
          msg <- paste0("\nCategory names for the following stressor variables do not match category names \nin the stressor_levels argument:\n", temp_str)
          error_vec <- c(error_vec, msg)
        } else {
          if (!all(names(stressor_levels) %in% vars_stressor)) {
            error_ind <- TRUE
            msg <- "Names for the stressor_levels list do not match the values in the vars_stressor \nargument."
            error_vec <- c(error_vec, msg)
          } else {
            indx <- match(vars_stressor, names(stressor_levels))
            stressor_levels <- stressor_levels[indx]
          }
        }
      }
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
        warn <- paste("The stratum named \"", stratum_levels[i], "\" contains a single value and was removed from the analysis.\n")
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
    stage1size, vartype, NULL
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
      pop_totals <- numeric(length(cnames))
      names(pop_totals) <- cnames
      pop_totals[1] <- sum(popsize[[1]])
      k <- 2
      for (i in names(popsize)) {
        temp <- popsize[[i]]
        for (j in 2:length(temp)) {
          pop_totals[k] <- temp[j]
          k <- k + 1
        }
      }
      design <- calibrate(design, make.formula(names(popsize)), pop_totals)
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

  # For variables that exist in the design$variables data frame, assign survey
  # design variables

  dframe <- design$variables
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

  # Assign values to weight variables

  if (cluster_ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

  # Create the arsum (results) data frame

  arsum <- NULL

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Loop through all subpopulations (domains)

  for (itype in subpops) {
    lev_itype <- levels(dframe[, itype])
    nlev_itype <- length(lev_itype)

    # Loop through all response variables (vars_response)

    for (ivar_r in vars_response) {

      # Loop through all stressor variables (vars_stressor)

      for (ivar_s in vars_stressor) {

        # Loop through all levels of the subpopulation

        for (isubpop in lev_itype) {
          tst <- !is.na(dframe[, itype]) & dframe[, itype] == isubpop &
            !is.na(dframe[, ivar_r]) & !is.na(dframe[, ivar_s])

          # Compute sum of the weights

          if (cluster_ind) {
            popsize_hat <- sum(wgt1[tst] * wgt2[tst])
          } else {
            popsize_hat <- sum(wgt[tst])
          }

          # Assign response variable values

          response <- dframe[, ivar_r]
          nresp <- length(response)

          # Assign stressor variable values

          stressor <- dframe[, ivar_s]

          # Create the warn_vec object

          warn_vec <- c(itype, isubpop, paste(ivar_r, "and", ivar_s))

          #
          # Branch to handle stratified and unstratified data
          #

          if (stratum_ind) {

            #
            # Begin the section for stratified data
            #

            # Initialize variables for all strata combined

            wgt_total <- 0
            varest <- 0

            #
            # Begin the subsection for individual strata
            #

            for (i in 1:nstrata) {

              # Calculate required values

              stratum_i <- tst & stratumID == stratum_levels[i]
              response_st <- response[stratum_i]
              stressor_st <- stressor[stratum_i]
              if (cluster_ind) {
                wgt1_st <- wgt1[stratum_i]
                wgt2_st <- wgt2[stratum_i]
              } else {
                wgt_st <- wgt[stratum_i]
              }

              # Compute the 2x2 table of weight totals

              wgt_total_st <- svytable(make.formula(paste(
                ivar_r, "+",
                ivar_s
              )), design = subset(design, stratum_i))

              # Calculate the variance-covariance estimate for the cell and
              # marginal totals

              if (cluster_ind) {
                temp <- attrisk_var(
                  response[stratum_i], stressor[stratum_i],
                  response_levels[[ivar_r]], stressor_levels[[ivar_s]],
                  wgt2[stratum_i], xcoord[stratum_i], ycoord[stratum_i],
                  stratum_ind, stratum_levels[i], cluster_ind,
                  clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                  ycoord1[stratum_i], vartype, warn_ind, warn_df, warn_vec
                )
              } else {
                temp <- attrisk_var(response[stratum_i], stressor[stratum_i],
                  response_levels[[ivar_r]], stressor_levels[[ivar_s]],
                  wgt[stratum_i], xcoord[stratum_i], ycoord[stratum_i],
                  stratum_ind, stratum_levels[i], cluster_ind,
                  vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                  warn_vec = warn_vec
                )
              }
              varest.st <- temp$varest
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

              # Add estimates to the variables for all strata combined

              wgt_total <- wgt_total + wgt_total_st
              varest <- varest + varest.st

              #
              # End the subsection for individual strata
              #
            }

            # Add margins to the wgt_total table

            wgt_total <- addmargins(wgt_total)

            # Assign cell weight totals

            total1 <- wgt_total[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][1]
            ]
            total2 <- wgt_total[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][1]
            ]
            total3 <- wgt_total[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][2]
            ]
            total4 <- wgt_total[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][2]
            ]

            # Calculate the estimate of attributable risk for all strata
            # combined

            if (total2 == 0 || total4 == 0) {
              ar <- NA
              warn_ind <- TRUE
              temp <- ifelse(total2 == 0, stressor_levels[[ivar_s]][1], stressor_levels[[ivar_s]][2])
              warn <- paste("Since there are no observations for level \"", temp, "\" of the stressor \nvariable, the attributable risk estimate and its standard error cannot be \ncalculated for stratum \"", stratum_levels[i], "\".  Also, the stratum \nwas removed from the analysis.\n", sep = "")
              act <- paste("The attributable risk estimate and its standard error were not calculated for \nstratum \"", stratum_levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep = "")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else if (total1 == 0 && total3 != 0) {
              ar <- 0
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for the cell defined by level \"", response_levels[[ivar_r]][1], "\" \nof the response variable and level \"", stressor_levels[[ivar_s]][1], "\" of the stressor \nvariable, the attributable risk estimate is zero and standard error of the attributable \nrisk estimate cannot be calculated for stratum \"", stratum_levels[i], "\".  \nAlso, the stratum was removed from the analysis.\n", sep = "")
              act <- paste("Standard error of the attributable risk estimate was not calculated for stratum \n\"", stratum_levels[i], "\".  Also, the stratum was removed from the \nanalysis.\n", sep = "")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else if (total1 == 0 && total3 == 0) {
              ar <- NA
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for the cell defined by level \"", response_levels[[ivar_r]][1], "\" \nof the response variable and level \"", stressor_levels[[ivar_s]][1], "\" of the stressor \nvariable and for the cell defined by level \"", response_levels[[ivar_r]][1], "\" of the \nresponse variable and level \"", stressor_levels[[ivar_s]][2], "\" of the stressor variable, \nthe attributable risk estimate and its standard error cannot be calculated for \nstratum \"", stratum_levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep = "")
              act <- paste("The attributable risk estimate and its standard error were not calculated for \nstratum \"", stratum_levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep = "")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else if (total3 == 0) {
              ar <- NA
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for the cell defined by level \"", response_levels[[ivar_r]][1], "\" \nof the response variable and level \"", stressor_levels[[ivar_s]][2], "\" of the stressor \nvariable, the attributable risk estimate and its standard error cannot be \ncalculated for stratum \"", stratum_levels[i], "\".  Also, the stratum \nwas removed from the analysis.\n", sep = "")
              act <- paste("The attributable risk estimate and its standard error were not calculated for \nstratum \"", stratum_levels[i], "\".  Also, the stratum was removed from \nthe analysis.\n", sep = "")
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else {
              theta <- (popsize_hat * total3) / ((total1 + total3) * (total3 +
                total4))
              ar <- 1 - theta
            }

            # Calculate the standard error estimate of the log of attributable
            # risk for all strata combined

            if (total3 == 0 || (total1 + total3) == 0 || (total3 + total4) == 0) {
              arlog_se <- NA
            } else {
              pder <- numeric(4)
              pder[1] <- 1 / popsize_hat - 1 / (total1 + total3)
              pder[2] <- 1 / popsize_hat
              pder[3] <- 1 / popsize_hat + 1 / total3 - 1 / (total1 + total3) -
                1 / (total3 + total4)
              pder[4] <- 1 / popsize_hat - 1 / (total3 + total4)
              pder <- 1 / c(total1, -total2, -total3, total4)
              arlog_se <- sqrt(t(pder) %*% varest %*% pder)
            }

            #
            # End the section for stratified data
            #
          } else {

            #
            # Begin the section for unstratified data
            #

            # Check whether the vector of response values contains a single
            # element

            if (nresp == 1) {
              stop("\nEstimates cannot be calculated since the vector of response values contains a \nsingle element.")
            }

            # Compute the 2x2 table of weight totals

            wgt_total <- addmargins(svytable(
              make.formula(paste(ivar_r, "+", ivar_s)),
              design = subset(design, tst)
            ))

            # Assign cell weight totals

            total1 <- wgt_total[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][1]
            ]
            total2 <- wgt_total[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][1]
            ]
            total3 <- wgt_total[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][2]
            ]
            total4 <- wgt_total[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][2]
            ]

            # Calculate the estimate of attributable risk

            if (total3 == 0 & total1 != 0 & total4 != 0) {
              ar <- 0
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for the cell defined by level \"", response_levels[[ivar_r]][1], "\" \nof the response variable and level \"", stressor_levels[[ivar_s]][2], "\" of the stressor variable, \nthe attributable risk estimate is zero and standard error of the attributable risk \nestimate cannot be calculated.\n", sep = "")
              act <- "Standard error of the attributable risk estimate was not calculated.\n"
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else if ((total1 + total3) == 0) {
              ar <- NA
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for level \"", response_levels[[ivar_r]][1], "\" of the response \nvariable, the attributable risk estimate and its standard error cannot be \ncalculated.\n", sep = "")
              act <- "The attributable risk estimate and its standard error were not calculated.\n"
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else if ((total3 + total4) == 0) {
              ar <- NA
              warn_ind <- TRUE
              warn <- paste("Since there are no observations for level \"", stressor_levels[[ivar_s]][2], "\" of the stressor \nvariable, the attributable risk estimate and its standard error cannot be \ncalculated.\n", sep = "")
              act <- "The attributable risk estimate and its standard error were not calculated.\n"
              warn_df <- rbind(warn_df, data.frame(
                func = I(fname),
                subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = NA, warning = I(warn),
                action = I(act)
              ))
            } else {
              theta <- (popsize_hat * total3) / ((total1 + total3) * (total3 +
                total4))
              ar <- 1 - theta
            }

            # Determine whether the standard error can be calculated

            if (total3 == 0 || (total1 + total3) == 0 || (total3 + total4) == 0) {
              arlog_se <- NA
            } else {

              # Calculate the variance-covariance estimate for the cell totals

              if (cluster_ind) {
                temp <- attrisk_var(
                  response[tst], stressor[tst],
                  response_levels[[ivar_r]], stressor_levels[[ivar_s]],
                  wgt2[tst], xcoord[tst], ycoord[tst], stratum_ind, NULL,
                  cluster_ind, clusterID[tst], wgt1[tst], xcoord1[tst],
                  ycoord1[tst], vartype, warn_ind, warn_df, warn_vec
                )
              } else {
                temp <- attrisk_var(response[tst], stressor[tst],
                  response_levels[[ivar_r]], stressor_levels[[ivar_s]],
                  wgt[tst], xcoord[tst], ycoord[tst], stratum_ind, NULL,
                  cluster_ind,
                  vartype = vartype, warn_ind = warn_ind,
                  warn_df = warn_df, warn_vec = warn_vec
                )
              }
              varest <- temp$varest
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

              # Calculate the standard error estimate of the log of theta

              pder <- numeric(4)
              pder[1] <- 1 / popsize_hat - 1 / (total1 + total3)
              pder[2] <- 1 / popsize_hat
              pder[3] <- 1 / popsize_hat + 1 / total3 - 1 / (total1 + total3) -
                1 / (total3 + total4)
              pder[4] <- 1 / popsize_hat - 1 / (total3 + total4)
              arlog_se <- sqrt(t(pder) %*% varest %*% pder)
            }

            #
            # End section for unstratified data
            #
          }

          # Calculate confidence limits for the estimate of attributable risk

          if (is.na(arlog_se)) {
            cl <- NA
          } else {
            cl <- c(
              1 - exp(log(theta) + arlog_se * mult),
              1 - exp(log(theta) - arlog_se * mult)
            )
          }

          # Calculate the table of cell and margin counts

          cc <- addmargins(table(list(
            response = response[tst],
            stressor = stressor[tst]
          )))

          # Calculate the table of cell proportion estimates

          cp <- wgt_total / wgt_total["Sum", "Sum"]

          # Append results to the arsum data frame

          arsum <- rbind(arsum, data.frame(
            Type = itype,
            Subpopulation = isubpop,
            Response = ivar_r,
            Stressor = ivar_s,
            nResp = cc[3, 3],
            Estimate = ar,
            StdError_log = arlog_se,
            MarginofError_log = mult * arlog_se,
            LCB = cl[1],
            UCB = cl[2],
            WeightTotal = wgt_total["Sum", "Sum"],
            CellCounts_11 = cc[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][1]
            ],
            CellCounts_12 = cc[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][2]
            ],
            CellCounts_21 = cc[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][1]
            ],
            CellCounts_22 = cc[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][2]
            ],
            CellProportions_11 = cp[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][1]
            ],
            CellProportions_12 = cp[
              response_levels[[ivar_r]][1],
              stressor_levels[[ivar_s]][2]
            ],
            CellProportions_21 = cp[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][1]
            ],
            CellProportions_22 = cp[
              response_levels[[ivar_r]][2],
              stressor_levels[[ivar_s]][2]
            ]
          ))

          # End of the loop for levels of the subpopulation
        }

        # End of the loop for stressor variables
      }

      # End of the loop for response variables
    }

    # End of the loop for subpopulations
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

  # Assign dimension names to the arsum data frame

  cell_lbl <- character(4)
  cell_lbl[1] <- "RespPoor_StressPoor"
  cell_lbl[2] <- "RespPoor_StressGood"
  cell_lbl[3] <- "RespGood_StressPoor"
  cell_lbl[4] <- "RespGood_StressGood"
  dimnames(arsum) <- list(1:nrow(arsum), c(
    "Type", "Subpopulation", "Response", "Stressor", "nResp", "Estimate",
    "StdError_log", "MarginofError_log", paste0("LCB", conf, "Pct"),
    paste0("UCB", conf, "Pct"), "WeightTotal", paste0("Count_", cell_lbl[1]),
    paste0("Count_", cell_lbl[2]), paste0("Count_", cell_lbl[3]),
    paste0("Count_", cell_lbl[4]), paste0("Prop_", cell_lbl[1]),
    paste0("Prop_", cell_lbl[2]), paste0("Prop_", cell_lbl[3]),
    paste0("Prop_", cell_lbl[4])
  ))

  # Return the arsum data frame

  arsum
}
