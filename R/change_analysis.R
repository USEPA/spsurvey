################################################################################
# Function: change_analysis (exported)
# Programmer: Tom Kincaid
# Date: July 29, 2020
# Revised: August 14, 2020 to allow use of an sf object as the input data
#          argument (dframe)
# Revised: December 15, 2020 to allow use of the Horvitz-Thompson and
#          Yates-Grundy variance estimators and to use a new function named
#          survey_design to create the survey design object
# Revised: January 28, 2021 to replace "warn.vec" with "warn_vec"
# Revised: March 2, 2021 to revise the process for creating unique site ID
#          values
# Revised: April 7, 2021 to ensure that the dframe argument does not contain
#          zero rows
# Revised: April 19, 2021 to ensure that categorical variables, continuopus
#          variables, and subpopulation variables do not contain only missing
#          values for either of the surveys
# Revised: April 29, 2021 to ensure that the dframe argument only belongs to
#          class "data.frame"
# Revised: May 4, 2021 to avoid warning messages being generated during creation
#          of help files
# Revised: May 6, 2021 to ensure that sf objects do not belong to class tbl_df
# Revised: June 8, 2021 to simplify specification of the values required for
#          calculation of the finite population correction factor and to
#          eliminate use of the finite population correction factor with the
#          local mean variance estimator
#
#' Estimation of Change between Two Probability Surveys
#'
#' This function organizes input and output for estimation of change between two
#' probability surveys.  The input data argument can be either a data frame or a
#' simple features (sf) object.  If an sf object is used, coordinates are
#' extracted from the geometry column in the object, arguments \code{xcoord} and
#' \code{ycoord} are assigned values \code{"xcoord"} and \code{"ycoord"},
#' respectively, and the geometry column is dropped from the object.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param vars_cat Vector composed of character values that identify the
#'   names of categorical response variables in the dframe data frame.  The
#'   default is \code{NULL}.
#'
#' @param vars_cont Vector composed of character values that identify the
#'   names of continuous response variables in the dframe data frame.  The
#'   default is \code{NULL}.
#'
#' @param vars_nondetect Vector composed of character values that identify the
#'   names of logical variables in the \code{dframe} data frame specifying the
#'   presence of not detected (nondetect) values for response variables.  The
#'   order of the values for this argument must match the order of the values
#'   for the \code{vars} argument. Each logical variable specifies the detection
#'   status for the corresponding values of a response variable, where
#'   \code{TRUE} equals not detected, and \code{FALSE} equals detected.  If a
#'   response variable does not include any notdetect values, then the name for
#'   the corresponding logical variable may be set equal to \code{NULL}.  If
#'   this argument equals \code{NULL}, then none of the response variables
#'   contain nondetect values.  The default is \code{NULL}.
#'
#' @param test Character string or character vector providing the location
#'   measure(s) to use for change estimation for continuous variables.  The
#'   choices are \code{"mean"}, \code{"median"}, or \code{c("mean", "median")}.
#'   The default is \code{"mean"}.
#'
#' @param subpops Vector composed of character values that identify the
#'   names of subpopulation (domain) variables in the \code{dframe} data frame.
#'   If a value is not provided, the value \code{"All_Sites"} is assigned to the
#'   subpops argument and a factor variable named \code{"All_Sites"} that takes
#'   the value \code{"All Sites"} is added to the \code{dframe} data frame.  The
#'   default value is \code{NULL}.
#'
#' @param surveyID Character value providing name of the survey ID variable in
#'   the \code{dframe} data frame.  The default value is \code{"surveyID"}.
#'
#' @param survey_names Character vector of length two that provides the survey
#'   names contained in the \code{surveyID} variable in the \code{dframe} data
#'   frame.  The two values in the vector identify the first survey and second
#'   survey, respectively.  If a value is not provided, unique values of the
#'   \code{surveyID} variable are assigned to the \code{survey_names} argument.
#'   The default is \code{NULL}.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the site ID variable
#'   identifies stage two site IDs.  The default value is \code{"siteID"}.
#'
#' @param weight Character value providing name of the survey design weight
#'   variable in the \code{dframe} data frame.  For a two-stage sample, the
#'   weight variable identifies stage two weights.  The default value is
#'   \code{"weight"}.
#'
#' @param revisitwgt Logical value that indicates whether each repeat visit
#'   site has the same survey design weight in the two surveys, where
#'   \code{TRUE} = the weight for each repeat visit site is the same and
#'   \code{FALSE} = the weight for each repeat visit site is not the same.  When
#'   this argument is \code{FALSE}, all of the repeat visit sites are assigned
#'   equal weights when calculating the covariance component of the change
#'   estimate standard error.  The default is \code{FALSE}.
#'
#' @param xcoord Character value providing name of the x-coordinate variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the x-coordinate
#'   variable identifies stage two x-coordinates.  Note that x-coordinates are
#'   required for calculation of the local mean variance estimator.  The default
#'   value is \code{NULL}.
#'
#' @param ycoord Character value providing name of the y-coordinate variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the y-coordinate
#'   variable identifies stage two y-coordinates.  Note that y-coordinates are
#'   required for calculation of the local mean variance estimator.  The default
#'   value is \code{NULL}.
#'
#' @param stratumID Character value providing name of the stratum ID variable in
#'   the \code{dframe} data frame.  The default value is \code{NULL}.
#'
#' @param clusterID Character value providing the name of the cluster
#'   (stage one) ID variable in the \code{dframe} data frame.  Note that cluster
#'   IDs are required for a two-stage sample.  The default value is \code{NULL}.
#'
#' @param weight1 Character value providing name of the stage one weight
#'   variable in the \code{dframe} data frame.  The default value is \code{NULL}.
#'
#' @param xcoord1 Character value providing the name of the stage one
#'   x-coordinate variable in the \code{dframe} data frame.  Note that x
#'   coordinates are required for calculation of the local mean variance
#'   estimator.  The default value is \code{NULL}.
#'
#' @param ycoord1 Character value providing the name of the stage one
#'   y-coordinate variable in the \code{dframe} data frame.  Note that
#'   y-coordinates are required for calculation of the local mean variance
#'   estimator.  The default value is \code{NULL}.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation, where \code{TRUE} = use size weights and
#'   \code{FALSE} = do not use size weights. To employ size weights for a
#'   single-stage sample, a value must be supplied for argument weight.  To
#'   employ size weights for a two-stage sample, values must be supplied for
#'   arguments \code{weight} and \code{weight1}. The default value is \code{FALSE}.
#'
#' @param sweight Character value providing the name of the size weight variable
#'   in the \code{dframe} data frame.  For a two-stage sample, the size weight
#'   variable identifies stage two size weights.  The default value is
#'   \code{NULL}.
#'
#' @param sweight1 Character value providing name of the stage one size weight
#'   variable in the \code{dframe} data frame.  The default value is \code{NULL}.
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
#'   is not used with the local mean variance estimator.\cr\cr
#'   Example fpc for a single-stage unstratified survey design:\cr
#'   \verb{fpc <- 15000}\cr\cr
#'   Example fpc for a single-stage stratified survey design:\cr
#'   \verb{fpc <- list(
#'     Stratum_1 = 9000,
#'     Stratum_2 = 6000)
#'    }\cr
#'   Example fpc for a two-stage unstratified survey design:\cr
#'   \verb{fpc <- c(
#'     Ncluster = 150,
#'     Cluster_1 = 150,
#'     Cluster_2 = 75,
#'     Cluster_3 = 75,
#'     Cluster_4 = 125,
#'     Cluster_5 = 75)
#'   }\cr
#'   Example fpc for a two-stage stratified survey design:\cr
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
#' @param popsize Object that provides values for the population argument of
#'   the \code{calibrate} or \code{postStratify} functions.  For the
#'   \code{calibrate} function, the object is a named list, where the names
#'   identify factor variables in the \code{dframe} data frame.  Each element
#'   of the list is a named vector containing the population total for each
#'   level of the associated factor variable.  For the \code{postStratify}
#'   function, the object is either a data frame, table, or xtabs
#'   object that provides the population total for all combinations of selected
#'   factor varaibles in the \code{dframe} data frame.  If a data frame is used
#'   for \code{popsize}, the variable containing population totals must be the
#'   last variable in the data frame.  If a table is used for \code{popsize},
#'   the table must have named \code{dimnames} where the names identify factor
#'   variables in the \code{dframe} data frame.  If the popsize argument is
#'   equal to \code{NULL}, then neither calibration nor post-stratification is
#'   performed.  The default value is
#'   \code{NULL}.\cr\cr
#'   Example popsize for calibration:\cr
#'   \verb{popsize <- list(
#'     Ecoregion = c(
#'       East = 750,
#'       Central = 500,
#'       West = 250),
#'     Type = c(
#'       Streams = 1150,
#'       Rivers = 350))
#'   }\cr
#'   Example popsize for post-stratification using a data frame:\cr
#'   \verb{popsize <- data.frame(
#'     Ecoregion = rep(c("East", "Central", "West"),
#'       rep(2, 3)),
#'     Type = rep(c("Streams", "Rivers"), 3),
#'     Total = c(575, 175, 400, 100, 175, 75))
#'   }\cr
#'   Example popsize for post-stratification using a table:\cr
#'   \verb{popsize <- with(MySurveyFrame,
#'     table(Ecoregion, Type))}\cr\cr
#'   Example popsize for post-stratification using an xtabs object:\cr
#'   \verb{popsize <- xtabs(~Ecoregion + Type,
#'     data = MySurveyFrame)}
#'
#' @param vartype Character value providing the choice of the variance
#'   estimator, where "Local" = the local mean estimator and \code{"SRS"} = the
#'   simple random sampling estimator.  The default value is \code{"Local"}.
#'
#' @param jointprob Character value providing the choice of joint inclusion
#'   probability approximation for use with Horvitz-Thompson and Yates-Grundy
#'   variance estimators, where \code{"overton"} indicates the Overton
#'   approximation, \code{"hr"} indicates the Hartley_Rao approximation, and
#'   \code{"brewer"} equals the Brewer approximation.  The default value is
#'   \code{"overton"}.
#'
#' @param conf Numeric value providing the confidence level.  The default value
#'   is \code{95}.
#'
#' @return List of change estimates composed of three items:
#'   (1) \code{catsum} contains change estimates for categorical variables,
#'   (2) \code{contsum_mean} contains estimates for continuous variables using
#'   the mean, and (3) \code{contsum_median} contains estimates for continuous
#'   variables using the median.  The items in the list will contain \code{NULL}
#'   for estimates that were not calculated.  Each data frame includes estimates
#'   for all combinations of population Types, subpopulations within types,
#'   response variables, and categories within each response variable (for
#'   categorical variables and continuous variables using the median).  Change
#'   estimates are provided plus standard error estimates and confidence
#'   interval estimates.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{calibrate}}}{conduct calibration for survey data}
#'     \item{\code{change_est}}{estimate change between two surveys}
#'     \item{\code{input_check}}{check input values for errors,
#'       consistency, and compatibility with analytical functions}
#'     \item{\code{\link{postStratify}}}{conduct post-stratification for survey
#'       data}
#'     \item{\code{survey_design}}{creates a survey design object}
#'     \item{\code{uniqueID}}{creates unique site IDs by appending a
#'       unique number to each occurrence of a site ID}
#'     \item{\code{vecprint}}{takes an input vector and outputs a
#'       character string with line breaks inserted}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{calibrate}}
#'   \code{\link{postStratify}}
#'
#' @keywords survey
#'
#' @examples
#' # Categorical variable example for three resource classes
#' dframe <- data.frame(
#'   surveyID = rep(c("Survey 1", "Survey 2"), c(100, 100)),
#'   siteID = paste0("Site", 1:200),
#'   wgt = runif(200, 10, 100),
#'   xcoord = runif(200),
#'   ycoord = runif(200),
#'   stratum = rep(rep(c("Stratum 1", "Stratum 2"), c(2, 2)), 50),
#'   CatVar = rep(c("North", "South"), 100),
#'   All_Sites = rep("All Sites", 200),
#'   Resource_Class = sample(c("Good", "Fair", "Poor"), 200, replace = TRUE)
#' )
#' myvars <- c("CatVar")
#' mysubpops <- c("All_Sites", "Resource_Class")
#' change_analysis(dframe,
#'   vars_cat = myvars, subpops = mysubpops,
#'   surveyID = "surveyID", siteID = "siteID", weight = "wgt",
#'   xcoord = "xcoord", ycoord = "ycoord", stratumID = "stratum"
#' )
#'
#' @export
################################################################################

change_analysis <- function(
  dframe, vars_cat = NULL, vars_cont = NULL, vars_nondetect = NULL,
  test = "mean", subpops = NULL, surveyID = "surveyID", survey_names = NULL,
  siteID = "siteID", weight = "weight", revisitwgt = FALSE, xcoord = NULL,
  ycoord = NULL, stratumID = NULL, clusterID = NULL, weight1 = NULL,
  xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE, sweight = NULL,
  sweight1 = NULL, fpc = NULL, popsize = NULL, vartype = "Local",
  jointprob = "overton", conf = 95) {

  # Create a vector for error messages

  error_ind <- FALSE
  error_vec <- NULL

  # Create a data frame for warning messages

  warn_ind <- FALSE
  warn_df <- NULL
  fname <- "change_analysis"

  # Ensure that the dframe argument was provided

  if (missing(dframe) | is.null(dframe)) {
    stop("\nThe dframe argument must be provided.\n")
  }

  # If the dframe argument is an sf object, extract coordinates from the geometry
  # column, assign values "xcoord" and "ycoord" to arguments xcoord and ycoord,
  # respectively, and drop the geometry column from the object

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

  # Ensure that the dframe data frame contains the survey ID variable

  ind1 <- FALSE
  if (!(surveyID %in% names(dframe))) {
    error_ind <- TRUE
    msg <- paste0("The name provided for the surveyID argument, \"", surveyID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {

    # Ensure that the survey names variable is a vector that has two unique values
    # and that those values match the categories used in the survey ID variable in
    # the dframe data frame

    if (is.null(survey_names)) {
      survey_names <- as.vector(unique(dframe[, surveyID]))
    }
    if (!is.vector(survey_names)) {
      error_ind <- TRUE
      msg <- "The survey_names argument must be a vector.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (is.list(survey_names)) {
        survey_names <- unlist(survey_names)
      }
      if (length(survey_names) != 2) {
        error_ind <- TRUE
        msg <- "The survey names variable should have two unique values.\n"
        error_vec <- c(error_vec, msg)
      } else {
        survey_names <- as.character(survey_names)
        temp <- unique(dframe[, surveyID])
        if (length(temp) != 2) {
          error_ind <- TRUE
          msg <- "The survey ID variable should have two categories.\n"
          error_vec <- c(error_vec, msg)
        } else {
          if (!all(survey_names %in% temp)) {
            error_ind <- TRUE
            msg <- "Values for the survey names variable do not match categories of the survey ID variable.\n"
            error_vec <- c(error_vec, msg)
          } else {
            dframe[, surveyID] <- factor(dframe[, surveyID], levels = survey_names)
            ind1 <- TRUE
          }
        }
      }
    }
  }

  # Create an indicator variable for survey one

  if (ind1) {
    survey_1 <- dframe[, surveyID] %in% survey_names[1]
  }

  # Create an indicator variable for survey two

  if (ind1) {
    survey_2 <- dframe[, surveyID] %in% survey_names[2]
  }

  # Ensure that the dframe data frame contains the site ID variable

  if (!(siteID %in% names(dframe))) {
    ind2 <- FALSE
    error_ind <- TRUE
    msg <- paste0("The name provided for the siteID argument, \"", siteID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {
    ind2 <- TRUE
  }

  # For each survey, check site IDs for repeat values and, as necessary, create
  # unique site IDs and output a warning message

  if (ind1 & ind2) {
    IDs <- dframe[, siteID]
    for (i in 1:2) {
      eval(parse(text = paste0("tst <- survey_", i)))
      temp <- sapply(split(IDs[tst], IDs[tst]), length)
      if (any(temp > 1)) {
        warn_ind <- TRUE
        temp.str <- vecprint(names(temp)[temp > 1])
        warn <- paste("The following site ID values occur more than once among the survey", i, "values \nthat were input to the function:\n", temp.str)
        act <- "Unique site ID values were created.\n"
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname), subpoptype = NA,
          subpop = NA, indicator = NA, stratum = NA, warning = I(warn), action = I(act)
        ))
        dframe[, siteID][tst] <- uniqueID(dframe[, siteID][tst])
      }
    }
  }

  # Determine whether the surveys include repeat visit sites and create indicator
  # variables for repeat visit sites in each survey

  repeat_1 <- logical(nrow(dframe))
  repeat_2 <- logical(nrow(dframe))
  if (ind1 & ind2) {
    repeat_1[survey_1] <- dframe[survey_1, siteID] %in% dframe[
      survey_2,
      siteID
      ]
    repeat_2[survey_2] <- dframe[survey_2, siteID] %in% dframe[
      survey_1,
      siteID
      ]
  }

  # Ensure that repeat visit sites for both surveys occur in the same order among
  # the rows of the dframe data frame

  if (ind1 & ind2) {
    if (any(repeat_1)) {
      indx <- match(dframe[repeat_1, siteID], dframe[repeat_2, siteID])
      dframe[repeat_2, ] <- dframe[repeat_2, ][indx, ]
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
      fpcsize = "fpcsize"
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

  # Ensure that a value was provided for at least one of the vars_cat (categorical
  # response variable names) or vars_cont (continuous response variable names)
  # arguments

  if (is.null(vars_cat) && is.null(vars_cont)) {
    error_ind <- TRUE
    msg <- "A value must be provided for at least one of the vars_cat (categorical \nresponse variable names) or vars_cont (continuous response variable names) \narguments.\n"
    error_vec <- c(error_vec, msg)
  }

  # For each categorical variable, check whether all values for either survey
  # are missing

  if (!is.null(vars_cat) && ind1 && ind2) {
    temp <- NULL
    for(v in vars_cat) {
      if (all(is.na(dframe[survey_1, v])) | all(is.na(dframe[survey_2, v]))) {
        temp <- c(temp, v)
      }
    }
    if (!is.null(temp)) {
      error_ind <- TRUE
      temp.str <- vecprint(temp)
      msg <- paste("For the following categorical variables, at least one of the surveys contains only \nmissing values:\n", temp.str)
      error_vec <- c(error_vec, msg)
    }
  }

  # For each continuous variable, check whether all values for either survey
  # are missing

  if (!is.null(vars_cont) && ind1 && ind2) {
    temp <- NULL
    for(v in vars_cont) {
      if (all(is.na(dframe[survey_1, v])) | all(is.na(dframe[survey_2, v]))) {
        temp <- c(temp, v)
      }
    }
    if (!is.null(temp)) {
      error_ind <- TRUE
      temp.str <- vecprint(temp)
      msg <- paste("For the following continuous variables, at least one of the surveys contains only \nmissing values:\n", temp.str)
      error_vec <- c(error_vec, msg)
    }
  }

  # If a value was not provided for the subpops (subpopulation names) argument,
  # assign the value "All_Sites" to the subpops argument and create a factor
  # named "All_Sites" in the dframe data frame that takes the value "All Sites"

  if (is.null(subpops)) {
    subpops <- "All_Sites"
    dframe$All_Sites <- "All Sites"
    dframe$All_Sites <- factor(dframe$All_Sites)
  }

  # For each subpopulation variable, check whether all values for either survey
  # are missing

  if (ind1 && ind2) {
    temp <- NULL
    for(v in subpops) {
      if (all(is.na(dframe[survey_1, v])) | all(is.na(dframe[survey_2, v]))) {
        temp <- c(temp, v)
      }
    }
    if (!is.null(temp)) {
      error_ind <- TRUE
      temp.str <- vecprint(temp)
      msg <- paste("For the following subpopulation variables, at least one of the surveys contains only \nmissing values:\n", temp.str)
      error_vec <- c(error_vec, msg)
    }
  }

  # Check input arguments
  temp <- input_check(dframe, design_names, vars_cat, vars_cont, NULL, NULL,
    subpops, sizeweight, fpc, popsize, vartype, jointprob, conf,
    error_ind = error_ind, error_vec = error_vec
  )
  dframe <- temp$dframe
  vars_cat <- temp$vars_cat
  vars_cont <- temp$vars_cont
  subpops <- temp$subpops
  popsize <- temp$popsize
  vartype <- temp$vartype
  jointprob <- temp$jointprob
  error_ind <- temp$error_ind
  error_vec <- temp$error_vec

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

  # Create the survey design object for each survey

  design <- survey_design(
    dframe, siteID, weight, stratum_ind, stratumID, cluster_ind, clusterID,
    weight1, sizeweight, sweight, sweight1, fpcfactor_ind, fpcsize, Ncluster,
    stage1size, vartype, jointprob
  )
  design_1 <- subset(design, survey_1)
  design_2 <- subset(design, survey_2)

  # If popsize is not equal to NULL, then call either the postStratify or
  # calibrate function, as appropriate

  if (!is.null(popsize)) {
    if (all(class(popsize) %in% c("data.frame", "table", "xtabs"))) {
      if ("data.frame" %in% class(popsize)) {
        pnames <- names(popsize)[-ncol(popsize)]
      } else {
        pnames <- names(dimnames(popsize))
      }
      design_1 <- postStratify(design_1, make.formula(pnames), popsize)
      design_2 <- postStratify(design_2, make.formula(pnames), popsize)
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
      design_1 <- calibrate(design_1, make.formula(cnames), pop_totals)
      design_2 <- calibrate(design_2, make.formula(cnames), pop_totals)
    }
  }

  # If popsize is not equal to NULL and vartype equals "Local", then assign
  # adjusted weights to the appropriate weight variable(s) in the design data
  # frames

  if (!is.null(popsize) && vartype == "Local") {
    if (cluster_ind) {
      design_1$variables$wgt2 <- weights(design_1) / design_1$variables$wgt1
      design_2$variables$wgt2 <- weights(design_2) / design_1$variables$wgt1
    } else {
      design_1$variables$wgt <- weights(design_1)
      design_2$variables$wgt <- weights(design_2)
    }
  }

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Create the output object

  changesum <- list(catsum = NULL, contsum_mean = NULL, contsum_median = NULL)

  #
  # Begin the section for categorical response variables
  #

  if (!is.null(vars_cat)) {

    # Loop through all subpopulations (domains)

    for (itype in subpops) {
      lev_itype <- levels(dframe[, itype])

      # Loop through all response variables

      for (ivar in vars_cat) {
        lev_ivar <- levels(dframe[, ivar])
        nlev_ivar <- length(lev_ivar)

        # Loop through all levels of a subpopulation

        for (isubpop in lev_itype) {

          # Calculate change estimates, standard error estimates, and confidence bound
          # estimates for each combination of subpopulation, response variable, and level
          # of the subpopulation

          temp <- change_est(
            resp_ind = "cat", survey_names, changesum,
            dframe, survey_1, survey_2, itype, isubpop, ivar, lev_ivar,
            nlev_ivar, design_1, design_2, design_names, repeat_1[survey_1],
            repeat_2[survey_2], siteID, revisitwgt, NULL, NULL, vartype, conf,
            mult, warn_ind, warn_df, warn_vec = c(itype, isubpop, ivar)
          )
          changesum <- temp$changesum
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

          # End of the loop for subpopulations
        }

        # End of the loop for response variables
      }

      # End of the loop for type of population
    }

    # End of the section for categorical response variables
  }

  #
  # Begin the section for continuous response variables
  #

  if (!is.null(vars_cont)) {

    # Loop through all subpopulations

    for (itype in subpops) {
      lev_itype <- levels(dframe[, itype])

      # Loop through all response variables

      for (ivar in vars_cont) {

        # Loop through all levels of a subpopulation

        for (isubpop in lev_itype) {

          # Calculate change estimates, standard error estimates, and confidence bound
          # estimates for each combination of subpopulation, response variable, and level
          # of the subpopulation

          indx <- match(ivar, vars_cont)
          temp <- change_est(
            resp_ind = "cont", survey_names, changesum,
            dframe, survey_1, survey_2, itype, isubpop, ivar, NULL, NULL,
            design_1, design_2, design_names, repeat_1[survey_1],
            repeat_2[survey_2], siteID, revisitwgt, test, vars_nondetect[indx],
            vartype, conf, mult, warn_ind, warn_df,
            warn_vec = c(itype, isubpop, ivar)
          )
          changesum <- temp$changesum
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df
          changesum <- temp$changesum
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

          # End of the loop for subpopulations
        }

        # End of the loop for response variables
      }

      # End of the loop for type of population
    }

    # End of the section for continuous response variables
  }

  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if (warn_ind) {
    warn_df <<- warn_df
    if (nrow(warn_df) == 1) {
      cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn_df'.  Enter the following command \nto view the warning message: warnprnt()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn_df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn_df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # As necessary, assign dimension names to the catsum, contsum_mean, and
  # contsum_median data frames

  if (!is.null(changesum$catsum)) {
    dimnames(changesum$catsum) <- list(1:nrow(changesum$catsum), c(
      "Survey_1",
      "Survey_2", "Type", "Subpopulation", "Indicator", "Category", "DiffEst.P",
      "StdError.P", "MarginofError.P", paste0("LCB", conf, "Pct.P"),
      paste0("UCB", conf, "Pct.P"), "DiffEst.U", "StdError.U",
      "MarginofError.U", paste0("LCB", conf, "Pct.U"),
      paste0("UCB", conf, "Pct.U"), "nResp_1", "Estimate.P_1", "StdError.P_1",
      "MarginofError.P_1", paste0("LCB", conf, "Pct.P_1"),
      paste0("UCB", conf, "Pct.P_1"), "Estimate.U_1", "StdError.U_1",
      "MarginofError.U_1", paste0("LCB", conf, "Pct.U_1"),
      paste0("UCB", conf, "Pct.U_1"), "nResp_2", "Estimate.P_2", "StdError.P_2",
      "MarginofError.P_2", paste0("LCB", conf, "Pct.P_2"),
      paste0("UCB", conf, "Pct.P_2"), "Estimate.U_2", "StdError.U_2",
      "MarginofError.U_2", paste0("LCB", conf, "Pct.U_2"),
      paste0("UCB", conf, "Pct.U_2")
    ))
  }

  if (!is.null(changesum$contsum_mean)) {
    dimnames(changesum$contsum_mean) <- list(
      1:nrow(changesum$contsum_mean),
      c(
        "Survey_1", "Survey_2", "Type", "Subpopulation", "Indicator",
        "Statistic", "DiffEst", "StdError", "MarginofError",
        paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct"), "nResp_1",
        "Estimate_1", "StdError_1", "MarginofError_1",
        paste0("LCB", conf, "Pct_1"), paste0("UCB", conf, "Pct_1"), "nResp_2",
        "Estimate_2", "StdError_2", "MarginofError_2",
        paste0("LCB", conf, "Pct_2"), paste0("UCB", conf, "Pct_2")
      )
    )
  }

  if (!is.null(changesum$contsum_median)) {
    dimnames(changesum$contsum_median) <- list(
      1:nrow(changesum$contsum_median),
      c(
        "Survey_1", "Survey_2", "Type", "Subpopulation", "Indicator",
        "Category", "DiffEst.P", "StdError.P", "MarginofError.P",
        paste0("LCB", conf, "Pct.P"), paste0("UCB", conf, "Pct.P"), "DiffEst.U",
        "StdError.U", "MarginofError.U", paste0("LCB", conf, "Pct.U"),
        paste0("UCB", conf, "Pct.U"), "nResp_1", "Estimate.P_1", "StdError.P_1",
        "MarginofError.P_1", paste0("LCB", conf, "Pct.P_1"),
        paste0("UCB", conf, "Pct.P_1"), "Estimate.U_1", "StdError.U_1",
        "MarginofError.U_1", paste0("LCB", conf, "Pct.U_1"),
        paste0("UCB", conf, "Pct.U_1"), "nResp_2", "Estimate.P_2",
        "StdError.P_2", "MarginofError.P_2", paste0("LCB", conf, "Pct.P_2"),
        paste0("UCB", conf, "Pct.P_2"), "Estimate.U_2", "StdError.U_2",
        "MarginofError.U_2", paste0("LCB", conf, "Pct.U_2"),
        paste0("UCB", conf, "Pct.U_2")
      )
    )
  }

  # Return the output object

  changesum
}
