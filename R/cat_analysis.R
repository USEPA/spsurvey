################################################################################
# Function: cat_analysis (exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: August 14, 2020 to allow use of an sf object as the input data
#          argument (dframe)
# Revised: December 15, 2020 to allow use of the Horvitz-Thompson and
#          Yates-Grundy variance estimators and to use a new function named
#          survey_design to create the survey design object
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
# Revised: September 9, 2021 to revise the documentation for argument popsize
#
#' Categorical variable analysis
#'
#' This function organizes input and output for the analysis of categorical variables.  The analysis data,
#' \code{dframe}, can be either a data frame or a simple features (\code{sf}) object.  If an
#' \code{sf} object is used, coordinates are extracted from the geometry column in the
#' object, arguments \code{xcoord} and \code{ycoord} are assigned values
#' \code{"xcoord"} and \code{"ycoord"}, respectively, and the geometry column is
#' dropped from the object.
#'
#' @param dframe Data to be analyzed (analysis data). A data frame or
#'   \code{sf} object containing survey design
#'   variables, response variables, and subpopulation (domain) variables.
#'
#' @param vars Vector composed of character values that identify the
#'   names of response variables in \code{dframe}.
#'
#' @param subpops Vector composed of character values that identify the
#'   names of subpopulation (domain) variables in \code{dframe}.
#'   If a value is not provided, the value \code{"All_Sites"} is assigned to the
#'   subpops argument and a factor variable named \code{"All_Sites"} that takes
#'   the value \code{"All Sites"} is added to the \code{dframe} data frame.  The
#'   default value is \code{NULL}.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the site ID variable
#'   identifies stage two site IDs.  The default value is \code{"siteID"}.
#'
#' @param weight Character value providing name of the design weight
#'   variable in \code{dframe}.  For a two-stage sample, the
#'   weight variable identifies stage two weights.  The default value is
#'   \code{"weight"}.
#'
#' @param xcoord Character value providing name of the x-coordinate variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the x-coordinate
#'   variable identifies stage two x-coordinates.  Note that x-coordinates are
#'   required for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the x-coordinate). The default
#'   value is \code{NULL}.
#'
#' @param ycoord Character value providing name of the y-coordinate variable in
#'   the \code{dframe} data frame.  For a two-stage sample, the y-coordinate
#'   variable identifies stage two y-coordinates.  Note that y-coordinates are
#'   required for calculation of the local mean variance estimator.  If \code{dframe}
#'   is an \code{sf} object, this argument is not required (as the geometry column
#'   in \code{dframe} is used to find the y-coordinate). The default
#'   value is \code{NULL}.
#'
#' @param stratumID Character value providing name of the stratum ID variable in
#'   the \code{dframe} data frame.  The default value is \code{NULL}.
#'
#' @param clusterID Character value providing the name of the cluster
#'   (stage one) ID variable in \code{dframe}.  Note that cluster
#'   IDs are required for a two-stage sample.  The default value is \code{NULL}.
#'
#' @param weight1 Character value providing name of the stage one weight
#'   variable in \code{dframe}.  The default value is \code{NULL}.
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
#'   arguments \code{weight} and \code{weight1}. The default value is \code{FALSE}.
#'
#' @param sweight Character value providing the name of the size weight variable
#'   in \code{dframe}.  For a two-stage sample, the size weight
#'   variable identifies stage two size weights.  The default value is
#'   \code{NULL}.
#'
#' @param sweight1 Character value providing name of the stage one size weight
#'   variable in \code{dframe}.  The default value is \code{NULL}.
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
#'   estimator, where \code{"Local"} indicates the local mean estimator,
#'   \code{"SRS"} indicates the simple random sampling estimator, \code{"HT"}
#'   indicates the Horvitz-Thompson estimator, and \code{"YG"} indicates the
#'   Yates-Grundy estimator.  The default value is \code{"Local"}.
#'
#' @param jointprob Character value providing the choice of joint inclusion
#'   probability approximation for use with Horvitz-Thompson and Yates-Grundy
#'   variance estimators, where \code{"overton"} indicates the Overton
#'   approximation, \code{"hr"} indicates the Hartley-Rao approximation, and
#'   \code{"brewer"} equals the Brewer approximation.  The default value is
#'   \code{"overton"}.
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
#' @return The analysis results. A data frame of population estimates for all combinations of
#'   subpopulations, categories within each subpopulation, response variables,
#'   and categories within each response variable.  Estimates are provided for
#'   proportion and total of the population plus standard error, margin of
#'   error, and confidence interval estimates.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey univar
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{cont_analysis}}}{ for continuous variable analysis}
#'   }
#'
#' @examples
#' dframe <- data.frame(
#'   siteID = paste0("Site", 1:100),
#'   wgt = runif(100, 10, 100),
#'   xcoord = runif(100),
#'   ycoord = runif(100),
#'   stratum = rep(c("Stratum1", "Stratum2"), 50),
#'   CatVar = rep(c("north", "south", "east", "west"), 25),
#'   All_Sites = rep("All Sites", 100),
#'   Resource_Class = rep(c("Good", "Poor"), c(55, 45))
#' )
#' myvars <- c("CatVar")
#' mysubpops <- c("All_Sites", "Resource_Class")
#' mypopsize <- data.frame(
#'   Resource_Class = c("Good", "Poor"),
#'   Total = c(4000, 1500)
#' )
#' cat_analysis(dframe,
#'   vars = myvars, subpops = mysubpops, siteID = "siteID",
#'   weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
#'   stratumID = "stratum", popsize = mypopsize
#' )
#' @export
################################################################################

cat_analysis <- function(dframe, vars, subpops = NULL, siteID = "siteID", weight = "weight",
                         xcoord = NULL, ycoord = NULL, stratumID = NULL, clusterID = NULL,
                         weight1 = NULL, xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE,
                         sweight = NULL, sweight1 = NULL, fpc = NULL, popsize = NULL,
                         vartype = "Local", jointprob = "overton", conf = 95, All_Sites = FALSE) {

  # Create a vector for error messages

  error_ind <- FALSE
  error_vec <- NULL

  # Create a data frame for warning messages

  warn_ind <- FALSE
  warn_df <- NULL
  fname <- "cat_analysis"

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

  # Ensure that the dframe data frame contains the site ID variable

  if (!(siteID %in% names(dframe))) {
    ind <- FALSE
    error_ind <- TRUE
    msg <- paste0("The name provided for the siteID argument, \"", siteID, "\", does not occur among \nthe names for the dframe data frame.\n")
    error_vec <- c(error_vec, msg)
  } else {
    ind <- TRUE
  }

  # Check site IDs for repeat values and, as necessary, create unique site IDs and
  # output a warning message

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

  # Ensure that a value was provided for the vars (response variable names)
  # argument

  if (missing(vars)) {
    error_ind <- TRUE
    msg <- "A value must be provided for the vars (response variable names) argument.\n"
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
  temp <- input_check(dframe, design_names, vars, NULL, NULL, NULL, subpops,
    sizeweight, fpc, popsize, vartype, jointprob, conf,
    error_ind = error_ind,
    error_vec = error_vec
  )
  dframe <- temp$dframe
  vars <- temp$vars_cat
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
      design <- calibrate(design, make.formula(cnames), pop_totals)
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

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Create the catsum (results) data frame

  catsum <- NULL

  # Loop through all subpopulations (domains)

  for (itype in subpops) {
    lev_itype <- levels(dframe[, itype])
    nlev_itype <- length(lev_itype)

    # Loop through all response variables (vars)

    for (ivar in vars) {
      lev_ivar <- levels(dframe[, ivar])
      nlev_ivar <- length(lev_ivar)

      # Calculate estimates

      temp <- category_est(
        catsum, dframe, itype, lev_itype, nlev_itype, ivar, lev_ivar, nlev_ivar,
        design, design_names, vartype, conf, mult, warn_ind, warn_df
      )
      catsum <- temp$catsum
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df

      # End of the loop for response variables
    }

    # End of the loop for subpopulations
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

  # Assign dimension names to the catsum data frame

  dimnames(catsum) <- list(1:nrow(catsum), c(
    "Type", "Subpopulation",
    "Indicator", "Category", "nResp", "Estimate.P", "StdError.P",
    "MarginofError.P", paste0("LCB", conf, "Pct.P"),
    paste0("UCB", conf, "Pct.P"), "Estimate.U", "StdError.U",
    "MarginofError.U", paste0("LCB", conf, "Pct.U"),
    paste0("UCB", conf, "Pct.U")
  ))

  # Return the catsum data frame

  catsum
}
