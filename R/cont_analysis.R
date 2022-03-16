################################################################################
# Function: cont_analysis (exported)
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
# Revised: June 11, 2021 to allow user control over calculation of CDF
#          estimates, percentile estimates and mean estimates and to use a new
#          function named mean_est for calculation of mean estimates
# Revised: September 9, 2021 to remove argument vartype from the call list for
#          function percentile_est, to remove argument vars_nondetect since
#          currently it is not used, and to revise the documentation for
#          argument popsize
# Revised: October 25 to include the option to calculate total estimates
#
#' Continuous variable analysis
#'
#' This function organizes input and output for the analysis of continuous
#' variables. The analysis data, \code{dframe}, can be either a data frame or a
#' simple features (\code{sf}) object.  If an \code{sf} object is used,
#' coordinates are extracted from the geometry column in the object, arguments
#' \code{xcoord} and \code{ycoord} are assigned values \code{"xcoord"} and
#' \code{"ycoord"}, respectively, and the geometry column is dropped from the
#' object.
#'
#' @inherit cat_analysis params
#'
#' @param pctval  Vector of the set of values at which percentiles are
#'   estimated.  The default set is: \code{c(5, 10, 25, 50, 75, 90, 95)}.
#'
#' @param statistics Character vector specifying desired estimates, where
#'   \code{"CDF"} specifies CDF estimates, \code{"Pct"} specifies percentile
#'   estimates, \code{"Mean"} specifies mean estimates, and "Total" specifies
#'   total estimates.  Any combination of the four choices may be provided by
#'   the user.  The default value is \code{c("CDF", "Pct", "Mean", "Total")}.
#'
#' @return The analysis results. A list composed of one, two, three, or four
#'   data frames that contain population estimates for all combinations of
#'   subpopulations, categories within each subpopulation, and response
#'   variables, where the number of data frames is determined by argument
#'   statistics.  The possible data frames in the output list are:
#'   \describe{
#'     \item{\code{CDF}}{: a data frame containing CDF estimates}
#'     \item{\code{Pct}}{: data frame containing percentile estimates}
#'     \item{\code{Mean}}{: a data frame containing mean estimates}
#'     \item{\code{Total}}{: a data frame containing total estimates}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey univar
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{cat_analysis}}}{ for categorical variable analysis}
#'   }
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
#' cont_analysis(dframe,
#'   vars = myvars, subpops = mysubpops, siteID = "siteID",
#'   weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
#'   stratumID = "stratum", popsize = mypopsize, statistics = "Mean"
#' )
#' @export
################################################################################

cont_analysis <- function(dframe, vars, subpops = NULL, siteID = NULL,
                          weight = "weight", xcoord = NULL, ycoord = NULL,
                          stratumID = NULL, clusterID = NULL, weight1 = NULL,
                          xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE,
                          sweight = NULL, sweight1 = NULL, fpc = NULL,
                          popsize = NULL, vartype = "Local",
                          jointprob = "overton", conf = 95,
                          pctval = c(5, 10, 25, 50, 75, 90, 95),
                          statistics = c("CDF", "Pct", "Mean", "Total"),
                          All_Sites = FALSE) {

  # Assign NULL to vars_nondetect

  vars_nondetect <- NULL

  # Create a vector for error messages

  error_ind <- FALSE
  error_vec <- NULL

  # Create a data frame for warning messages

  warn_ind <- FALSE
  warn_df <- NULL
  fname <- "cont_analysis"

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

  if (is.null(vars)) {
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

  temp <- input_check(dframe, design_names, NULL, vars, NULL, vars_nondetect,
    subpops, sizeweight, fpc, popsize, vartype, jointprob, conf,
    pctval = pctval, error_ind = error_ind, error_vec = error_vec
  )
  dframe <- temp$dframe
  vars <- temp$vars_cont
  vars_nondetect <- temp$vars_nondetect
  subpops <- temp$subpops
  popsize <- temp$popsize
  vartype <- temp$vartype
  jointprob <- temp$jointprob
  error_ind <- temp$error_ind
  error_vec <- temp$error_vec

  # Check argument statistics

  if (!is.vector(statistics)) {
    error_ind <- TRUE
    msg <- "Argument statistics must be a vector\n"
    error_vec <- c(error_vec, msg)
  } else if (!is.character(statistics)) {
    error_ind <- TRUE
    msg <- "Argument statistics must contain character values.\n"
    error_vec <- c(error_vec, msg)
  } else {
    tst <- statistics %in% c("CDF", "Pct", "Mean", "Total")
    if (any(!tst)) {
      error_ind <- TRUE
      msg <- "Argument statistics must contain only the values: 'CDF', 'Pct', 'Mean, and 'Total'.\n"
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
      dframe <- droplevels(dframe)
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

  # Create the contsum (results) list

  contsum <- list(
    CDF = NULL,
    Pct = NULL,
    Mean = NULL,
    Total = NULL
  )

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Loop through all subpopulations (domains)

  for (itype in subpops) {
    lev_itype <- levels(dframe[, itype])
    nlev_itype <- length(lev_itype)

    # Loop through all response variables (vars)

    for (ivar in vars) {
      indx <- match(ivar, vars)

      # Calculate CDF estimates

      if ("CDF" %in% statistics) {
        temp <- cdf_est(
          contsum$CDF, dframe, itype, lev_itype, nlev_itype, ivar, design,
          design_names, vars_nondetect[indx], vartype, conf, mult, warn_ind,
          warn_df
        )
        contsum$CDF <- temp$cdfsum
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      }

      # Calculate percentile estimates

      if ("Pct" %in% statistics) {
        temp <- percentile_est(
          contsum$Pct, dframe, itype, lev_itype, nlev_itype, ivar, design,
          design_names, vars_nondetect[indx], conf, mult, pctval, warn_ind,
          warn_df
        )
        contsum$Pct <- temp$pctsum
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      }

      # Calculate mean estimates

      if ("Mean" %in% statistics) {
        temp <- mean_est(
          contsum$Mean, dframe, itype, lev_itype, nlev_itype, ivar, design,
          design_names, vars_nondetect[indx], vartype, conf, mult, warn_ind,
          warn_df
        )
        contsum$Mean <- temp$meansum
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
      }

      # Calculate total estimates

      if ("Total" %in% statistics) {
        temp <- total_est(
          contsum$Total, dframe, itype, lev_itype, nlev_itype, ivar, design,
          design_names, vars_nondetect[indx], vartype, conf, mult, warn_ind,
          warn_df
        )
        contsum$Total <- temp$totalsum
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
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

  # Assign dimension names to the contsum data frames


  if (!is.null(contsum$CDF)) {
    dimnames(contsum$CDF) <- list(1:nrow(contsum$CDF), c(
      "Type", "Subpopulation", "Indicator", "Value", "nResp", "Estimate.P",
      "StdError.P", "MarginofError.P", paste0("LCB", conf, "Pct.P"),
      paste0("UCB", conf, "Pct.P"), "Estimate.U", "StdError.U", "MarginofError.U",
      paste0("LCB", conf, "Pct.U"), paste0("UCB", conf, "Pct.U")
    ))
    class(contsum$CDF) <- c("sp_CDF", class(contsum$CDF))
  }

  if (!is.null(contsum$Pct)) {
    dimnames(contsum$Pct) <- list(1:nrow(contsum$Pct), c(
      "Type", "Subpopulation", "Indicator", "Statistic", "nResp", "Estimate",
      "StdError", "MarginofError", paste0("LCB", conf, "Pct"),
      paste0("UCB", conf, "Pct")
    ))
  }

  if (!is.null(contsum$Mean)) {
    dimnames(contsum$Mean) <- list(1:nrow(contsum$Mean), c(
      "Type", "Subpopulation", "Indicator", "nResp", "Estimate", "StdError",
      "MarginofError", paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct")
    ))
  }

  if (!is.null(contsum$Total)) {
    dimnames(contsum$Total) <- list(1:nrow(contsum$Total), c(
      "Type", "Subpopulation", "Indicator", "nResp", "Estimate", "StdError",
      "MarginofError", paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct")
    ))
  }

  # Return the contsum object

  contsum
}
