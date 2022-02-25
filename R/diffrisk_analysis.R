################################################################################
# Function: diffrisk_analysis (exported)
# Programmer: Tom Kincaid
# Date: July 12, 2021
# Revised: July 28, 2021 to improve documentation
# Revised: September 9, 2021 to revise the documentation for argument popsize
#
#' Risk difference analysis
#'
#' This function organizes input and output for risk difference analysis (of
#' categorical variables).  The analysis data,
#' \code{dframe}, can be either a data frame or a simple features (\code{sf}) object.  If an
#' \code{sf} object is used, coordinates are extracted from the geometry column in the
#' object, arguments \code{xcoord} and \code{ycoord} are assigned values
#' \code{"xcoord"} and \code{"ycoord"}, respectively, and the geometry column is
#' dropped from the object.
#'
#' @inherit attrisk_analysis params
#'
#' @section Details:
#' Risk difference measures the absolute strength of association between
#' conditional probabilities defined for a response variable and a stressor
#' variable, where the response and stressor variables are classified as either
#' good (i.e., reference condition) or poor (i.e., different from reference
#' condition). Risk difference is defined as the difference between two
#' conditional probabilities: the probability that the response variable is in
#' poor condition given that the stressor variable is in poor condition and the
#' probability that the response variable is in poor condition given that the
#' stressor variable is in good condition.  Risk difference values close to zero
#' indicate that the stressor variable has little or no impact on the
#' probability that the response variable is in poor condition.  Risk difference
#' values much greater than zero indicate that the stressor variable has a
#' significant impact on the probability that the response variable is in poor
#' condition.
#'
#' @return The analysis results. A data frame of population estimates for all combinations of
#'   subpopulations, categories within each subpopulation, response variables,
#'   and categories within each response variable.  Estimates are provided for
#'   proportion and size of the population plus standard error, margin of error,
#'   and confidence interval estimates.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey univar
#'
#' @seealso
#'   \describe{
#'   \item{\code{\link{attrisk_analysis}}}{ for attributable risk analysis}
#'   \item{\code{\link{relrisk_analysis}}}{ for relative risk analysis}
#'   }
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
#' diffrisk_analysis(dframe,
#'   vars_response = myresponse,
#'   vars_stressor = mystressor, subpops = mysubpops, siteID = "siteID",
#'   weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
#'   stratumID = "stratum"
#' )
#' @export
################################################################################

diffrisk_analysis <- function(dframe, vars_response, vars_stressor, response_levels = NULL,
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
  fname <- "diffrisk_analysis"

  # Ensure that the dframe argument was provided

  if (missing(dframe) | is.null(dframe)) {
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
      temp.str <- vecprint(names(temp)[temp > 1])
      warn <- paste("The following site ID values occur more than once among the values that were \ninput to the function:\n", temp.str)
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

  # Create the drsum (results) data frame

  drsum <- NULL

  # Assign the confidence bound multiplier

  mult <- qnorm(0.5 + (conf / 100) / 2)

  # Loop through all subpopulations (domains)

  for (itype in subpops) {
    lev_itype <- levels(dframe[, itype])

    # Loop through all response variables (vars_response)

    for (ivar_r in vars_response) {
      lev_ivar_r <- levels(dframe[, ivar_r])

      # Loop through all stressor variables (vars_stressor)

      for (ivar_s in vars_stressor) {

        # Loop through all levels of the subpopulation

        for (isubpop in lev_itype) {
          tst <- !is.na(dframe[, itype]) & dframe[, itype] == isubpop &
            !is.na(dframe[, ivar_r]) & !is.na(dframe[, ivar_s])

          # Assign response variable values

          response <- dframe[, ivar_r]

          # Assign stressor variable values

          stressor <- dframe[, ivar_s]

          # Calculate the first proportion estimate and variance of that
          # estimate, where the estimate is conditional on the first level of
          # the stressor variable

          ind <- tst & stressor == stressor_levels[[ivar_s]][1]
          temp <- category_est(
            NULL, subset(dframe, ind), itype, isubpop, 1, ivar_r,
            lev_ivar_r, 2, subset(design, ind), design_names, vartype, conf,
            mult, warn_ind, warn_df
          )
          ind <- temp$catsum$Category == response_levels[[ivar_r]][1]
          prop1 <- temp$catsum$Estimate.P[ind] / 100
          var1 <- (temp$catsum$StdError.P[ind] / 100)^2
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

          # Calculate the second proportion estimate and variance of that
          # estimate, where the estimate is conditional on the second level of
          # the stressor variable

          ind <- tst & stressor == stressor_levels[[ivar_s]][2]
          temp <- category_est(
            NULL, subset(dframe, ind), itype, isubpop, 1, ivar_r,
            lev_ivar_r, 2, subset(design, ind), design_names, vartype, conf,
            mult, warn_ind, warn_df
          )
          ind <- temp$catsum$Category == response_levels[[ivar_r]][1]
          prop2 <- temp$catsum$Estimate.P[ind] / 100
          var2 <- (temp$catsum$StdError.P[ind] / 100)^2
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

          # Calculate the difference estimate and standard error of the
          # difference estimate

          diffest <- prop1 - prop2
          stderr_est <- sqrt(var1 + var2)

          # Calculate the table of cell and margin counts

          cc <- addmargins(table(list(
            response = response[tst],
            stressor = stressor[tst]
          )))

          # Calculate the table of cell and margin proportion estimates

          wgt_total <- addmargins(svytable(
            make.formula(paste(ivar_r, "+", ivar_s)),
            design = subset(design, tst)
          ))
          cp <- wgt_total / wgt_total["Sum", "Sum"]

          # Append results to the drsum data frame

          drsum <- rbind(drsum, data.frame(
            Type = itype,
            Subpopulation = isubpop,
            Response = ivar_r,
            Stressor = ivar_s,
            nResp = cc[3, 3],
            Estimate = diffest,
            Estimate_Poor = prop1,
            Estimate_Good = prop2,
            StdError = stderr_est,
            MarginofError = mult * stderr_est,
            LCB = max(diffest - mult * stderr_est, -1),
            UCB = min(diffest + mult * stderr_est, 1),
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

  # Assign dimension names to the drsum data frame

  cell_lbl <- character(4)
  cell_lbl[1] <- "RespPoor_StressPoor"
  cell_lbl[2] <- "RespPoor_StressGood"
  cell_lbl[3] <- "RespGood_StressPoor"
  cell_lbl[4] <- "RespGood_StressGood"
  dimnames(drsum) <- list(1:nrow(drsum), c(
    "Type", "Subpopulation", "Response", "Stressor", "nResp", "Estimate",
    "Estimate_StressPoor", "Estimate_StressGood", "StdError", "MarginofError",
    paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct"), "WeightTotal",
    paste0("Count_", cell_lbl[1]), paste0("Count_", cell_lbl[2]),
    paste0("Count_", cell_lbl[3]), paste0("Count_", cell_lbl[4]),
    paste0("Prop_", cell_lbl[1]), paste0("Prop_", cell_lbl[2]),
    paste0("Prop_", cell_lbl[3]), paste0("Prop_", cell_lbl[4])
  ))

  # Return the drsum data frame

  drsum
}
