################################################################################
# Function: input_check (not exported)
# Programmer: Tom Kincaid
# Date: October 9, 2020
# Revised: November 5, 2020 to correct an error when checking size-weights for
#          two-stage samples
# Revised: December 17, 2020 to check input values for the Horvitz-Thompson and
#          Yates-Grundy options for variance estimator
# Revised: June 4, 2021 to implement checking the revised approach used for
#          specification of the finite population correction factor and to
#          ensure that continuous variables have syntactically valid names
# Revised: July 14, 2021 to ensure that character variables are processed as
#          factors
#
#' Check Input Values for Analytical Functions
#'
#' This function checks input values for errors, consistency, and compatibility
#' with analytical functions.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param design_names Vector composed of character values that identify the
#'   names of the following survey design variables in the \code{dframe} data
#'   frame: \code{siteID}, \code{weight}, \code{xcoord}, \code{ycoord},
#'   \code{stratumID}, \code{clusterID}, \code{weight1}, \code{xcoord1},
#'   \code{ycoord1}, \code{sweight}, \code{sweight1}, \code{fpcsize},
#'   \code{Ncluster}, \code{stage1size}.
#'
#' @param vars_cat Vector composed of character values that identify the names
#'   of categorical response variables in the \code{dframe} data frame.
#'
#' @param vars_cont Vector composed of character values that identify the names
#'   of continuous response variables in the \code{dframe} data frame.
#'
#' @param vars_stressor Vector composed of character values that identify the
#'   names of stressor variables in the \code{dframe} data frame.
#'
#' @param vars_nondetect Vector composed of character values that identify the
#'   names of logical variables in the \code{dframe} data frame specifying the
#'   presence of not detected (nondetect) values for response variables.
#'
#' @param subpops Vector composed of character values that identify the names of
#'   subpopulation variables in the \code{dframe} data frame.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation.
#'
#' @param fpc Object that specifies values required for calculation of the
#'   finite population correction factor used during variance estimation.
#'
#' @param popsize Object providing known size of the resource.
#'
#' @param vartype Character value providing choice of the variance estimator,
#'   where \code{"Local"} = the local mean estimator, \code{"SRS"} = the simple
#'   random sampling estimator, \code{"HT"} = the Horvitz-Thompson estimator,
#'   and \code{"YG"} = the Yates-Grundy estimator.
#'
#' @param jointprob Character value providing choice of joint inclusion
#'   probability approximation for use with Horvitz-Thompson and Yates-Grundy
#'   variance estimators, where \code{"overton"} indicates the Overton
#'   approximation, \code{"hr"} indicates the Hartley_Rao approximation, and
#'   \code{"brewer"} equals the Brewer approximation.
#'
#' @param conf Numeric value providing the confidence level.
#'
#' @param cdfval Vector of the set of values at which the CDF is estimated.
#'
#' @param pctval Vector of the set of values at which percentiles are estimated.
#'
#' @param error_ind Logical value that indicates whether error messages were
#'   generated.
#'
#' @param error_vec Vector for storing error messages.
#'
#' @return A list consisting of \code{dframe}, \code{vars_cat},
#'   \code{vars_cont}, \code{vars_stressor}, \code{subpops}, \code{popsize},
#'   \code{vartype}, \code{jointprob}, \code{error_ind}, and \code{error_vec}.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{make.names}}}{creates syntactically valid names}
#'     \item{\code{vecprint}}{takes an input vector and outputs a character
#'       string with line breaks inserted}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @noRd
################################################################################

input_check <- function(dframe, design_names, vars_cat, vars_cont,
                        vars_stressor, vars_nondetect, subpops, sizeweight,
                        fpc, popsize, vartype, jointprob, conf, cdfval = NULL,
                        pctval = NULL, error_ind, error_vec) {

  # Ensure that character variables are processed as factors

  # options(stringsAsFactors = TRUE)
  dframe <- as.data.frame(unclass(dframe), stringsAsFactors = TRUE)

  # For variables that exist in the dframe data frame, assign survey design
  # variables and check those variables for missing values

  temp <- c("surveyID", "siteID", "weight")
  design_names <- design_names[!(names(design_names) %in% c("fpcsize",
                                                            "Ncluster", "stage1size"))]
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      if (!(design_names[[i]] %in% names(dframe))) {
        if (!(i %in% temp)) {
          error_ind <- TRUE
          msg <- paste0("The name provided for the ", i, " argument, \"", design_names[[i]], "\", does not occur among \nthe names for the dframe data frame.\n")
          error_vec <- c(error_vec, msg)
        }
        eval(parse(text = paste0(i, " <- NULL")))
      } else {
        eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
        if (any(is.na(dframe[, design_names[[i]]]))) {
          error_ind <- TRUE
          msg <- paste0("Missing values are not allowed for the ", design_names[[i]], " variable in the dframe data \nframe.\n")
          error_vec <- c(error_vec, msg)
          eval(parse(text = paste0(i, " <- NULL")))
        }
      }
    }
  }

  # If the sample is stratified, code stratumID as a factor, determine stratum
  # levels, and calculate number of strata

  if (!is.null(stratumID)) {
    stratumID <- factor(stratumID)
    stratum_levels <- levels(stratumID)
    nstrata <- length(stratum_levels)
  }

  # Check the response variables arguments to ensure that the arguments are
  # vectors composed of character values and that the elements of the vectors
  # are included among the names in the dframe data frame.  For categorical
  # response variables, ensure that the variables referenced in the argument are
  # coded as factors.  For continuous response variables, ensure that the
  # variables referenced in the argument are coded as numeric variables and that
  # variable names are syntactically valid

  if (!is.null(vars_cat)) {
    if (!is.vector(vars_cat)) {
      error_ind <- TRUE
      msg <- "The vars_cat argument must be a vector.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (is.list(vars_cat)) {
        vars_cat <- unlist(vars_cat)
      }
      if (!all(is.character(vars_cat))) {
        vars_cat <- sapply(vars_cat, as.character)
      }
      tst <- !(vars_cat %in% names(dframe))
      if (any(tst)) {
        error_ind <- TRUE
        temp_str <- vecprint(vars_cat[tst])
        msg <- paste0("The following categorical response variable names do not occur among the \nnames for the dframe data frame:\n", temp_str)
        error_vec <- c(error_vec, msg)
      } else {
        for (i in vars_cat) {
          if (!is.factor(dframe[, i])) {
            dframe[, i] <- factor(dframe[, i])
          }
        }
      }
    }
  }

  if (!is.null(vars_cont)) {
    if (!is.vector(vars_cont)) {
      error_ind <- TRUE
      msg <- "The vars_cont argument must be a vector.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (is.list(vars_cont)) {
        vars_cont <- unlist(vars_cont)
      }
      tst <- !(vars_cont %in% names(dframe))
      if (any(tst)) {
        error_ind <- TRUE
        temp_str <- vecprint(vars_cont[tst])
        msg <- paste0("The following continuous response variable names do not occur among the \nnames for the dframe data frame:\n", temp_str)
        error_vec <- c(error_vec, msg)
      } else {
        for (i in vars_cont) {
          if (!is.numeric(dframe[, i])) {
            dframe[, i] <- as.numeric(dframe[, i])
          }
        }
        indx <- match(vars_cont, names(dframe))
        vars_cont <- make.names(vars_cont)
        names(dframe)[indx] <- vars_cont
      }
    }
  }

  # Check the stressor variables argument to ensure that the argument is a
  # vector composed of character values,that elements of the vector are included
  # among the names in the dframe data frame, and that the variables referenced
  # in the vector are coded as factors.

  if (!is.null(vars_stressor)) {
    if (!is.vector(vars_stressor)) {
      error_ind <- TRUE
      msg <- "The vars_stressor argument must be a vector.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (is.list(vars_stressor)) {
        vars_stressor <- unlist(vars_stressor)
      }
      if (!all(is.character(vars_stressor))) {
        vars_stressor <- sapply(vars_stressor, as.character)
      }
      tst <- !(vars_stressor %in% names(dframe))
      if (any(tst)) {
        error_ind <- TRUE
        temp_str <- vecprint(vars_stressor[tst])
        msg <- paste0("The following stressor[ variable names do not occur among the names for the \ndframe data frame:\n", temp_str)
        error_vec <- c(error_vec, msg)
      } else {
        for (i in vars_stressor) {
          if (!is.factor(dframe[, i])) {
            dframe[, i] <- factor(dframe[, i])
          }
        }
      }
    }
  }

  # Check the vars_nondetect logical variables argument to ensure that the
  # argument is a vector composed of character values, that elements of the
  # vector are included among the names in the dframe data frame, and that the
  # variables referenced in the vector are coded as logical variables.

  if (!is.null(vars_nondetect)) {
    if (!is.vector(vars_nondetect)) {
      error_ind <- TRUE
      msg <- "The vars_nondetect argument must be a vector.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (is.list(vars_nondetect)) {
        vars_nondetect <- unlist(vars_nondetect)
      }
      if (!all(is.character(vars_nondetect))) {
        vars_nondetect <- sapply(vars_nondetect, as.character)
      }
      tst <- !(vars_nondetect %in% names(dframe))
      if (any(tst)) {
        error_ind <- TRUE
        temp_str <- vecprint(vars_nondetect[tst])
        msg <- paste0("The following response variable names do not occur among the names for the \ndframe data frame:\n", temp_str)
        error_vec <- c(error_vec, msg)
      } else {
        for (i in vars_nondetect) {
          if (!is.logical(dframe[, i])) {
            dframe[, i] <- as.logical(dframe[, i])
          }
        }
      }
    }
  }

  # Check the subpopulation (domain) names argument to ensure that the argument
  # is a vector composed of character values, that the elements of the vector
  # are included among the names in the dframe data frame, and that all response
  # variables referenced in the vector are coded as factors

  if (is.list(subpops)) {
    subpops <- unlist(subpops)
  }
  if (!is.vector(subpops)) {
    subpops <- as.vector(subpops)
  }
  if (!all(is.character(subpops))) {
    subpops <- as.character(subpops)
  }
  tst <- !(subpops %in% names(dframe))
  if (any(tst)) {
    error_ind <- TRUE
    temp_str <- vecprint(subpops[tst])
    msg <- paste0("The following subpopulation (domain) names do not occur among the names for the \ndframe data frame:\n", temp_str)
    error_vec <- c(error_vec, msg)
  } else {
    for (i in subpops) {
      if (!is.factor(dframe[, i])) {
        dframe[, i] <- factor(dframe[, i])
      }
    }
  }

  # Check weight arguments

  if (!is.null(clusterID)) {
    if (!is.null(weight)) {
      if (min(weight, na.rm = TRUE) <= 0) {
        error_ind <- TRUE
        msg <- "Stage two weights must be positive.\n"
        error_vec <- c(error_vec, msg)
      }
    }
    if (sizeweight) {
      if (is.null(sweight)) {
        error_ind <- TRUE
        msg <- "Argument sweight was not supplied.\n"
        error_vec <- c(error_vec, msg)
      } else {
        if (min(sweight, na.rm = TRUE) <= 0) {
          error_ind <- TRUE
          msg <- "Stage two size-weights must be positive.\n"
          error_vec <- c(error_vec, msg)
        }
      }
    }
    if (is.null(weight1)) {
      error_ind <- TRUE
      msg <- "Argument weight1 was not supplied.\n"
      error_vec <- c(error_vec, msg)
    } else {
      if (min(weight1, na.rm = TRUE) <= 0) {
        error_ind <- TRUE
        msg <- "Stage one weights must be positive.\n"
        error_vec <- c(error_vec, msg)
      }
      if (!is.null(stratumID)) {
        temp_weight1 <- split(weight1, stratumID)
        for (i in 1:nstrata) {
          tst <- stratumID == stratum_levels[i]
          temp_clusterID <- clusterID[tst]
          if (any(sapply(
            tapply(temp_weight1[[i]], temp_clusterID, unique),
            length
          ) > 1)) {
            error_ind <- TRUE
            msg <- paste0("\nThe stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum_levels[i], ".\n")
            error_vec <- c(error_vec, msg)
          }
        }
      } else {
        if (any(sapply(tapply(weight1, clusterID, unique), length) > 1)) {
          error_ind <- TRUE
          msg <- "The stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
          error_vec <- c(error_vec, msg)
        }
      }
    }
    if (sizeweight) {
      if (is.null(sweight1)) {
        error_ind <- TRUE
        msg <- "Argument sweight1 was not supplied.\n"
        error_vec <- c(error_vec, msg)
      } else {
        if (min(sweight1, na.rm = TRUE) <= 0) {
          error_ind <- TRUE
          msg <- "Stage one size-weights must be positive.\n"
          error_vec <- c(error_vec, msg)
        }
        if (!is.null(stratumID)) {
          temp.sweight1 <- split(sweight1, stratumID)
          for (i in 1:nstrata) {
            tst <- stratumID == stratum_levels[i]
            temp_clusterID <- clusterID[tst]
            if (any(sapply(
              tapply(temp.sweight1[[i]], temp_clusterID, unique),
              length
            ) > 1)) {
              error_ind <- TRUE
              msg <- paste0("\nThe stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit of stratum ", stratum_levels[i], ".\n")
              error_vec <- c(error_vec, msg)
            }
          }
        } else {
          if (any(sapply(tapply(sweight1, clusterID, unique), length) > 1)) {
            error_ind <- TRUE
            msg <- "The stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit.\n"
            error_vec <- c(error_vec, msg)
          }
        }
      }
    }
  } else {
    if (!is.null(weight)) {
      if (min(weight, na.rm = TRUE) <= 0) {
        error_ind <- TRUE
        msg <- "Weights must be positive.\n"
        error_vec <- c(error_vec, msg)
      }
    }
    if (sizeweight) {
      if (is.null(sweight)) {
        error_ind <- TRUE
        msg <- "Argument sweight was not supplied.\n"
        error_vec <- c(error_vec, msg)
      } else {
        if (min(sweight, na.rm = TRUE) <= 0) {
          error_ind <- TRUE
          msg <- "Size-weights must be positive.\n"
          error_vec <- c(error_vec, msg)
        }
      }
    }
  }

  # Check coordinate arguments

  if (vartype == "Local") {
    if (!is.null(clusterID)) {
      if (is.null(xcoord) || is.null(ycoord)) {
        error_ind <- TRUE
        msg <- "Stage two x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.\n"
        error_vec <- c(error_vec, msg)
      }
      if (is.null(xcoord1) || is.null(ycoord1)) {
        error_ind <- TRUE
        msg <- "Stage one x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.\n"
        error_vec <- c(error_vec, msg)
      } else {
        if (!is.null(stratumID)) {
          temp_xcoord1 <- split(xcoord1, stratumID)
          temp_ycoord1 <- split(ycoord1, stratumID)
          for (i in 1:nstrata) {
            tst <- stratumID == stratum_levels[i]
            temp_clusterID <- clusterID[tst]
            if (any(sapply(
              tapply(temp_xcoord1[[i]], temp_clusterID, unique),
              length
            ) > 1)) {
              error_ind <- TRUE
              msg <- paste0("\nThe stage one x-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum_levels[i], ".\n")
              error_vec <- c(error_vec, msg)
            }
            if (any(sapply(
              tapply(temp_ycoord1[[i]], temp_clusterID, unique),
              length
            ) > 1)) {
              error_ind <- TRUE
              msg <- paste0("\nThe stage one y-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum_levels[i], ".\n")
              error_vec <- c(error_vec, msg)
            }
          }
        } else {
          if (any(sapply(tapply(xcoord1, clusterID, unique), length) > 1)) {
            error_ind <- TRUE
            msg <- "The stage one x-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
            error_vec <- c(error_vec, msg)
          }
          if (any(sapply(tapply(ycoord1, clusterID, unique), length) > 1)) {
            error_ind <- TRUE
            msg <- "The stage one y-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
            error_vec <- c(error_vec, msg)
          }
        }
      }
    } else {
      if (is.null(xcoord) || is.null(ycoord)) {
        error_ind <- TRUE
        msg <- "x-coordinates and y-coordinates for location are required for the local mean \nvariance estimator.\n"
        error_vec <- c(error_vec, msg)
      }
    }
  }

  # Check the known size of the resource argument

  if (!is.null(popsize)) {
    cls <- class(popsize)[1]
    if (cls %in% c("data.frame", "table", "xtabs")) {
      if (cls == "data.frame") {
        pnames <- names(popsize)[-ncol(popsize)]
        tst <- !(pnames %in% names(dframe))
        if (any(tst)) {
          error_ind <- TRUE
          temp_str <- vecprint(pnames[tst])
          msg <- paste0("\nThe following post-stratification variables do not occur among the names for \nthe dframe data frame:\n", temp_str)
          error_vec <- c(error_vec, msg)
        } else {
          for (i in length(pnames)) {
            temp <- unique(as.character(popsize[, pnames[i]]))
            tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
            if (any(tst)) {
              error_ind <- TRUE
              temp_str <- vecprint(temp[tst])
              msg <- paste0("\nThe following values for the \"", pnames[i], "\" post-stratification variable in the \npopsize data frame do not occur among the unique values for that variable in \nthe dframe data frame:\n", temp_str)
              error_vec <- c(error_vec, msg)
            }
          }
        }
      } else {
        pnames <- names(dimnames(popsize))
        if (is.null(pnames)) {
          error_ind <- TRUE
          msg <- paste0("\nThe popsize argument must have named dimnames when the class for popsize is: \n\"", cls, "\"")
          error_vec <- c(error_vec, msg)
        }
        tst <- !(pnames %in% names(dframe))
        if (any(tst)) {
          error_ind <- TRUE
          temp_str <- vecprint(pnames[tst])
          msg <- paste0("\nThe following post-stratification variables do not occur among the names for \nthe dframe data frame:\n", temp_str)
          error_vec <- c(error_vec, msg)
        } else {
          for (i in length(dim(popsize))) {
            temp <- dimnames(popsize)[[i]]
            tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
            if (any(tst)) {
              error_ind <- TRUE
              temp_str <- vecprint(temp[tst])
              msg <- paste0("\nThe following values for the \"", pnames[i], "\" post-stratification variable in the \npopsize object do not occur among the unique values for that variable in \nthe dframe data frame:\n", temp_str)
              error_vec <- c(error_vec, msg)
            }
          }
        }
      }
    } else if (cls == "list") {
      cnames <- names(popsize)
      tst <- !(cnames %in% names(dframe))
      if (any(tst)) {
        error_ind <- TRUE
        temp_str <- vecprint(cnames[tst])
        msg <- paste0("\nThe following calibration variables do not occur among the names for the dframe \ndata frame:\n", temp_str)
        error_vec <- c(error_vec, msg)
      }
      for (i in length(popsize)) {
        temp <- names(popsize[[i]])
        tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
        if (any(tst)) {
          error_ind <- TRUE
          temp_str <- vecprint(temp[tst])
          msg <- paste0("\nThe following values for the \"", cnames[i], "\" calibration variable in the popsize \nlist do not occur among the unique values for that variable in the dframe \ndata frame:\n", temp_str)
          error_vec <- c(error_vec, msg)
        }
      }
    } else {
      error_ind <- TRUE
      msg <- paste0("\nThe class for the popsize argument, \"", cls, "\", is not a valid class.  The class \nmust be either \"data frame\", \"table\", \"xtabs\", or \"list\".")
      error_vec <- c(error_vec, msg)
    }
  }

  # Check the finite population correction factor argument and, as necessary,
  # add variables to the dframe data frame

  if (!is.null(fpc)) {
    if (!is.null(stratumID)) {
      if (!is.null(clusterID)) {
        cluster_levels <- tapply(clusterID, stratumID,
                                 function(x) levels(factor(x)))
        ncluster <- sapply(cluster_levels, length)
        indx <- match(names(fpc), names(ncluster),nomatch = 0)
        if (!is.list(fpc)) {
          error_ind <- TRUE
          msg <- "For a stratified, two-stage survey design, the fpc argument must be a list.\n"
          error_vec <- c(error_vec, msg)
        } else if(length(fpc) != nstrata) {
          error_ind <- TRUE
          msg <- "For a stratified, two-stage survey design, the fpc argument must be the same length \nas the number of strata.\n"
          error_vec <- c(error_vec, msg)
        } else if(is.null(names(fpc))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must be a named list.\n"
          error_vec <- c(error_vec, msg)
        } else if(!all(stratum_levels %in% names(fpc))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, names for the fpc argument must match \ncatgories for the stratum ID variable in the dframe data frame.\n"
          error_vec <- c(error_vec, msg)
        } else if(any(sapply(fpc, function(x) !is.vector(x))) ||
                  any(sapply(fpc, length) != (ncluster[indx] + 1)) ||
                  any(sapply(fpc, function(x) !is.numeric(x)))) {
          error_ind <- TRUE
          msg <- "For a stratified, two-stage survey design, the fpc argument, for each stratum, must be \na numeric vector with length equal to one plus the number of clusters in the sample for \nthe stratum.\n"
          error_vec <- c(error_vec, msg)
        } else {
          if (any(sapply(fpc, function(x) any(x <= 0)))) {
            error_ind <- TRUE
            msg <- "For a stratified, two-stage survey design, the fpc argument, for each stratum, must contain positive values.\n"
            error_vec <- c(error_vec, msg)
          } else {
            dframe$Ncluster <- numeric(nrow(dframe))
            dframe$stage1size <- numeric(nrow(dframe))
            for (i in stratum_levels) {
              temp <- fpc[[i]]
              tst <- stratumID == i
              k <- 1
              dframe$Ncluster[tst] <- temp[k]
              for (j in levels(factor(clusterID[tst]))) {
                tst <- stratumID == i & clusterID == j
                k <- k + 1
                dframe$stage1size[tst] <- temp[k]
              }
            }
          }
        }
      } else {
        if (!is.list(fpc)) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must be a list.\n"
          error_vec <- c(error_vec, msg)
        } else if(length(fpc) != nstrata) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must be the same length \nas the number of strata.\n"
          error_vec <- c(error_vec, msg)
        } else if(is.null(names(fpc))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must be a named list.\n"
          error_vec <- c(error_vec, msg)
        } else if(!all(stratum_levels %in% names(fpc))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, names for the fpc argument must match \ncatgories for the stratum ID variable in the dframe data frame.\n"
          error_vec <- c(error_vec, msg)
        } else if(any(sapply(fpc, function(x) !is.vector(x))) ||
                  any(sapply(fpc, length) > 1) ||
                  any(sapply(fpc, function(x) !is.numeric(x)))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must contain a single \nnumeric value for each strata.\n"
          error_vec <- c(error_vec, msg)
        } else {
          if (any(fpc <= 0)) {
            error_ind <- TRUE
            msg <- "For a stratified, single-stage survey design, the fpc argument must contain a positive \nvalue for each stratum.\n"
            error_vec <- c(error_vec, msg)
          } else {
            dframe$fpcsize <- numeric(nrow(dframe))
            for (i in stratum_levels) {
              tst <- stratumID == i
              dframe$fpcsize[tst] <- fpc[[i]]
            }
          }
        }
      }
    } else {
      if (!is.null(clusterID)) {
        cluster_levels <- levels(factor(clusterID))
        ncluster <- length(cluster_levels)
        if (!is.vector(fpc)) {
          error_ind <- TRUE
          msg <- "For an unstratified, two-stage survey design, the fpc argument must be a vector\n"
          error_vec <- c(error_vec, msg)
        } else if(length(fpc) != (ncluster+1)) {
          error_ind <- TRUE
          msg <- "For an unstratified, two-stage survey design, the fpc argument must be the same length \nas the number of clusters plus one\n"
          error_vec <- c(error_vec, msg)
        } else if(is.null(names(fpc))) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, the fpc argument must be a named vector.\n"
        } else if(!all(cluster_levels %in% names(fpc)[-1])) {
          error_ind <- TRUE
          msg <- "For a stratified, single-stage survey design, names for the fpc argument must include \nall of the catgories for the cluster ID variable in the dframe data frame.\n"
          error_vec <- c(error_vec, msg)
        } else if(any(sapply(fpc, length) > 1) ||
                  any(sapply(fpc, function(x) !is.numeric(x)))) {
          error_ind <- TRUE
          msg <- "For unstratified, two-stage survey design, the fpc argument must contain a single \nnumeric value for each item in the vector\n"
          error_vec <- c(error_vec, msg)
        } else {
          if (any(fpc <= 0)) {
            error_ind <- TRUE
            msg <- "For an unstratified, two-stage survey design, the fpc argument must contain a positive \nvaluec.\n"
            error_vec <- c(error_vec, msg)
          } else {
            dframe$Ncluster <- fpc[1]
            dframe$stage1size <- numeric(nrow(dframe))
            for (i in cluster_levels) {
              tst <- clusterID == i
              dframe$stage1size[tst] <- fpc[[i]]
            }
          }
        }
      } else {
        if (!is.vector(fpc) || length(fpc) != 1 || !is.numeric(fpc)) {
          error_ind <- TRUE
          msg <- "For an unstratified, single-stage survey design, the fpc argument  must contain a single \nnumeric value.\n"
          error_vec <- c(error_vec, msg)
        } else {
          if (fpc <= 0) {
            error_ind <- TRUE
            msg <- "For an unstratified, single-stage survey design the fpc argument must contain a single \npositive value.\n"
            error_vec <- c(error_vec, msg)
          }
          dframe$fpcsize <- fpc
        }
      }
    }
  }

  # Check the vartype argument

  if (!(vartype %in% c("Local", "local", "SRS", "srs", "HT", "ht", "YG", "yg"))) {
    error_ind <- TRUE
    msg <- paste0("\nThe value provided for argument vartype must equal either \"Local\",  \"SRS\", \"HT\", or \"YG\".  \nThe value provided was: \"", vartype, "\".")
    error_vec <- c(error_vec, msg)
  }
  if (vartype == "local") {
    vartype <- "Local"
  }
  if (vartype == "srs") {
    vartype <- "SRS"
  }
  if (vartype == "ht") {
    vartype <- "HT"
  }
  if (vartype == "yg") {
    vartype <- "YG"
  }
  if (vartype %in% c("HT", "YG")) {
    if (jointprob == "Overton") {
      jointprob <- "overton"
    }
    if (jointprob == "HR") {
      jointprob <- "hr"
    }
    if (jointprob == "Brewer") {
      jointprob <- "brewer"
    }
    if (!(jointprob %in% c("overton", "hr", "brewer"))) {
      error_ind <- TRUE
      msg <- paste0("\nThe value provided for argument jointprob must equal either \"overton\",  \"hr\", or \"brewer\".  \nThe value provided was: \"", jointprob, "\".")
      error_vec <- c(error_vec, msg)
    }
    if (jointprob %in% c("overton", "hr") && !is.null(clusterID)) {
      error_ind <- TRUE
      msg <- paste0("\nA two-stage survey design is not allowed when the jointprob argument equals \"", jointprob, "\".")
      error_vec <- c(error_vec, msg)
    }
    if (jointprob == "brewer" && vartype != "HT") {
      error_ind <- TRUE
      msg <- paste("\nThe vartype argument must equal \"HT\" when the jointprob argument equals \"brewer\".")
      error_vec <- c(error_vec, msg)
    }
  }

  # Check the confidence level argument

  if (!is.numeric(conf)) {
    error_ind <- TRUE
    msg <- "The confidence level must be a numeric value.\n"
    error_vec <- c(error_vec, msg)
  }

  # Check the CDF values argument

  if (!is.null(cdfval) && !is.numeric(cdfval)) {
    error_ind <- TRUE
    msg <- "The set of value at which the CDF is estimated must be numeric values.\n"
    error_vec <- c(error_vec, msg)
  }


  # Check the percentile values argument

  if (!is.null(pctval) && !is.numeric(pctval)) {
    error_ind <- TRUE
    msg <- "The set of value at which percentiles are estimated must be numeric values.\n"
    error_vec <- c(error_vec, msg)
  }

  # Return the results list

  list(
    dframe = dframe, vars_cat = vars_cat, vars_cont = vars_cont,
    vars_stressor = vars_stressor, vars_nondetect = vars_nondetect,
    subpops = subpops, popsize = popsize, vartype = vartype,
    jointprob = jointprob, error_ind = error_ind, error_vec = error_vec
  )
}
