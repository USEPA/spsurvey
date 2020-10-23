################################################################################
# Function: input_check
# Programmer: Tom Kincaid
# Date: October 9, 2020
#'
#' Check Input Values for Analytical Functions
#'
#' This function checks input values for errors, consistency, and compatibility
#' with analytical functions.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param design_names Vector composed of character values that identify the
#'   names of the following survey design variables in the dframe argument:
#'   siteID, weight, xcoord, ycoord, stratumID, clusterID, weight1, xcoord1,
#'   ycoord1, sweight, sweight1, fpcsize, Ncluster, stage1size.
#'
#' @param vars_cat Vector composed of character values that identify the
#'   names of categorical response variables in the dframe data frame.
#'
#' @param vars_cont Vector composed of character values that identify the
#'   names of continuous response variables in the dframe data frame.
#'
#' @param vars_stressor Vector composed of character values that identify the
#'   names of stressor variables in the dframe argument.
#'
#' @param vars_nondetect Vector composed of character values that identify the
#'   names of logical variables in the dframe data frame specifying the presence
#'   of not detected (nondetect) values for response variables.
#'
#' @param subpops Vector composed of character values that identify the
#'   names of subpopulation variables in the dframe argument.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor is used during variance estimation.
#'
#' @param popsize List providing known size of the resource.
#'
#' @param vartype  Character value identifying the choice for variance
#'   estimator, where "Local" = local mean estimator and "SRS" = simple random
#'   sampling estimator.
#'
#' @param conf Numeric value providing the confidence level.
#'
#' @param cdfval Vector of the set of values at which the CDF is estimated.
#'
#' @param pctval Vector of the set of values at which percentiles are
#'   estimated.
#'
#' @param sigma Numeric value providing the measurement error variance.
#'
#' @param var.sigma Numeric value providing the variance of the measurement
#'   error variance.
#'
#' @param error.ind Logical value that indicates whether error messages were
#'   generated.
#'
#' @param error.vec Vector for storing error messages.
#'
#' @return A list consisting of dframe, vars_cat, vars_cont, vars_stressor,
#'   subpops, popsize, vartype, error.ind, and error.vec.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{vecprint}}}{takes an input vector and outputs a
#'       character string with line breaks inserted}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

input_check <- function(dframe, design_names, vars_cat, vars_cont,
  vars_stressor, vars_nondetect, subpops, sizeweight, popcorrect, popsize,
  vartype, conf, cdfval = NULL, pctval = NULL, sigma = NULL, var.sigma = NULL,
  error.ind, error.vec) {

# For variables that exist in the dframe data frame, assign survey design
# variables and check those variables for missing values

  temp <- c("surveyID", "siteID", "weight")
  for(i in names(design_names)) {
    if(is.null(design_names[[i]])) {
      eval(parse(text=paste0(i, " <- NULL")))
    } else {
      if(!(design_names[[i]] %in% names(dframe))) {
        if(!(i %in% temp)) {
          error.ind <- TRUE
          msg <- paste0("The name provided for the ", i, " argument, \"", design_names[[i]], "\", does not occur among \nthe names for the dframe data frame.\n")
          error.vec <- c(error.vec, msg)
        }
        eval(parse(text=paste0(i, " <- NULL")))
      } else {
        eval(parse(text=paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
        if(any(is.na(dframe[, design_names[[i]]]))) {
          error.ind <- TRUE
          msg <- paste0("Missing values are not allowed for the ", design_names[[i]], " variable in the dframe data \nframe.\n")
          error.vec <- c(error.vec, msg)
          eval(parse(text=paste0(i, " <- NULL")))
        }
      }
    }
  }

# If the sample is stratified, code stratumID as a factor, determine stratum
# levels, and calculate number of strata

  if(!is.null(stratumID)) {
    stratumID <- factor(stratumID)
    stratum.levels <- levels(stratumID)
    nstrata <- length(stratum.levels)
  }

# Check the response variables arguments to ensure that the arguments are vectors
# composed of character values and that the elements of the vectors are included
# among the names in the dframe data frame.  For categorical response variables,
# ensure that the variables referenced in the argument are coded as factors.  For
# continuous response variables, ensure that the variables referenced in the
# argument are coded as numeric variables.

  if(!is.null(vars_cat)) {
    if(!is.vector(vars_cat)) {
      error.ind <- TRUE
      msg <- "The vars_cat argument must be a vector.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(is.list(vars_cat)) {
        vars_cat <- unlist(vars_cat)
      }
      if(!all(is.character(vars_cat))) {
        vars_cat <- sapply(vars_cat, as.character)
      }
      tst <- !(vars_cat %in% names(dframe))
      if(any(tst)) {
        error.ind <- TRUE
        temp.str <- vecprint(vars_cat[tst])
        msg <- paste0("The following categorical response variable names do not occur among the \nnames for the dframe data frame:\n", temp.str)
        error.vec <- c(error.vec, msg)
      } else {
        for(i in vars_cat) {
          if(!is.factor(dframe[, i])) {
            dframe[, i] <- factor(dframe[, i])
          }
        }
      }
    }
  }

  if(!is.null(vars_cont)) {
    if(!is.vector(vars_cont)) {
      error.ind <- TRUE
      msg <- "The vars_cont argument must be a vector.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(is.list(vars_cont)) {
        vars_cont <- unlist(vars_cont)
      }
      tst <- !(vars_cont %in% names(dframe))
      if(any(tst)) {
        error.ind <- TRUE
        temp.str <- vecprint(vars_cont[tst])
        msg <- paste0("The following continuous response variable names do not occur among the \nnames for the dframe data frame:\n", temp.str)
        error.vec <- c(error.vec, msg)
      }
    }
  }

# Check the stressor variables argument to ensure that the argument is a vector
# composed of character values and that the elements of the vector are included
# among the names in the dframe data frame.  Ensure that the variables
# referenced in the vector are coded as factors.

  if(!is.null(vars_stressor)) {
    if(!is.vector(vars_stressor)) {
      error.ind <- TRUE
      msg <- "The vars_stressor argument must be a vector.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(is.list(vars_stressor)) {
        vars_stressor <- unlist(vars_stressor)
      }
      if(!all(is.character(vars_stressor))) {
        vars_stressor <- sapply(vars_stressor, as.character)
      }
      tst <- !(vars_stressor %in% names(dframe))
      if(any(tst)) {
        error.ind <- TRUE
        temp.str <- vecprint(vars_stressor[tst])
        msg <- paste0("The following stressor[ variable names do not occur among the names for the \ndframe data frame:\n", temp.str)
        error.vec <- c(error.vec, msg)
      } else {
        for(i in vars_stressor) {
          if(!is.factor(dframe[, i])) {
            dframe[, i] <- factor(dframe[, i])
          }
        }
      }
    }
  }

# Check the vars_nondetect logical variables argument to ensure that the
# argument is a vector composed of character values and that the elements of the
# vector are included among the names in the dframe data frame.

  if(!is.null(vars_nondetect)) {
    if(!is.vector(vars_nondetect)) {
      error.ind <- TRUE
      msg <- "The vars_nondetect argument must be a vector.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(is.list(vars_nondetect)) {
        vars_nondetect <- unlist(vars_nondetect)
      }
      if(!all(is.character(vars_nondetect))) {
        vars_nondetect <- sapply(vars_nondetect, as.character)
      }
      tst <- !(vars_nondetect %in% names(dframe))
      if(any(tst)) {
        error.ind <- TRUE
        temp.str <- vecprint(vars_nondetect[tst])
        msg <- paste0("The following response variable names do not occur among the names for the \ndframe data frame:\n", temp.str)
        error.vec <- c(error.vec, msg)
      } else {
        for(i in vars_nondetect) {
          if(!is.factor(dframe[, i])) {
            dframe[, i] <- factor(dframe[, i])
          }
        }
      }
    }
  }

# Check the subpopulation (domain) names argument to ensure that the argument is
# a vector composed of character values, that the elements of the vector are
# included among the names in the dframe data frame, and that all response
# variables referenced in the vector are coded as factors

  if(is.list(subpops)) {
    subpops <- unlist(subpops)
  }
  if(!is.vector(subpops)) {
    subpops <- as.vector(subpops)
  }
  if(!all(is.character(subpops))) {
    subpops <- as.character(subpops)
  }
  tst <- !(subpops %in% names(dframe))
  if(any(tst)) {
    error.ind <- TRUE
    temp.str <- vecprint(subpops[tst])
    msg <- paste0("The following subpopulation (domain) names do not occur among the names for the \ndframe data frame:\n", temp.str)
    error.vec <- c(error.vec, msg)
  } else {
    for(i in subpops) {
      if(!is.factor(dframe[, i])) {
        dframe[, i] <- factor(dframe[, i])
      }
    }
  }

# Check weight arguments

  if(!is.null(clusterID)) {
    if(!is.null(weight)) {
      if(min(weight, na.rm=TRUE) <= 0) {
        error.ind <- TRUE
        msg <- "Stage two weights must be positive.\n"
        error.vec <- c(error.vec, msg)
      }
    }
    if(sizeweight) {
      if(is.null(sweight)) {
        error.ind <- TRUE
        msg <- "Argument sweight was not supplied.\n"
        error.vec <- c(error.vec, msg)
      } else {
        if(min(sweight, na.rm=TRUE) <= 0) {
          error.ind <- TRUE
          msg <- "Stage two size-weights must be positive.\n"
          error.vec <- c(error.vec, msg)
        }
      }
    }
    if(is.null(weight1)) {
      error.ind <- TRUE
      msg <- "Argument weight1 was not supplied.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(min(weight1, na.rm=TRUE) <= 0) {
        error.ind <- TRUE
        msg <- "Stage one weights must be positive.\n"
        error.vec <- c(error.vec, msg)
      }
      if(!is.null(stratumID)) {
        temp.weight1 <- split(weight1, stratumID)
        for(i in 1:nstrata) {
          tst <- stratumID == stratum.levels[i]
          temp.clusterID <- clusterID[tst]
          if(any(sapply(tapply(temp.weight1[[i]], temp.clusterID, unique),
            length) > 1)) {
            error.ind <- TRUE
            msg <- paste0("\nThe stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum.levels[i], ".\n")
            error.vec <- c(error.vec, msg)
          }
        }
      } else {
        if(any(sapply(tapply(weight1, clusterID, unique), length) > 1)) {
          error.ind <- TRUE
          msg <- "The stage one weight must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
          error.vec <- c(error.vec, msg)
        }
      }
    }
    if(is.null(sweight1)) {
      error.ind <- TRUE
      msg <- "Argument sweight1 was not supplied.\n"
      error.vec <- c(error.vec, msg)
    } else {
      if(min(sweight1, na.rm=TRUE) <= 0) {
        error.ind <- TRUE
        msg <- "Stage one size-weights must be positive.\n"
        error.vec <- c(error.vec, msg)
      }
      if(!is.null(stratumID)) {
        temp.sweight1 <- split(sweight1, stratumID)
        for(i in 1:nstrata) {
          tst <- stratumID == stratum.levels[i]
          temp.clusterID <- clusterID[tst]
          if(any(sapply(tapply(temp.sweight1[[i]], temp.clusterID, unique),
              length) > 1)) {
              error.ind <- TRUE
              msg <- paste0("\nThe stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit of stratum ", stratum.levels[i], ".\n")
              error.vec <- c(error.vec, msg)
          }
        }
      } else {
        if(any(sapply(tapply(sweight1, clusterID, unique), length) > 1)) {
            error.ind <- TRUE
            msg <- "The stage one size-weight must be constant for all stage two sampling units \nwithin each stage one sampling unit.\n"
            error.vec <- c(error.vec, msg)
        }
      }
    }
  } else {
    if(!is.null(weight)) {
      if(min(weight, na.rm=TRUE) <= 0) {
        error.ind <- TRUE
        msg <- "Weights must be positive.\n"
        error.vec <- c(error.vec, msg)
      }
    }
    if(sizeweight) {
      if(is.null(sweight)) {
        error.ind <- TRUE
        msg <- "Argument sweight was not supplied.\n"
        error.vec <- c(error.vec, msg)
      } else {
        if(min(sweight, na.rm=TRUE) <= 0) {
          error.ind <- TRUE
          msg <- "Size-weights must be positive.\n"
          error.vec <- c(error.vec, msg)
        }
      }
    }
  }

# Check coordinate arguments

  if(vartype == "Local") {
    if(!is.null(clusterID)) {
      if(is.null(xcoord) || is.null(ycoord)) {
        error.ind <- TRUE
        msg <- "Stage two x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.\n"
        error.vec <- c(error.vec, msg)
      }
      if(is.null(xcoord1) || is.null(ycoord1)) {
        error.ind <- TRUE
        msg <- "Stage one x-coordinates and y-coordinates for location are required for the \nlocal mean variance estimator.\n"
        error.vec <- c(error.vec, msg)
      } else {
        if(!is.null(stratumID)) {
          temp.xcoord1 <- split(xcoord1, stratumID)
          temp.ycoord1 <- split(ycoord1, stratumID)
          for(i in 1:nstrata) {
            tst <- stratumID == stratum.levels[i]
            temp.clusterID <- clusterID[tst]
            if(any(sapply(tapply(temp.xcoord1[[i]], temp.clusterID, unique),
              length) > 1)) {
              error.ind <- TRUE
              msg <- paste0("\nThe stage one x-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum.levels[i], ".\n")
              error.vec <- c(error.vec, msg)
            }
            if(any(sapply(tapply(temp.ycoord1[[i]], temp.clusterID, unique),
              length) > 1)) {
              error.ind <- TRUE
              msg <- paste0("\nThe stage one y-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit of stratum ", stratum.levels[i], ".\n")
              error.vec <- c(error.vec, msg)
            }
          }
        } else {
          if(any(sapply(tapply(xcoord1, clusterID, unique), length) > 1)) {
            error.ind <- TRUE
            msg <- "The stage one x-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
            error.vec <- c(error.vec, msg)
          }
          if(any(sapply(tapply(ycoord1, clusterID, unique), length) > 1)) {
            error.ind <- TRUE
            msg <- "The stage one y-coordinate must be constant for all stage two sampling units within \neach stage one sampling unit.\n"
            error.vec <- c(error.vec, msg)
          }
        }
      }
    } else {
      if(is.null(xcoord) || is.null(ycoord)) {
        error.ind <- TRUE
        msg <- "x-coordinates and y-coordinates for location are required for the local mean \nvariance estimator.\n"
        error.vec <- c(error.vec, msg)
      }
    }
  }

# Check the known size of the resource argument

  if(!is.null(popsize)) {
    cls <- class(popsize)[1]
    if(cls %in% c("data.frame", "table", "xtabs")) {
      if(cls == "data.frame") {
        pnames <- names(popsize)[-ncol(popsize)]
        tst <- !(pnames %in% names(dframe))
        if(any(tst)) {
          error.ind <- TRUE
          temp.str <- vecprint(pnames[tst])
          msg <- paste0("\nThe following post-stratification variables do not occur among the names for \nthe dframe data frame:\n", temp.str)
          error.vec <- c(error.vec, msg)
        } else {
          for(i in length(pnames)) {
            temp <- unique(as.character(popsize[, pnames[i]]))
            tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
            if(any(tst)) {
              error.ind <- TRUE
              temp.str <- vecprint(temp[tst])
              msg <- paste0("\nThe following values for the \"", pnames[i], "\" post-stratification variable in the \npopsize data frame do not occur among the unique values for that variable in \nthe dframe data frame:\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
      } else {
        pnames <- names(dimnames(popsize))
        if(is.null(pnames)) {
          error.ind <- TRUE
          msg <- paste0("\nThe popsize argument must have named dimnames when the class for popsize is: \n\"", cls, "\"")
          error.vec <- c(error.vec, msg)
        }
        tst <- !(pnames %in% names(dframe))
        if(any(tst)) {
          error.ind <- TRUE
          temp.str <- vecprint(pnames[tst])
          msg <- paste0("\nThe following post-stratification variables do not occur among the names for \nthe dframe data frame:\n", temp.str)
          error.vec <- c(error.vec, msg)
        } else {
          for(i in length(dim(popsize))) {
            temp <- dimnames(popsize)[[i]]
            tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
            if(any(tst)) {
              error.ind <- TRUE
              temp.str <- vecprint(temp[tst])
              msg <- paste0("\nThe following values for the \"", pnames[i], "\" post-stratification variable in the \npopsize object do not occur among the unique values for that variable in \nthe dframe data frame:\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
      }
    } else if(cls == "list") {
      cnames <- names(popsize)
      tst <- !(cnames %in% names(dframe))
      if(any(tst)) {
        error.ind <- TRUE
        temp.str <- vecprint(cnames[tst])
        msg <- paste0("\nThe following calibration variables do not occur among the names for the dframe \ndata frame:\n", temp.str)
        error.vec <- c(error.vec, msg)
      }
      for(i in length(popsize)) {
        temp <- names(popsize[[i]])
        tst <- !(temp %in% unique(as.character(dframe[, pnames[i]])))
        if(any(tst)) {
          error.ind <- TRUE
          temp.str <- vecprint(temp[tst])
          msg <- paste0("\nThe following values for the \"", cnames[i], "\" calibration variable in the popsize \nlist do not occur among the unique values for that variable in the dframe \ndata frame:\n", temp.str)
          error.vec <- c(error.vec, msg)
        }
      }
    } else {
      error.ind <- TRUE
      msg <- paste0("\nThe class for the popsize argument, \"", cls, "\", is not a valid class.  The class \nmust be either \"data frame\", \"table\", \"xtabs\", or \"list\".")
      error.vec <- c(error.vec, msg)
    }
  }

# Check the finite population correction factor arguments

  if(popcorrect) {
    if(!is.null(stratumID)) {
      if(!is.null(clusterID)) {
        if(is.null(Ncluster)) {
          error.ind <- TRUE
          msg <- "The known number of stage one sampling units must be provided in order to \ncalculate the finite population correction factor for variance estimation \nin a two-stage sample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(Ncluster <= 0)) {
            error.ind <- TRUE
            msg <- "The known number of stage one sampling units for each stratum must be positive.\n"
            error.vec <- c(error.vec, msg)
          }
          temp.Ncluster <- tapply(Ncluster, stratumID, unique)
          temp <- sapply(temp.Ncluster, length) > 1
          if(any(temp)) {
            error.ind <- TRUE
            temp.str <- vecprint(stratum.levels[temp])
            msg <- paste0("\nThe known number of stage one sampling units was not constant for the \nfollowing strata:\n", temp.str)
            error.vec <- c(error.vec, msg)
          } else {
            temp <- tapply(Ncluster, stratumID, length) > temp.Ncluster
            if(any(temp)) {
              error.ind <- TRUE
              temp.str <- vecprint(stratum.levels[temp])
              msg <- paste0("\nThe number of sampled stage one sampling units exceeded the known number of \nstage one sampling units for the following strata:\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
        if(is.null(stage1size)) {
          error.ind <- TRUE
          msg <- "The known number of stage two sampling units must be provided in order to \ncalculate the finite population correction factor for variance estimation \nin a two-stage sample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(stage1size <= 0)) {
            error.ind <- TRUE
            msg <- "The known number of stage two sampling units must be positiv.\n"
            error.vec <- c(error.vec, msg)
          }
          temp.stage1size <- tapply(stage1size, factor(paste(stratumID,
            clusterID, sep=":")), unique)
          temp <- sapply(temp.stage1size, length) > 1
          if(any(temp)) {
            error.ind <- TRUE
            temp.str <- vecprint(names(temp.stage1size[temp]))
            msg <- paste0("\nThe known number of stage two sampling units was not constant for the \nfollowing stratumID:clusterID combination(s):\n", temp.str)
            error.vec <- c(error.vec, msg)
          } else {
            temp <- tapply(stage1size, factor(paste(stratumID, clusterID,
              sep=":")), length) > temp.stage1size
            if(any(temp)) {
              error.ind <- TRUE
              temp.str <- vecprint(names(temp.stage1size[temp]))
              msg <- paste0("\nThe number of sampled stage two sampling units exceeded the known number of \nstage two sampling units for the following stratumID:clusterID combination(s):\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
      } else {
        if(is.null(fpcsize)) {
          error.ind <- TRUE
          msg <- "The known size of the resource must be provided in order to calculate the \nfinite population correction factor for variance estimation in a single-stage \nsample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(fpcsize <= 0)) {
            error.ind <- TRUE
            msg <- "The known size of the resource must be positive.\n"
            error.vec <- c(error.vec, msg)
          }
          temp.fpcsize <- tapply(fpcsize, stratumID, unique)
          temp <- sapply(temp.fpcsize, length) > 1
          if(any(temp)) {
            error.ind <- TRUE
            temp.str <- vecprint(stratum.levels[temp])
            msg <- paste0("\nThe known number of sites was not constant for the following strata:\n", temp.str)
            error.vec <- c(error.vec, msg)
          } else {
            temp <- tapply(fpcsize, stratumID, length) > temp.fpcsize
            if(any(temp)) {
              error.ind <- TRUE
              temp.str <- vecprint(stratum.levels[temp])
              msg <- paste0("\nThe number of sampled sites exceeded the known number of sites for the \nfollowing strata:\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
      }
    } else {
      if(!is.null(clusterID)) {
        if(is.null(Ncluster)) {
          error.ind <- TRUE
          msg <- "The known number of stage one sampling units must be provided in order to \ncalculate the finite population correction factor for variance estimation in a \ntwo-stage sample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(Ncluster <= 0)) {
            error.ind <- TRUE
            msg <- "The known number of stage one sampling units must be a positive value.\n"
            error.vec <- c(error.vec, msg)
          }
          temp <- unique(Ncluster)
          if(length(temp) > 1) {
            error.ind <- TRUE
            msg <- "The known number of stage one sampling units was not constant.\n"
            error.vec <- c(error.vec, msg)
          } else {
            if(length(Ncluster) > temp) {
              error.ind <- TRUE
              msg <- "The number of sampled stage one sampling units exceeded the known number of \nstage one sampling units.\n"
              error.vec <- c(error.vec, msg)
            }
          }
        }
        if(is.null(stage1size)) {
          error.ind <- TRUE
          msg <- "The known size of the stage one sampling units must be provided in order to \ncalculate the finite and continuous population correction factor for variance \nestimation in a two-stage sample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(stage1size <= 0)) {
            error.ind <- TRUE
            msg <- "The known size of the stage one sampling units must be positive values.\n"
            error.vec <- c(error.vec, msg)
          }
          temp.stage1size <- tapply(stage1size, clusterID, unique)
          temp <- sapply(temp.stage1size, length) > 1
          if(any(temp)) {
            error.ind <- TRUE
            temp.str <- vecprint(names(temp.stage1size[temp]))
            msg <- paste0("\nThe known number of stage two sampling units was not constant for the \nfollowing clusterID value(s):\n", temp.str)
            error.vec <- c(error.vec, msg)
          } else {
            temp <- tapply(stage1size, clusterID, length) > temp.stage1size
            if(any(temp)) {
              error.ind <- TRUE
              temp.str <- vecprint(names(temp.stage1size[temp]))
              msg <- paste0("\nThe number of sampled stage two sampling units exceeded the known number of \nstage two sampling units for the following clusterID value(s):\n", temp.str)
              error.vec <- c(error.vec, msg)
            }
          }
        }
      } else {
        if(is.null(fpcsize)) {
          error.ind <- TRUE
          msg <- "The known size of the resource must be provided in order to calculate the \nfinite population correction factor for variance estimation in a single-stage \nsample.\n"
          error.vec <- c(error.vec, msg)
        } else {
          if(any(fpcsize <= 0)) {
            error.ind <- TRUE
            msg <- "The known size of the resource must be positive.\n"
            error.vec <- c(error.vec, msg)
          }
          temp <- unique(fpcsize)
          if(length(temp) > 1) {
            error.ind <- TRUE
            msg <- "The known number of sites was not constant.\n"
            error.vec <- c(error.vec, msg)
          } else {
            if(length(fpcsize) > temp) {
              error.ind <- TRUE
              msg <- "The number of sampled sites exceeded the known number of sites.\n"
              error.vec <- c(error.vec, msg)
            }
          }
        }
      }
    }
  }

# Check the vartype argument

  if(!(vartype %in% c("Local", "local", "SRS", "srs"))) {
    error.ind <- TRUE
    msg <- paste0("\nThe value provided for argument vartype must equal either \"Local\" or \"SRS\".  \nThe value provided was: \"", vartype, "\".")
    error.vec <- c(error.vec, msg)
  }
  if(vartype == "local") {
    vartype <- "Local"
  }
  if(vartype == "srs") {
    vartype <- "SRS"
  }

# For a two-stage sample, ensure that the Ncluster argument is provided when
# popsize is not equal to NULL and vartype equals "Local"

  if(!is.null(clusterID) && !is.null(popsize) && vartype == "Local" &&
      is.null(Ncluster)) {
    error.ind <- TRUE
    msg <- "For a two-stage sample, the Ncluster argument must be specified whenever the \npopsize argument is not equal to NULL and the vartype argument equals \"Local\".\n"
    error.vec <- c(error.vec, msg)
  }

# Check the confidence level argument

  if(!is.numeric(conf)) {
    error.ind <- TRUE
    msg <- "The confidence level must be a numeric value.\n"
    error.vec <- c(error.vec, msg)
  }

# Check the CDF values argument

  if(!is.null(cdfval) && !is.numeric(cdfval)) {
    error.ind <- TRUE
    msg <- "The set of value at which the CDF is estimated must be numeric values.\n"
    error.vec <- c(error.vec, msg)
  }


# Check the percentile values argument

  if(!is.null(pctval) && !is.numeric(pctval)) {
    error.ind <- TRUE
    msg <- "The set of value at which percentiles are estimated must be numeric values.\n"
    error.vec <- c(error.vec, msg)
  }

# Check measurement error arguments

  if(!is.null(sigma)) {
    if(length(sigma) > 1) {
      if(!is.numeric(sigma)) {
        error.ind <- TRUE
        msg <- "The values provided for measurement error variance must be numeric.\n"
        error.vec <- c(error.vec, msg)
      }
      temp <- sigma[!is.na(sigma)] <= 0
      if(any(temp)) {
        error.ind <- TRUE
        temp.str <- vecprint(names(sigma)[temp])
        msg <- paste0("\nA positive value for measurement error variance was not provided for the \nfollowing response variables:\n", temp.str)
        error.vec <- c(error.vec, msg)
      }
      if(!is.null(var.sigma)) {
        if(!is.numeric(var.sigma)) {
          error.ind <- TRUE
          msg <- "The values provided for variance of the measurement error variance must be \nnumeric.\n"
          error.vec <- c(error.vec, msg)
        }
        temp <- var.sigma[!is.na(var.sigma)] <= 0
        if(any(temp)) {
          error.ind <- TRUE
          temp.str <- vecprint(names(var.sigma)[temp])
          msg <- paste0("\nA positive value for variance of the estimated measurement error variance was \nnot provided for the following response variables:\n", temp.str)
          error.vec <- c(error.vec, msg)
        }
      }
    } else {
      if(!is.numeric(sigma)) {
        error.ind <- TRUE
        msg <- "The value provided for measurement error variance must be numeric.\n"
        error.vec <- c(error.vec, msg)
      }
      if(sigma <= 0) {
        error.ind <- TRUE
        msg <- "The value provided for measurement error variance must be positive.\n"
        error.vec <- c(error.vec, msg)
      }
      if(!is.null(var.sigma)) {
        if(!is.numeric(var.sigma)) {
          error.ind <- TRUE
          msg <- "The value provided for variance of the measurement error variance must be \nnumeric.\n"
          error.vec <- c(error.vec, msg)
        }
        if(var.sigma <= 0) {
          error.ind <- TRUE
          msg <- "The value provided for variance of the measurement error variance must be positive.\n"
          error.vec <- c(error.vec, msg)
        }
      }
    }
  }

# Return the results list

list(dframe = dframe, vars_cat = vars_cat, vars_cont = vars_cont,
  vars_stressor = vars_stressor, vars_nondetect = vars_nondetect,
  subpops = subpops, popsize = popsize, vartype = vartype,
  error.ind = error.ind, error.vec = error.vec)
}
