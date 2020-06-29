###################################################################################
# Function: dsgn_check
# Programmers: Tony Olsen
# Date: "`r format(Sys.time(),  '%B %d, %Y')`"
#'
#' Check the input associated with the survey design list object and the sample frame
#'
#' @param dsgn Named list of stratum design specifications which are also
#'   lists.  Stratum names must be subset of values in stratum argument.  Each
#'   stratum list has four components:
#'   \describe{
#'     \item{panel}{named vector of sample sizes for each panel in stratum}
#'     \item{seltype}{the type of random selection, which must be one of
#'       following: "Equal" - equal probability of selection, "Unequal" - unequal
#'       probability of selection by the categories specified in caty.n,
#'       or "Proportional" - unequal probability selection proportional to
#'       auxiliary variable mdcaty}
#'     \item{caty.n}{if seltype equals "Unequal", a named vector of sample sizes
#'       for each category specified by a sample frame variable ipcaty, where sum
#'       of the sample sizes must equal sum of the panel sample sizes, and names
#'       must be a subset of values in ipcaty}
#'     \item{over}{number of replacement sites ("oversample" sites) for the
#'       entire design, which is set equal to 0 if none are required)}
#'   }
#'
#' @param sframe Sample frame for points as an sf object
#'
#'
#' @param ipcaty The categorical character variable in the sample frame that
#'   identifies the category for each element that will be used for calculating
#'   the unequal inclusion probabilities for the stratum.
#'
#' @return Nothing is returned. If errors are found they are collected and written out.
#'   One or more errors will cause the call to grtspts to stop.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{{\link{junk}}}{What it does}
#'     }
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#'
#' @export
#################################################################################

dsgn_check <- function(sframe, stratum, seltype, nsamp, caty.n, over.n, over.near, stratum_var, 
                       caty_var, aux_var, legacy_var, mindis, DesignID, SiteBegin, maxtry,
                       startlev, maxlev) {

  # Create a data frame for stop messages
  stop.ind <- FALSE
  stop.df <- NULL
  fname <- "grtspts input"

  ## Check Sample frame and that it has variables that are as required
  # check that sframe is an sf point geometry
  tmp <- st_geometry_type(sframe)
  tst <- all(tmp %in% c("POINT", "MULTIPOINT"))
  if(!tst) {
    stop.ind <- TRUE
    stop.mess <- "The geometry type for the sample frame is not POINT or MULTIPOINT\n"
    stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
  }

  # check that sframe has required variables for stratum, caty, aux and legacy
  # If stratum_var is provided, does the attribute exist in sframe
  if(!is.null(stratum_var)) {
    if(match(stratum_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for stratum variable does not exist as a variable in sframe\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
  }

  # If caty_var is provided, does the attribute exist in sframe
  if(!is.null(caty_var)) {
    if(match(caty_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for unequal probability category variable does not exist as a variable in sframe\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
  }

  # If aux_var is provided, does the attribute exist in sframe
  if(!is.null(aux_var)) {
    if(match(aux_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for the auxillary variable for proportional sampling does not exist as a variable in sframe\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
    # ensure class for aux variable is numeric
    if(!is.numeric (sframe[[aux_var]])) {
      stop.ind <- TRUE
      stop.mess <- "The auxillary variable for proportional sampling must be numeric\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    } else {
      # check that values are > 0.
      if(any (sframe[[aux_var]] <= 0)) {
        stop.ind <- TRUE
        stop.mess <- "The auxillary variable for proportional sampling must have all values greater than zero\n"
        stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
      }
    }
  }

  # If legacy_var is provided, does the attribute exist in sframe
  if(!is.null(legacy_var)) {
    if(match(legacy_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for the variable identifying legacy sites does not exist as a variable in sframe\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
    # check that legacy variable is logical
    if(!is.logical(sframe[[legacy_var]])) {
      stop.ind <- TRUE
      stop.mess <- "The legacy variable must be a logical variable with TRUE for sites to included.\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
  }


  ##### Check design components to ensure they provide what is required.
  
  # check if stratum is provided and values are in sframe
  if(!is.null(stratum)) {
    if(is.null(stratum_var)){
      stop.ind <- TRUE
      stop.mess <- "Design is stratified and no 'stratum_var' is provided.\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    } else {
      if(any(stratum %in% unique(sframe[[stratum_var]]) == FALSE)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Not all stratum values are in sample frame.\n")
        stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
      }
    }
  }
  
  # check seltype
  if(any(seltype %in% c("equal", "unequal", "proportional") == FALSE)) {
    stop.ind <- TRUE
    stop.mess <- paste0("seltype must be 'equal', 'unequal' or 'proportional'.\n")
    stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
  }
  
  # check nsamp
  if(any(nsamp <= 0)) {
    stop.ind <- TRUE
    stop.mess <- paste0("Sample size must be integers greater than 0.\n")
    stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
  }
  
  # check caty.n
  if(!is.null(caty.n)) {
    if(!is.list(caty.n)) {
      if(any(caty.n %in% unique(sframe[[caty_var]]) == FALSE)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Not all caty.n values are in sample frame.\n")
        stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
      }
      if(nsamp != sum(caty.n)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Sum of caty.n values do not equal nsamp.\n")
        stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
      }
    } 
  }
  
  # check over.n
  if(!is.null(caty.n)) {
    if(!is.list(over.n)) {
      if(any(over.n < 0)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("over.n values must be zero or positive.\n")
        stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
      }
    }
  }
  
  

  # check over.near
  if(!is.null(over.near)) {
    if(!(over.near %in% 1:10)) {
      stop.ind <-  TRUE
      stop.mess <- paste0("over.near must be from 1 to 10.\n")
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
  }
  

  ### If any issues, write out stop.df and then stop
  if(stop.ind) {
    stop.df <<- stop.df
    cat("During the check of the input to grtspts, one or more issues were identified.\n")
    cat("Enter the following command to view all input issue messages: stopprnt()\n")
    cat("To view a subset of the issues (e.g., issues 1 and 5) enter stopprnt(m=c(1,5))\n\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()

   }

}


