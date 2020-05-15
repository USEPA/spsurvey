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

dsgn_check <- function(dsgn, sframe, DesignID, SiteBegin, stratum_var, caty_var,
                       aux_var, legacy_var, mindis, startlev, maxlev) {

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
    # ensure class for stratum variable is character
    sframe[[stratum_var]] <- as.character(sframe[[stratum_var]])
  }

  # If caty_var is provided, does the attribute exist in sframe
  if(!is.null(caty_var)) {
    if(match(caty_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for unequal probability category variable does not exist as a variable in sframe\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
    # ensure class for caty variable is character
    sframe[[caty_var]] <- as.character(sframe[[caty_var]])
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


  ##### Check design object to ensure that it provides what is required.
  # Ensure that a design list is provided
  if(!is.list(dsgn)) {
    stop.ind <- TRUE
    stop.mess <- "dsgn must be a list.\n"
    stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
  } else {
    # check that design has strata names
    if(is.null(names(dsgn))) {
      stop.ind <- TRUE
      stop.mess <- "The design list must have name for each stratum. If no strata or only one stratum, then 'None' maybe used.\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
    # check that stratum_var is provided
    if(length (names(dsgn)) > 1 & is.null(stratum_var)) {
      stop.ind <- TRUE
      stop.mess <- "Design is stratified and no 'stratum_var' is provided.\n"
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }

    # check design for each stratum
    for (s in names(dsgn)) {
      if(is.list (dsgn[[s]]) ) {
        tmp <- names(dsgn[[s]]) %in% c("seltype", "panel", "caty.n", "over.n")
        if(any(tmp) == FALSE) {
          stop.ind <-  TRUE
          stop.mess <- paste0("In stratum ", s, " List components may only include 'seltype', 'panel', 'caty.n' and 'over.n'\n")
          stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
        } else {
          # seltype
          if(!(dsgn[[s]]$seltype %in% c("equal", "unequal", "proportional"))) {
            stop.ind <- TRUE
            stop.mess <- paste0("In stratum ", s, " seltype must be 'equal', 'unequal' or 'proportional'.\n")
            stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
          }

          # panel
          if(any(is.null(names(dsgn[[s]]$panel)))){
            stop.ind <- TRUE
            stop.mess <- paste0("In stratum ", s, " 'panel' must be a named vector of sample sizes.\n")
            stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
          }

          # caty.n

          if(dsgn[[s]]$seltype == "unequal") {
            # check if caty_var provided
            if(is.null(caty_var)){
              stop.ind <- TRUE
              stop.mess <- "Stratum is 'unequal' selection type and no 'caty_var' is provided.\n"
              stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
            }

            if(is.null(dsgn[[s]]$caty.n) ){
              stop.ind <- TRUE
              stop.mess <- paste0("In stratum ", s, " 'caty.n' must be provided and is named vector of sample sizes.\n")
              stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
            } else {
              if( is.null(names(dsgn[[s]]$caty.n))){
                stop.ind <- TRUE
                stop.mess <- paste0("In stratum ", s, " 'caty.n' must be a named vector of sample sizes.\n")
                stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
              }
            }
          }

          # over.n
          if (!is.null(names(dsgn[[s]]$over.n))) {
            if(dsgn[[s]]$seltype == "unequal") {
              tmp <- match(names(dsgn[[s]]$over.n), names(dsgn[[s]]$caty.n), nomatch = 0)
              if(any(tmp == 0)) {
                stop.ind <- TRUE
                stop.mess <- paste0("In stratum ", s, " names for 'over.n' must match names for 'caty.n'.\n")
                stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
              }
            } else {
              if (length(dsgn[[s]]$over.n) > 1) {
                stop.ind <- TRUE
                stop.mess <- paste0("In stratum ", s, " 'over.n' must be a single value for sample size.\n")
                stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
              }
            }
          }

          # check sample size for panels equals sample sizes for caty.n
          if (dsgn[[s]]$seltype == "unequal" &
              sum(dsgn[[s]]$panel) != sum(dsgn[[s]]$caty.n)) {
            stop.ind <- TRUE
            stop.mess <- paste0("In stratum ", s, " Sum of sample sizes in 'panel' and 'caty.n' must be equal.\n")
            stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
          }

          # if seltype is proportional, check if aux_var provided
          if(dsgn[[s]]$seltype == "proportional" & is.null(aux_var)){
            stop.ind <- TRUE
            stop.mess <- "Stratum is 'proportional' selection type and no 'aux_var' is provided.\n"
            stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
          }
        }

    } else { # stratum s is not a list
      stop.ind <- TRUE
      stop.mess <- paste0("In stratum ", s, " must be a list.\n")
      stop.df <- rbind(stop.df, data.frame(func = I(fname), I(stop.mess)))
    }
   } # end of stratum loop
  } # end of checking dsgn


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



