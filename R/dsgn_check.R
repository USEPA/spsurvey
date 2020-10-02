###################################################################################
# Function: dsgn_check
# Programmer: Tony Olsen
# Date: September 28, 2020
#'
#' Check the input associated with the survey design list object and the sample frame
#' 
#' @param sframe Sample frame as an sf object.
#' 
#' @param sf_type Geometry type for sample frame: point, linear, or area.
#' 
#' @param legacy_sites An sf object of any legacy sites to be included in the survey
#'   design.
#'
#' @param legacy_option Logical variable where if equal to TRUE, legacy sites are
#'   to be incorporated into survey design.
#' 
#' @param stratum Single character value or character vector that identifies the strata for
#'   the design.  Default is NULL which means no stratification and all elements in the sample
#'   frame are assumed to be included.
#'   
#' @param seltype Single character value or character vector that identifies the type of 
#'   random selection, which must be one of following: "equal" for equal probability selection, 
#'   "unequal" for unequal probability selection by the categories specified in caty.n or 
#'   "proportional" for unequal probability selection proportional to the auxiliary variable
#'   aux_var. If single character, then seltype applies to all strata. If vector, then each 
#'   stratum may have different selection type. Default is single character value of "equal".
#' 
#' @param n.samp The sample size required. If single stratum, then single numeric value.
#'   If sample is stratified, then numeric vector with same length as "stratum" and sample sizes
#'   required in same order as strata in "stratum". Must be specified.
#' 
#' @param caty.n If design is not stratified and seltype is "unequal", a named character vector
#'   with the expected sample size for each category specified in variable caty_var. If design
#'   is stratified, then either a named character vector with the expected sample size for each
#'   category for all strata or if the expected sample size for each category may differ, then
#'   a list of named character vectors with the expected sample size for each category in the
#'   stratum. The list must be in same order as the "stratum" variable. For each stratum, 
#'   the sum of caty.n values must equal n.samp for that stratum. Default is NULL.
#'   
#' @param n.over If seltype is "equal" and is not stratified, a numeric value specifying the 
#'   over sample size requested. If seltype is "equal" and is stratified either a numeric value
#'   specifying the over sample size that will be applied to each stratum or a numeric vector
#'   specifying the over sample size for each stratum listed in same order as "strata".  
#'   If seltype is "unequal" and is not stratified, a named character vector with the over sample size 
#'   for each category where names are the same as "caty.n". If seltype is "unequal" and is 
#'   stratified,  either a numeric vector specifying the over sample size for each category in
#'   "caty.n" that will be applied to each stratum or a list of named numeric vectors with 
#'   the over sample size for each "caty.n" category for each stratum. List must be in same order
#'   as the "stratum" variable order. Default is NULL.
#'
#' @param n.near Numeric value specifying the number of nearby points to select as
#'   possible replacement sites if a site cannot be sampled. Default is NULL. If specified,
#'   must be integer from 1 to 10.
#'  
#' @param stratum_var Character string containing the name of the column from
#'   sframe that identifies stratum membership for each element in the frame.
#'   If stratum equals NULL, the design is unstratified and all elements in sample frame
#'   are eligible to be selected in the sample. The default is NULL.
#'
#' @param caty_var Character string containing the name of the column from
#'   sframe that identifies the unequal probability category for each element
#'   in the frame.  Default is NULL.
#'
#' @param aux_var Character string that is the name of the column from sframe that
#'   identifies the auxiliary variable value for each element in the sample frame
#'   that will be used to calculate inclusion probabilities when the survey design
#'   specifies that the selection type (seltype) is "proportional". Default is NULL.
#' 
#' @param legacy_var Character value for name of column for legacy site variable.
#'   Default is NULL.
#' 
#' @param mindis Numeric value for the minimum distance required between elements
#'   in the sample. If design is stratified, then mindis applies only within each stratum.
#'   Units must be the same units as in sf geometry. Default is NULL.  
#'
#' @param DesignID Name for the design, which is used to create a site ID for
#'   each site.  Default is "Site".
#'
#' @param SiteBegin Number to use for first site in the design.  Default is 1.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance (mindis) between sites.
#'   Default is 10.
#'   
#'
#' @return Nothing is returned. If errors are found they are collected and written out.
#'   One or more errors will cause the call to grts to stop.
#'
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @export
#################################################################################

dsgn_check <- function(sframe, sf_type, legacy_sites, legacy_option, stratum, seltype, n.samp, caty.n,
                       n.over, n.near, stratum_var,  caty_var, aux_var, legacy_var, mindis, 
                       DesignID, SiteBegin, maxtry) {

  # Create a data frame for stop messages
  stop.ind <- FALSE
  stop.df <- NULL

  # check that sframe has required variables for stratum, caty, aux and legacy
  # If stratum_var is provided, does the attribute exist in sframe
  if(!is.null(stratum_var)) {
    if(match(stratum_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for stratum variable does not exist as a variable in sframe."
      stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
    }
  }

  # If caty_var is provided, does the attribute exist in sframe
  if(!is.null(caty_var)) {
    if(match(caty_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for unequal probability category variable does not exist as a variable in sframe."
      stop.df <- rbind(stop.df, data.frame(func = I("caty_var"), I(stop.mess)))
    }
  }

  # If aux_var is provided, does the attribute exist in sframe
  if(!is.null(aux_var)) {
    if(match(aux_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for the auxillary variable for proportional sampling does not exist as a variable in sframe."
      stop.df <- rbind(stop.df, data.frame(func = I("aux_var"), I(stop.mess)))
    }
    # ensure class for aux variable is numeric
    if(!is.numeric (sframe[[aux_var]])) {
      stop.ind <- TRUE
      stop.mess <- "The auxillary variable in sample frame for proportional sampling must be numeric."
      stop.df <- rbind(stop.df, data.frame(func = I("aux_var"), I(stop.mess)))
    } else {
      # check that values are > 0.
      if(any (sframe[[aux_var]] <= 0)) {
        stop.ind <- TRUE
        stop.mess <- "The auxillary variable for proportional sampling must have all values greater than zero"
        stop.df <- rbind(stop.df, data.frame(func = I("aux_var"), I(stop.mess)))
      }
    }
  }

  # If legacy_var is provided, does the attribute exist in sframe
  if(sf_type == "point" & !is.null(legacy_var)) {
    if(match(legacy_var, names(sframe), nomatch=0) == 0) {
      stop.ind <- TRUE
      stop.mess <- "The value provided for the variable identifying legacy sites does not exist as a variable in sframe."
      stop.df <- rbind(stop.df, data.frame(func = I("legacy_var"), I(stop.mess)))
    }
  }
  
  ### Check legacy_sites sf object if present
  if(sf_type %in% c("linear", "area")){
    # check that legacy_sites has required variables for stratum, caty, aux and legacy
    # If stratum_var is provided, does the attribute exist
    if(!is.null(stratum_var)) {
      if(match(stratum_var, names(legacy_sites), nomatch=0) == 0) {
        stop.ind <- TRUE
        stop.mess <- "The value provided for stratum variable does not exist as a variable in legacy_sites."
        stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
      }
    }
    # If caty_var is provided, does the attribute exist
    if(!is.null(caty_var)) {
      if(match(caty_var, names(legacy_sites), nomatch=0) == 0) {
        stop.ind <- TRUE
        stop.mess <- "The value provided for caty variable does not exist as a variable in legacy_sites."
        stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
      }
    }
    # If aux_var is provided, does the attribute exist
    if(!is.null(aux_var)) {
      if(match(aux_var, names(legacy_sites), nomatch=0) == 0) {
        stop.ind <- TRUE
        stop.mess <- "The value provided for aux variable does not exist as a variable in legacy_sites."
        stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
      }
    }
    # If legacy_var is provided, does the attribute exist
    if(!is.null(legacy_var)) {
      if(match(legacy_var, names(legacy_sites), nomatch=0) == 0) {
        stop.ind <- TRUE
        stop.mess <- "The value provided for legacy variable does not exist as a variable in legacy_sites."
        stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
      }
    }
  }


  ##### Check design components to ensure they provide what is required.
  
  # check if stratum is provided and values are in sframe
  if(!is.null(stratum)) {
    if(is.null(stratum_var)){
      stop.ind <- TRUE
      stop.mess <- "Design is stratified and no 'stratum_var' is provided."
      stop.df <- rbind(stop.df, data.frame(func = I("stratum_var"), I(stop.mess)))
    } else {
      if(any(stratum %in% unique(sframe[[stratum_var]]) == FALSE)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Not all stratum values are in sample frame.")
        stop.df <- rbind(stop.df, data.frame(func = I("stratum"), I(stop.mess)))
      }
    }
  }
  
  # check seltype
  if(any(seltype %in% c("equal", "unequal", "proportional") == FALSE)) {
    stop.ind <- TRUE
    stop.mess <- paste0("seltype must be 'equal', 'unequal' or 'proportional'.")
    stop.df <- rbind(stop.df, data.frame(func = I("seltype"), I(stop.mess)))
  }
  
  # check n.samp
  if(any(n.samp <= 0)) {
    stop.ind <- TRUE
    stop.mess <- paste0("Sample size must be integers greater than 0.")
    stop.df <- rbind(stop.df, data.frame(func = I("n.samp"), I(stop.mess)))
  }
  
  # check caty.n
  if(!is.null(caty.n)) {
    if(!is.list(caty.n)) {
      if(any(names(caty.n) %in% unique(sframe[[caty_var]]) == FALSE)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Not all caty.n values are in sample frame.")
        stop.df <- rbind(stop.df, data.frame(func = I("caty.n"), I(stop.mess)))
      }
      tst <- function(x, caty.n){x != sum(caty.n)}
      if(any(sapply(n.samp,tst, caty.n)) ) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Sum of caty.n values do not equal n.samp.")
        stop.df <- rbind(stop.df, data.frame(func = I("caty.n"), I(stop.mess)))
      }
    }
    if(is.list(caty.n)) {
      if(any(names(caty.n) %in% stratum == FALSE)) {
        stop.ind <-  TRUE
        stop.mess <- paste0("Names for caty.n list are not values in 'stratum' variable.")
        stop.df <- rbind(stop.df, data.frame(func = I("caty.n"), I(stop.mess)))
        stop.mess <- paste0("For each stratum make sure caty.n values in 'caty_var' variable.")
        stop.df <- rbind(stop.df, data.frame(func = I("caty.n"), I(stop.mess)))
      }
    }
  }

  # check n.over
  if(!is.null(n.over)){
    if(is.null(stratum) | length(stratum) == 1) {
      if( any(seltype %in% c("equal", "proportional"))) {
        if(n.over < 0){
          stop.ind <-  TRUE
          stop.mess <- paste0("n.over value must be zero or positive.")
          stop.df <- rbind(stop.df, data.frame(func = I("n.over"), I(stop.mess)))
        }
      }
      if(any(seltype == "unequal")){
        if(!is.null(caty.n)) {
          if(!is.list(n.over)) {
            if(any(n.over < 0)) {
              stop.ind <-  TRUE
              stop.mess <- paste0("n.over values must be zero or positive.")
              stop.df <- rbind(stop.df, data.frame(func = I("n.over"), I(stop.mess)))
            }
          }
        }
      }
    }
    if(length(stratum) > 1) {
      if( any(seltype %in% c("equal", "proportional", "unequal"))) {
        if(!is.list(n.over)) {
          if(any(n.over < 0)) {
            stop.ind <-  TRUE
            stop.mess <- paste0("n.over values must be zero or positive.")
            stop.df <- rbind(stop.df, data.frame(func = I("n.over"), I(stop.mess)))
          }
        }
        if(is.list(n.over)) {
          if(any(names(n.over) %in% stratum == FALSE)) {
            stop.ind <-  TRUE
            stop.mess <- paste0("Names for n.over list are not values in 'stratum' variable.")
            stop.df <- rbind(stop.df, data.frame(func = I("n.over"), I(stop.mess)))
            stop.mess <- paste0("For each stratum make sure n.over values are non-negative.")
            stop.df <- rbind(stop.df, data.frame(func = I("n.over"), I(stop.mess)))
          }
        }
      }
    }
  }


  # check n.near
  if(!is.null(n.near)) {
    if(!(any(n.near %in% 1:10))) {
      stop.ind <-  TRUE
      stop.mess <- paste0("n.near must be from 1 to 10.\n")
      stop.df <- rbind(stop.df, data.frame(func = I("n.near"), I(stop.mess)))
    }
  }
  

  ### If any issues, write out stop.df and then stop
  if(stop.ind) {
    names(stop.df) <- c("Design Input", "Error Message")
    stop.df <<- stop.df
    cat("During the check of the input to grtspts, one or more errors were identified.\n")
    cat("Enter the following command to view all input error messages: stopprnt()\n")
    cat("To view a subset of the errors (e.g., errors 1 and 5) enter stopprnt(m=c(1,5))\n\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()

   }

}



