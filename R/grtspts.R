###################################################################################
# Function: grtspts
# Programmers: Tony Olsen, Tom Kincaid
# Date: June 18, 2020
#'
#' Select a spatially balanced sample from a finite population using generalized random tessalation
#' stratified algorithm from a point sample frame based on a survey design specification.
#'
#'
#' @param sframe Sample frame for points as an sf object. If the design is stratified,
#'   unequal probability, proportional probability or has legacy sites, then sample frame 
#'   must include variables that identify the stratum; category, auxillary and legacy variables
#'   for unequal selection; or that identify elements that are legacy sites. 
#'   The coordinate system for sframe must be one where distance for coordinates is meaningful.
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
#' @param nsamp The sample size required. If single stratum, then single numeric value.
#'   If sample is stratified, then numeric vector with same length as "stratum" and sample sizes
#'   required in same order as strata in "stratum". Must be specified.
#' 
#' @param caty.n If seltype is "unequal", a named character vector with the expected sample size
#'   for each category specified in variable caty_var. If design is stratified, then either
#'   a named character vector with the expected sample size for each category where categories 
#'   are the same for all strata or a list of character vectors with the expected sample size.
#'   List must be in same order as the "stratum" variable. For each stratum, the sum of
#'   caty.n values must equal nsamp for that stratum. Default is NULL.
#'   
#' @param over.n Numeric value specifying the over sample size requested. Default in NULL.
#'   If design is stratified, then either a named character vector with the over sample size 
#'   for each category where categories are the same for all strata or a list of character 
#'   vectors with the over sample size. List must be in same order as the "stratum" variable.
#'   Default is NULL.
#'
#' @param over.near Numeric value specifying the number of nearby points to select as
#'   possible replacement sites if a site cannot be sampled. Default is NULL. If specified,
#'   possible values are 1, 2 or 3.
#'   
#' @param panel To be defined. Default is NULL.
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
#' @param legacy_var Character string that is the name of the logical variable in sframe that
#'   identifies which elements in the sample frame are legacy elements. The logical
#'   variable equals TRUE if element is a legacy site and FALSE otherwise. Default is NULL.
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
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'
#' @param startlev Initial number of hierarchical levels to use for the GRTS
#'   grid, which must be less than or equal to maxlev (if maxlev is specified)
#'   and cannot be greater than 11.  The default is NULL.
#'
#' @param maxlev Maxmum number of hierarchical levels to use for the GRTS
#'   grid, which cannot be greater than 11.  The default is 11.
#'
#'
#' @return sites A list of three sf objects containing the base sites (sites.base),
#'   the over.n sites (sites.over) and the over.near sites (sites.near) selected 
#'   that meet the survey design requirements.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{{\link{junk}}}{What it does}
#'     }
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#'   test.sample <- grts(dsgn=test_design, sframe = "test_sf" DesignID="TestSite",
#'     stratum="test_stratum", mdcaty="test_mdcaty")
#' }
#'
#' @export
#################################################################################

grtspts <- function(sframe, stratum = NULL, seltype = "equal", nsamp, caty.n = NULL, 
                     over.n = NULL, over.near = NULL, panel = NULL, stratum_var = NULL, 
                     caty_var = NULL, aux_var = NULL, legacy_var = NULL, mindis = NULL, 
                     DesignID = "Site", SiteBegin = 1,  maxtry = 10, 
                     startlev = NULL, maxlev = 11) {


  # check input. If errors, dsgn_check will stop grtspts and report errors.
#  dsgn_check(sframe, seltype, nsamp, caty.n, over.n, over.near, panel, stratum_var, 
#             caty_var, aux_var, legacy_var, mindis, DesignID, SiteBegin, maxtry,
#             startlev, maxlev)

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn.ind <- FALSE
  warn.df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")

  ## Create variables in sample frame if needed.
  # Create unique ID values and save variable names in sample frame provided on input
  sframe$id <- 1:nrow(sframe)
  geom_name <- attr(sframe, "sf_column")
  names.sframe <- names(sframe)[names(sframe) != geom_name]
  
  # Assign stratum variable or create it if design not stratified and variable not provided.
  if(is.null(stratum_var)) {
    stratum_var <- "stratum"
    sframe$stratum <- "None"
    stratum <- c("None")
  } else {
    sframe$stratum <- sframe[[stratum_var]]
  }

  # set caty, aux and legacy variables in sample frame if needed
  if(!is.null(caty_var)) sframe$caty <- sframe[[caty_var]]
  if(!is.null(aux_var)) sframe$aux <- sframe[[aux_var]]
  if(!is.null(legacy_var)) sframe$legacy <- sframe[[legacy_var]]
  
  ## Create a dsgn list object
  # variable assignments to dsgn list object
  dsgn <- list(stratum_var = stratum_var, caty_var = caty_var, aux_var = aux_var,
               legacy_var = legacy_var)
  
  # stratum
  dsgn$stratum <- stratum
  
  # seltype
  if(length(seltype) == length(stratum)) {
    dsgn$seltype <- seltype
    names(dsgn$seltype) <- stratum
  } else {
    tmp <- lapply(stratum, function(x, seltype) { x = seltype}, seltype)
    names(tmp) <- stratum
    dsgn$seltype <- tmp
  }
  
  # nsamp
  if(length(nsamp) == length(stratum)) {
    dsgn$nsamp <- nsamp
    names(dsgn$nsamp) <- stratum
  } else {
    tmp <- lapply(stratum, function(x, nsamp) { x = nsamp}, nsamp)
    names(tmp) <- stratum
    dsgn$nsamp <- tmp
  }
  
  # caty.n
  if(is.list(caty.n)) {
    dsgn$caty.n <- caty.n
  } else {
    tmp <- lapply(stratum, function(x, caty.n) { x = caty.n}, caty.n)
    names(tmp) <- stratum
    dsgn$caty.n <- tmp
  }
  
  # over.n
  if(is.list(over.n)) {
    dsgn$over.n <- over.n
  } else {
    tmp <- lapply(stratum, function(x, over.n) { x = over.n}, over.n)
    names(tmp) <- stratum
    dsgn$over.n <- tmp
  }
  
  # over.near
  tmp <- lapply(stratum, function(x, over.near) { x = over.near}, over.near)
  names(tmp) <- stratum
  dsgn$over.near <- tmp
  
  # panel
  dsgn$panel <- panel
  
  # mindis
  dsgn$mindis <- mindis
  
  ## select sites for each stratum
  rslts <- lapply(dsgn$stratum, grtspts_stratum, dsgn, sframe, maxtry = maxtry,
                  startlev = startlev, maxlev = 11, warn.ind, warn.df)
  names(rslts) <- stratum

  # combine across strata
  sites.base <- NULL
  sites.over <- NULL
  sites.near <- NULL
  warn.ind <- FALSE
  warn.df <- NULL
  for (i in 1:length(rslts)) {
    sites.base <- rbind(sites.base, rslts[[i]]$sites.base)
    sites.over <- rbind(sites.over, rslts[[i]]$sites.over)
    sites.near <- rbind(sites.near, rslts[[i]]$sites.near)
    if(rslts[[i]]$warn.ind) {
      warn.ind <- TRUE
      warn.df <- rbind(warn.df, rslts[[i]]$warn.df)
    }
  }
 
  # Create siteID for base sites using DesignID and SiteBegin
  sites.base$siteID <- gsub(" ","0", paste0(DesignID,"-",
                                   format(SiteBegin - 1 + 1:nrow(sites.base), sep="")))
  # create siteID for over.n sites if any
  if(!is.null(over.n)) {
  sites.over$siteID <- gsub(" ","0", paste0(DesignID,"-", 
                                            format(nrow(sites.base) + 1:nrow(sites.over), 
                                                   sep="")))
  }
  # create siteID for over.near sites if any
  if(!is.null(over.near)) {
    ntmp <- sum(nrow(sites.base), nrow(sites.over), na.rm = TRUE)
    sites.near$siteID <- gsub(" ","0", paste0(DesignID,"-", 
                                              format(ntmp + 1:nrow(sites.near), sep="")))
  }
  
  # if over.near sample sites, assign base siteIDs to the replacement sites
  if(!is.null(over.near)) {
    tst <- match(sites.near$replsite, sites.base$id, nomatch = 0)
    sites.near$replsite[tst > 0] <- sites.base$siteID[tst]
    tst <- match(sites.near$replsite, sites.over$id, nomatch = 0)
    sites.near$replsite[tst > 0] <- sites.over$siteID[tst]
  }
  
  sites <- list(sites.base = sites.base, sites.over = sites.over, sites.near = sites.near)
  
  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if(warn.ind) {
    warn.df <<- warn.df
    if(nrow(warn.df) == 1){
      cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn.df'.  Enter the following command \nto view the warning message: warnprnt()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn.df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn.df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # return the survey design sf object
  invisible(sites)
}


