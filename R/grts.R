###################################################################################
# Function: grts
# Programmers: Tony Olsen, Tom Kincaid
# Date: September 22, 2020
#'
#' Select a spatially balanced sample from a finite, linear or area population using 
#' generalized random tessalation stratified algorithm  from a sample frame based on
#' a survey design specification. The survey design may be stratified and within each stratum
#' may be equal probability, unequal probability by categories or unequal probabilty
#' proportional to an auxillary variable. Additional options include selecting additional 
#' sites as an "over" sample, selecting nearby replacement sites, including legacy sites, 
#' and requiring minimum distance between sites.
#'
#'
#' @param sframe Sample frame as an sf object. If the design is stratified, unequal probability 
#'   or proportional probability, then sample frame must include variables that identify the 
#'   stratum; and the category or auxillary variables for unequal selection. The coordinate 
#'   system for sframe must be one where distance for coordinates is meaningful. If legacy sites
#'   are included in a finite sample frame, then a legacy variable must be provided to
#'   identify elements that are legacy sites.
#'    
#' @param nsamp The sample size required. If single stratum, then single numeric value.
#'   If sample is stratified, then numeric vector with same length as "stratum" and sample sizes
#'   required in same order as strata in "stratum". Must be specified.
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
#' @param pt_density For linear and area sample frame, the point density for the systematic
#'   sample. Must be in units of the sframe sf.object. Default is NULL.
#'
#' @param caty.n If design is not stratified and seltype is "unequal", a named character vector
#'   with the expected sample size for each category specified in variable caty_var. If design
#'   is stratified, then either a named character vector with the expected sample size for each
#'   category for all strata or if the expected sample size for each category may differ, then
#'   a list of named character vectors with the expected sample size for each category in the
#'   stratum. The list must be in same order as the "stratum" variable. For each stratum, 
#'   the sum of caty.n values must equal nsamp for that stratum. Default is NULL.
#'   
#' @param over.n If seltype is "equal" and is not stratified, a numeric value specifying the 
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
#' @param over.near Numeric value specifying the number of nearby points to select as
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
#' @param legacy_option Logical variable that when TRUE legacy sites are to be included 
#'   in the survey design. 
#'   
#' @param legacy_sites sf object of legacy sites to be included in the survey design. Note
#'   legacy_option must be equal to TRUE. For finite populations, legacy sites may be
#'   included in the sample frame where they are identified by the legacy variable. For linear
#'   and area sample frames, a legacy_sites sf object must be provided. For finite populations
#'   legacy sites may either be included in sample frame with a legacy variable or as a 
#'   separate sf point object. The latter will not guarantee that the sites selected will be
#'   unique, i.e., a legacy site may be included twice.
#'   
#' @param legacy_var For finite sample frames when legacy sites are included with the sample
#'   frame, a character string that is the name of the logical variable in sframe that
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
#' @param maxtry Number of maximum attempts to ensure minimum distance (mindis) between sites.
#'   Default is 10.
#'
#'
#' @return sites A list of three sf objects containing the base sites (sites.base),
#'   the over.n sites (sites.over) and the over.near sites (sites.near) selected 
#'   that meet the survey design requirementse plus a design list object that documents
#'   the survey design used.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{{\link{junk}}}{What it does}
#'     }
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey design
#'
#' @examples
#' \dontrun{
#'   test.sample <- grts(dsgn=test_design, sframe = "test_sf" DesignID="TestSite",
#'     stratum="test_stratum", mdcaty="test_mdcaty")
#' }
#'
#' @export
#################################################################################

grts <- function(sframe, nsamp, stratum = NULL, seltype = "equal", pt_density = NULL,
                 caty.n = NULL, over.n = NULL, over.near = NULL, stratum_var = NULL, 
                 caty_var = NULL, aux_var = NULL, legacy_option = FALSE,
                 legacy.sites = NULL, legacy_var = NULL, mindis = NULL, 
                 DesignID = "Site", SiteBegin = 1,  maxtry = 10) {
  
  # Ensure that the geometry types for sframe are consistent
  
  temp <- st_geometry_type(sframe)
  tst <- all(temp %in% c("POINT", "MULTIPOINT")) |
    all(temp %in% c("LINESTRING", "MULTILINESTRING")) |
    all(temp %in% c("POLYGON", "MULTIPOLYGON"))
  if(!tst) {
    stop(paste("\nThe geometry types for the survey frame object passed to function grts: \n\"", unique(st_geometry_type(sf.object)), "\" are not consistent.", sep=""))
  }

  # Determine type of sample frame: point, line, polygon
  if(all(temp %in% c("POINT", "MULTIPOINT"))) sf_type <- "sf_point"
  if(all(temp %in% c("LINESTRING", "MULTILINESTRING"))) sf_type <- "sf_linear"
  if(all(temp %in% c("POLYGON", "MULTIPOLYGON"))) sf_type <- "sf_area"
  
  # check input. If errors, dsgn_check will stop grtspts and report errors.
  dsgn_check(sframe, stratum, seltype, nsamp, caty.n, over.n, over.near, stratum_var, 
             caty_var, aux_var, legacy_var, mindis, DesignID, SiteBegin, maxtry,
             startlev, maxlev)

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn.ind <- FALSE
  warn.df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")
  
  # preserve original sframe names
  sframe.names <- names(sframe)

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
    # ensure class for stratum variable is character and assign to stratum
    sframe$stratum <- as.character(sframe[[stratum_var]])
  }

  # set caty, aux and legacy variables in sample frame if needed
  if(!is.null(caty_var)) sframe$caty <- as.character(sframe[[caty_var]])
  if(!is.null(aux_var)) sframe$aux <- sframe[[aux_var]]
  if(!is.null(legacy_var)) sframe$legacy <- sframe[[legacy_var]]
  
  ## Create a dsgn list object
  # variable assignments to dsgn list object
  dsgn <- list(stratum_var = stratum_var, caty_var = caty_var, aux_var = aux_var,
               legacy_var = legacy_var, stratum = stratum, seltype = NULL,
               nsamp = NULL, caty.n = NULL, over.n = NULL, over.near = NULL, mindis = mindis)
  
  # seltype
  if(length(seltype) == length(stratum)) {
    dsgn$seltype <- seltype
    names(dsgn$seltype) <- stratum
  } else {
    tmp <- sapply(stratum, function(x, seltype) { x = seltype}, seltype)
    names(tmp) <- stratum
    dsgn$seltype <- tmp
  }
  
  # nsamp
  if(length(nsamp) == length(stratum)) {
    dsgn$nsamp <- nsamp
    names(dsgn$nsamp) <- stratum
  } else {
    tmp <- sapply(stratum, function(x, nsamp) { x = nsamp}, nsamp)
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
  if(!is.null(over.n)) {
    if(is.list(over.n)) {
      dsgn$over.n <- over.n
    } else {
      tmp <- lapply(stratum, function(x, over.n) { x = over.n}, over.n)
      names(tmp) <- stratum
      dsgn$over.n <- tmp
    }
  }
  
  # over.near
  if(!is.null(over.near)) {
    tmp <- sapply(stratum, function(x, over.near) { x = over.near}, over.near)
    names(tmp) <- stratum
    dsgn$over.near <- tmp
  }
  
  ## select sites for each stratum
  rslts <- lapply(dsgn$stratum, grts_stratum, dsgn = dsgn, sframe = sframe, sf_type = sf_type, 
                  pt_density = pt_density, maxtry = maxtry, warn.ind, warn.df)
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
    jnk <- max(nchar(sites.base$siteID))
    nlast <- max(as.numeric(substr(sites.base$siteID, nchar(DesignID)+2, jnk)))
    sites.over$siteID <- gsub(" ","0", 
                              paste0(DesignID,"-", format(nlast + 1:nrow(sites.over), sep="")))
  }
  
  # if over.near sample sites, assign base ids to the replacement sites. then add siteIDs
  if(!is.null(over.near)) {
    tst <- match(sites.near$replsite, sites.base$id, nomatch = 0)
    sites.near$replsite[tst > 0] <- sites.base$siteID[tst]
    tst <- match(sites.near$replsite, sites.over$id, nomatch = 0)
    sites.near$replsite[tst > 0] <- sites.over$siteID[tst]
    
    # sort by id so that sites.near in same order as sites in sites.base and sites.over
    sites.near <- sites.near[order(sites.near$replsite, sites.near$siteuse),]
    # assign siteIDs
    jnk <- max(nchar(sites.base$siteID), nchar(sites.over$siteID), na.rm = TRUE)
    nlast <- max(as.numeric(substr(sites.base$siteID, nchar(DesignID)+2, jnk)),
                 as.numeric(substr(sites.over$siteID, nchar(DesignID)+2, jnk)))
    sites.near$siteID <- gsub(" ","0", 
                              paste0(DesignID,"-", format(nlast + 1:nrow(sites.near), sep="")))
  }
  
  # reorder sf object variables by first specifying design names excluding unique 
  # feature ID id as it is internal
  dsgn.names <- c("siteID", "replsite", "siteuse", "stratum", "wgt", "ip", "caty", "aux",
                  "legacy")
  # check what design variables are present in sf objects and add if missing
  tmp.names <- names(sites.base)
  add.names <- dsgn.names[!(dsgn.names %in% tmp.names)]
  if( !is.null(add.names)) {
    tmp <- matrix(NA, nrow = nrow(sites.base), ncol = length(add.names), 
                  dimnames = list(NULL, add.names))
    sites.base <- cbind(sites.base, tmp)
    if(!is.null(sites.over)){
      tmp <- matrix(NA, nrow = nrow(sites.over), ncol = length(add.names), 
                  dimnames = list(NULL, add.names))
      sites.over <- cbind(sites.over, tmp)
    }
    if(!is.null(sites.near)){
      tmp <- matrix(NA, nrow = nrow(sites.near), ncol = length(add.names), 
                  dimnames = list(NULL, add.names))
      sites.near <- cbind(sites.near, tmp)
    }
  }
  # check if any dsgn.names occur in sframe names and drop in sframe names if duplicated
  sframe.names <- sframe.names[!(sframe.names %in% dsgn.names)]
  # use subset to reorder variables and drop internal variables and duplicated variables
  sites.base <- subset(sites.base, select = c(dsgn.names, sframe.names))
  if(!is.null(sites.over)) sites.over <- subset(sites.over, select = c(dsgn.names, sframe.names))
  if(!is.null(sites.near)) sites.near <- subset(sites.near, select = c(dsgn.names, sframe.names))
  
  # create output list
  sites <- list(sites.base = sites.base, sites.over = sites.over, sites.near = sites.near,
                dsgn = dsgn)
  
  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if(warn.ind) {
    warn.df <<- warn.df
    if(nrow(warn.df) == 1){
      cat("During execution of the program, a warning message was generated. The warning \nmessage is stored in a data frame named 'warn.df'.  Enter the following command \nto view the warning message: warndsg()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn.df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn.df'.  Enter the following \ncommand to view the warning messages: warndsgn() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # return the survey design sf object
  invisible(sites)
}


