###################################################################################
# Function: grtspts_stratum
# Programmers: Tony Olsen
# Date: June 18, 2020
#'
#' For a single stratum, select a spatially balanced sample from a finite population using 
#' generalized random tessalation stratified algorithm from a point sample frame based on a 
#' survey design specification.
#'
#' @param stratum Character value for the stratum name.
#' 
#' @param dsgn List of componenents that specify the survey design. Includes all strata.
#'   See grtspts for contents of dsgn.
#' 
#' @param sframe Sample frame for points as an sf object. If the design is stratified,
#'   unequal probability, proportional probability or has legacy sites, then sample frame 
#'   must include variables that identify the stratum; category, auxillary and legacy variables
#'   for unequal selection; or that identify elements that are legacy sites. 
#'   The coordinate system for sframe must be one where distance for coordinates is meaningful.
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
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'   
#' @return rslts A list consisting of an sf object for base sites, an sf object of 
#'   over.n sites (NULL if none) an sf object of over.near sites (NULL if none) where
#'   the sf objects containing the sites selected that meet the survey design requirements 
#'   and warn.ind - logical value for warning indicator and warn.df - a data.frame
#'   for warning messages.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{\link{grtspts_ip}}}{Calculates inclusion probabilities}
#'     \item{\code{\link{grtspts_ipleg}}}{Calculates inclusion probabilities with legacy points}
#'     \item{\code{\link{numLevels}}}{calculates the number of levels for a generalized 
#'       random-tesselation stratified (GRTS) Survey Design}
#'     \item{\code{\link{cellWeight}}}{calculates total inclusion probability
#'       for each cell in a grid}
#'     \item{\code{\link{grtspts_select2}}}{selects a grts sample}
#'     \item{\code{\link{grtspts_mindis}}}{selects a grts sample with minimum distance between
#'       points}
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

grtspts_stratum <- function(stratum, dsgn, sframe, maxtry = 10, startlev = NULL, maxlev = 11,
                            warn.ind = FALSE, warn.df = NULL) {
  
  # subset sframe to s stratum
  sftmp <- sframe[sframe$stratum == stratum, ]
  # Determine number of elements in stratum
  Nstratum <- nrow(sftmp)
  
  # detemine overall sample size required from dsgn for stratum
  # account for over.n sample option if present
  n.total <- dsgn[["nsamp"]][[stratum]] + sum(dsgn[["over.n"]][[stratum]], na.rm = TRUE)
  if(dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    n.caty <- n.total
  } else {
    ifelse(is.null(dsgn[["over.n"]][[stratum]]), n.caty <- dsgn[["caty.n"]][[stratum]],
           n.caty <- dsgn[["caty.n"]][[stratum]] + dsgn[["over.n"]][[stratum]])
  }
  
  # If seltype is "equal" or "proportional", set caty to same as stratum
  if(dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    sftmp$caty <- sftmp$stratum
  }
  
  # compute inclusion probabilities

  ip <- grtspts_ip(type = dsgn[["seltype"]][stratum], nsamp = n.caty,
                   Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
                   warn.ind = warn.ind,  warn.df = warn.df)
  # save initial inclusion probabilities
  sftmp$ip_init <- ip$ip
  sftmp$ip <- ip$ip
  # accumulate warning messages if any
  if(ip$warn.ind) {
    warn.ind <- ip$warn.ind
    warn.df <- ip$warn.df
    warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
  }
  

  
  # Create hierarchical grid based on number of levels required
  grts_grid <- numLevels(n.total, sftmp, startlev, maxlev,
                         warn.ind = warn.ind,  warn.df = warn.df)
  if(grts_grid$warn.ind) {
    warn.ind <- grts_grid$warn.ind
    warn.df <- grts_grid$warn.df
    warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
  }
  
  # If legacy sites, adjust inclusion probabilities adjust cell weights to use 
  # legacy inclusion probabilities
  if(!is.null(dsgn[["legacy_var"]])) {
    sftmp$ip <- grtspts_ipleg(sftmp$ip_init, sftmp$legacy)
    # accumulate warning messages if any
    if(ip$warn.ind) {
      warn.ind <- ip$warn.ind
      warn.df <- ip$warn.df
      warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
      }
    grts_grid$cel.wt <- cellWeight(grts_grid$xc, grts_grid$yc, 
                                   grts_grid$dx, grts_grid$dy, sftmp)
  }
  
  # select sites if no minimum distance between sites
  if(is.null(dsgn[["mindis"]])) {
    sites <- grtspts_select(sftmp, grts_grid, samplesize = n.total, 
                             over.near = dsgn[["over.near"]][[stratum]],
                             warn.ind = warn.ind, warn.df = warn.df)
  }
  # If minimum distance between sites, select sites
  if(!is.null(dsgn[["mindis"]])) {
    sites <- grtspts_mindis(dsgn[["mindis"]], sftmp, grts_grid, samplesize = n.total, 
                            over = dsgn[["over.near"]][[stratum]],
                            stratum = stratum, legacy_var = dsgn[["legacy_var"]],
                            maxtry = maxtry, warn.ind = warn.ind, warn.df = warn.df)
  }
  # check for warning messages
  warn.ind <- sites$warn.ind
  warn.df <- sites$warn.df
  if(warn.ind) {
      warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
  }
  
  # adjust inclusion probabilities when over sample sites present
  n.base <- dsgn[["nsamp"]][[stratum]]
  sites[["sites.base"]]$ip <- sites[["sites.base"]]$ip * n.total / n.base
  # save base sites
  sites.base <- sites$sites.base[1:n.base,]
  # save over.n sample sites
  sites.over <- NULL
  if(!is.null(dsgn[["over.n"]][[stratum]])) {
    sites.over <- sites$sites.base[(n.base + 1):n.total,]
  }
  # save over.near sites
  sites.near <- NULL
  if(!is.null(dsgn[["over.near"]][[stratum]])) {
    sites.near <- sites$sites.near
  }
  
  # Assign original inclusion probabilites to sites, create weights and drop legacy ip variable
  sites.base$ip <- sites.base$ip_init
  sites.base$wgt <- 1/sites.base$ip
  tmp <- names(sites.base)
  sites.base <- subset(sites.base, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  
  # Do same for sites.over if any
  if(!is.null(dsgn[["over.n"]][[stratum]])) {
    sites.over$ip <- sites.over$ip_init
    sites.over$wgt <- 1/sites.over$ip
    tmp <- names(sites.over)
    sites.over <- subset(sites.over, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  }
  
  # Do same for sites.near if any
  if(!is.null(dsgn[["over.near"]][[stratum]])) {
    sites.near$ip <- sites.near$ip_init
    sites.near$wgt <- 1/sites.near$ip
    tmp <- names(sites.near)
    sites.near <- subset(sites.near, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  }
  
  rslts <- list(sites.base = sites.base, sites.over = sites.over, sites.near = sites.near,
                warn.ind = warn.ind, warn.df = warn.df)
  
  invisible(rslts)
}


  

