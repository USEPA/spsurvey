###################################################################################
# Function: grts_stratum
# Programmers: Tony Olsen
# Date: September 8, 2020
#'
#' For a single stratum, select a spatially balanced sample using generalized random
#' tessalation stratified algorithm. For a point sample frame, the selection is a one-step
#' process. For linear and area sample frames, a two-step process is used. First a systematic
#' point sample is selected from either linear or area sample frame to create a dense point
#' sample. Then the grts algorithm is applied to the systematic point sample. The selection
#' in all cases is based on the survey design specification for the stratum.
#'
#' @param stratum Character value for the stratum name.
#' 
#' @param dsgn List of componenents that specify the survey design. Includes all strata.
#'   See grtspts for contents of dsgn.
#' 
#' @param sframe Sample frame as an sf object. If the design is stratified,
#'   unequal probability, proportional probability or has legacy sites, then sample frame 
#'   must include variables that identify the stratum; category, auxillary and legacy variables
#'   for unequal selection; or that identify elements that are legacy sites. 
#'   The coordinate system for sframe must be one where distance for coordinates is meaningful.
#'
#' @param sf_type The sample frame geometry type: point, linear or area
#' 
#' @param pt_density For linear and area sample frame, the point density for the systematic
#'   sample. Must be in units of the sframe sf.object. Default is NULL.
#' 
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
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
#'     \item{\code{\link{get.address}}}{Creates hierarchical order for points}
#'     \item{\code{\link{UPpivotal}}}{Selects sample using pivotal method}
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
#'   test.sample <- grts(stratum, dsgn=test_design, sframe = "test_sf", sf_type = "point")
#' }
#'
#' @export
#################################################################################

grts_stratum <- function(stratum, dsgn, sframe, sf_type, pt_density = NULL, maxtry = 10,
                            warn.ind = FALSE, warn.df = NULL) {
  
  # subset sframe to s stratum
  sftmp <- sframe[sframe$stratum == stratum, ]
  
  # sf_type equals point
  if(sf_type == "sf_point") {
    ip_step1 <- 1
    sftmp$xcoord <- st_coordinates(sftmp)[,"X"]
    sftmp$ycoord <- st_coordinates(sftmp)[,"Y"]
  }
  
  # sf_type equals linear
  if(sf_type == "sf_linear") {
    # determine sample size from pt_density and total length of sample frame in stratum
    stratum_len <- sum(st_length(sftmp))
    n_size <- as.integer(pt_density * stratum_len)
    sfpts <- st_sample(sftmp, size = n_size, type = 'regular')
    sfpts <- st_as_sf(as_tibble(sfpts), crs = st_crs(sftmp))
    # drop features with no points
    # sfpts <- sfpts[!st_is_empty(sfpts),, drop = FALSE]
    # join sites with linear features
    sftmp <- st_join(sfpts, sftmp, join = st_nearest_feature)
    sftmp$xcoord <- st_coordinates(sftmp)[,"X"]
    sftmp$ycoord <- st_coordinates(sftmp)[,"Y"]
    # drop features with no points
    sftmp <- sftmp[!st_is_empty(sftmp),]
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp)/stratum_len
  }
  
  
  # sf_type equals area
  if(sf_type == "sf_area") {
    # determine sample size from pt_density and total area of sample frame in stratum
    stratum_area <- sum(st_area(sftmp))
    n_size <- as.integer(pt_density * stratum_area)
    sfpts <- st_sample(sftmp, size = n_size, type = 'hexagonal')
    sfpts <- st_as_sf(as_tibble(sfpts), crs = st_crs(sftmp))
    sftmp <- st_join(sfpts, sftmp)
    names(sftmp)[names(sftmp) == "sfpts"] <- "geometry"
    sftmp$xcoord <- st_coordinates(sftmp)[,"X"]
    sftmp$ycoord <- st_coordinates(sftmp)[,"Y"]
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp)/stratum_area
  }
  
  # Determine number of elements in stratum
  Nstratum <- nrow(sftmp)
  
  # Step 2 site selection if linear or area; otherwise Step 1 for points.
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
  
  # If legacy sites, adjust inclusion probabilities to use 
  # legacy inclusion probabilities
  if(!is.null(dsgn[["legacy_var"]])) {
    sftmp$ip <- grtspts_ipleg(sftmp$ip_init, sftmp$legacy)
    # accumulate warning messages if any
    if(ip$warn.ind) {
      warn.ind <- ip$warn.ind
      warn.df <- ip$warn.df
      warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
      }
  }
  
  # select sites if no minimum distance between sites
  if(is.null(dsgn[["mindis"]])) {
    sites <- sftmp[get_address(sftmp$xcoord, sftmp$ycoord, rand = TRUE), ]
    s <- UPpivotal(sites$ip)
    sites <- sites[round(s) == 1, ]
    sites <- rho(sites)
    sites$siteuse <- NA
    sites$replsite <- NA
    sites <- list(sites.base = sites, warn.ind = warn.ind, warn.df = warn.df)

  }
  # If minimum distance between sites, select sites
  if(!is.null(dsgn[["mindis"]])) {
    sites <- grtspts_mindis(dsgn[["mindis"]], sftmp, grts_grid, samplesize = n.total, 
                            over.near = dsgn[["over.near"]][[stratum]],
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
    sites.over$siteuse <- "Over"
  }
  # save over.near sites
  sites.near <- NULL
  if(!is.null(dsgn[["over.near"]][[stratum]])) {
    sites.near <- sites$sites.near
  }
  
  # Assign original inclusion probabilites to sites, create weights and drop legacy ip variable
  sites.base$ip <- sites.base$ip_init * ip_step1
  sites.base$wgt <- 1/sites.base$ip
  tmp <- names(sites.base)
  sites.base <- subset(sites.base, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  
  # Do same for sites.over if any
  if(!is.null(dsgn[["over.n"]][[stratum]])) {
    sites.over$ip <- sites.over$ip_init * ip_step1
    sites.over$wgt <- 1/sites.over$ip
    tmp <- names(sites.over)
    sites.over <- subset(sites.over, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  }
  
  # Do same for sites.near if any
  if(!is.null(dsgn[["over.near"]][[stratum]])) {
    sites.near$ip <- sites.near$ip_init * ip_step1
    sites.near$wgt <- 1/sites.near$ip
    tmp <- names(sites.near)
    sites.near <- subset(sites.near, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
  }
  
  rslts <- list(sites.base = sites.base, sites.over = sites.over, sites.near = sites.near,
                warn.ind = warn.ind, warn.df = warn.df)
  
  invisible(rslts)
}


  

