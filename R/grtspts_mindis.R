################################################################################
# Function: grtspts_mindis
# Programmer:  Tony Olsen
# Date: September 28, 2020
#
#' Select a grts sample with minimum distance between sites.
#'
#' @param mindis Minimum distance required between sites in sample.
#'
#' @param sframe The sf object containing variables: id and ip.
#' 
#' @param samplesize The sample size required for the stratum.
#' 
#' @param stratum Character string that identifies stratum membership for each element 
#'   in the frame. Cannot be NULL.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'   
#' @param legacy_option Logical variable where if equal to TRUE, legacy sites are
#'   to be incorporated into survey design.
#'
#' @param legacy_var Character value for name of column for legacy site variable.
#'   Default is NULL.
#'
#' @param warn_ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn_df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#'
#' @return sites A list of sf object of sample points, an sf object of over sample
#'   points if any, warning indicator and warning messages data.frame
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{get_address}}{constructs the hierarchical address for
#'       sample points}
#'     \item{\code{UPpivotal}}{selects sample point(s)}
#'   }
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

grtspts_mindis <- function(mindis, sframe, samplesize, stratum, maxtry = 10,
                           legacy_option = NULL, legacy_var = NULL, 
                           warn_ind = NULL, warn_df = NULL) {

  # select initial set of sites
  sites <- sframe[get_address(sframe$xcoord, sframe$ycoord, rand = TRUE), ]
  s <- UPpivotal(sites$ip)
  sites_base <- sites[round(s) == 1, ]
  
  # calculate distance between sites
  site_dist <- st_distance(sites_base)
  class(site_dist) <- "numeric"
  nr <- nrow(sites_base)
  
  # find sites less than mindis and set to FALSE otherwise set to TRUE
  keep <- apply(site_dist, 1, function(x){ifelse(any(x[x>0] < mindis), FALSE, TRUE)})
  
  # if any legacy sites keep those sites
  if(legacy_option == TRUE) {
    keep[!is.na(sites_base$legacy)] <- TRUE
  }

  # see if any sites are less than mindis and check until none or max tries
  ntry <- 1
  while(any(!keep)) {
    # identify sites that will be treated as legacy probability sites in sample frame
    sframe$probdis <- FALSE
    sframe$probdis[sframe$idpts %in% sites_base$idpts[keep]] <- TRUE
    
    # if any true legacy sites add them to sites to be kept
    if(legacy_option == TRUE) {
      sframe$probdis[!is.na(sframe$legacy)] <- TRUE
    }

    # Adjust initial inclusion probabilities to account for current mindis sites
    # and any legacy sites
    sframe$ip <- grtspts_ipleg(sframe$ip_init, sframe$probdis)

    # select new sites that include legacy sites
    sites <- sframe[get_address(sframe$xcoord, sframe$ycoord, rand = TRUE), ]
    s <- UPpivotal(sites$ip)
    sites_base <- sites[round(s) == 1, ]

    # calculate distance between sites
    site_dist <- st_distance(sites_base)
    class(site_dist) <- "numeric"
    nr <- nrow(sites_base)
    
    # identify sites less than mindis
    keep <- apply(site_dist, 1, function(x){ifelse(any(x[x>0] < mindis), FALSE, TRUE)})
    
    # Change to TRUE if any legacy sites
    if(legacy_option == TRUE) {
      keep[!is.na(sites_base$legacy)] <- TRUE
    }

    # check if maxtry reached. If so write out warning message
    if(ntry >= maxtry) {
      keep <- rep(TRUE, nr)
      warn <- paste0("Minimum distance between sites not attained after ", maxtry, " attempts.")
      if(warn_ind){
        warn_df <- rbind(warn_df, data.frame(stratum = stratum, func = I("grtspts_mindis"),
                                             warning = warn))
      } else {
        warn_ind <- TRUE
        warn_df <- data.frame(stratum = stratum, func = I("grtspts_mindis"), warning = warn)
      }
    } else { ntry <- ntry + 1}
  } # end of ntry loop

  # drop internal variables
  tmp <- names(sites_base)
  sites_base <- subset(sites_base, select = tmp[!(tmp %in% c("probdis", "geometry"))])

  # Put sites in reverse hierarchical order
  sites_base <- rho(sites_base)
  sites_base$siteuse <- NA
  sites_base$replsite <- NA

  sites <- list(sites_base = sites_base, 
                warn_ind = warn_ind, warn_df = warn_df)

  invisible(sites)
}



