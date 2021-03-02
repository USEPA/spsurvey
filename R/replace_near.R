###############################################################################
# Function: replace_near.R (exported)
# Programmer:  Michael Dumelle and Tony Olsen
# Date: March 2, 2021
#
#' @param n_near number of nearby sites to be used as potential replacement(s) 
#'    if a site cannot be sampled for any reason. Must be integer from 1 to 10.
# 
#' Replacement sites generated via nearest neighbors
#' 
#' Returns 1 to 10 nearest neighbor replacement sites for each site in the 
#'   base sample
#' 
#' @param n_near number of nearby sites to be used as potential replacement(s)
#'    if a site cannot be sampled for any reason. Must be integer from \code{1} to \code{10}.
#'    Default is \code{NULL}.
#'
#' @param sites sf point object of selected sites that require replacement sites
#'
#' @param sframe sf point object that was used as sample frame to select sites.
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov} and Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @keywords survey design
#'
#' @export
################################################################################
replace_near <- function(n_near, sites, sframe) {
  
  # calculate distance between points
  site_dist <- st_distance(sites, sframe)

  # set  possible levels for siteuse
  names_siteuse <- c("Near__1st", "Near__2nd", "Near__3rd", "Near__4th", "Near__5th", 
                     "Near__6th", "Near__7th", "Near__8th", "Near__9th", "Near_10th")

  # split
  site_dist_list <- split(site_dist, 1:nrow(site_dist))
  # apply
  sites.near <- mapply(function(x, y) {
    sites.tmp_ind <- sframe$idpts[order(x)][2:(n_near + 1)] # 0 always gets carried along
    sites.tmp <- sframe[sites.tmp_ind, , drop = FALSE]
    sites.tmp$siteuse <- names_siteuse[1:nrow(sites.tmp)] # covers cases where there are less than n_near sites available
    sites.tmp$replsite <- y
    sites.tmp
  },
  x = site_dist_list,
  y = sites$idpts,
  SIMPLIFY = FALSE
  )

  # combine
  sites.near <- do.call("rbind", sites.near)
}

