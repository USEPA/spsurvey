###############################################################################
# Function: replace_near.R (exported)
# Programmer:  Michael Dumelle and Tony Olsen
# Date: March 2, 2021
#
#' Find Nearest Neighbor Replacement Sites
#'
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

  # remove sampled sites from sframe
  sframe <- sframe[!(sframe$idpts %in% sites$idpts), , drop = FALSE]

  # calculate distance between points
  site_dist <- st_distance(sites, sframe)

  # set  possible levels for siteuse
  names_siteuse <- c(
    "Near-1st", "Near-2nd", "Near-3rd", "Near-4th", "Near-5th",
    "Near-6th", "Near-7th", "Near-8th", "Near-9th", "Near-10th"
  )

  # split
  site_dist_list <- split(site_dist, 1:nrow(site_dist))
  # apply
  sites_near <- mapply(function(x, y) {
    sites_tmp_ind <- sframe$idpts[order(x)][seq_len(n_near)]
    sites_tmp <- sframe[sframe$idpts %in% sites_tmp_ind, , drop = FALSE]
    sites_tmp$siteuse <- names_siteuse[seq_len(nrow(sites_tmp))] # covers cases where there are less than n_near sites available
    sites_tmp$replsite <- y
    sites_tmp
  },
  x = site_dist_list,
  y = sites$idpts,
  SIMPLIFY = FALSE
  )

  # combine
  sites_near <- do.call("rbind", sites_near)
}
