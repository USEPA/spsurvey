################################################################################
# Function: replace_near.R
# Programmer:  Tony Olsen
# Date: December 23, 2020
#
#' @param over.near number of nearby sites to be used as potential replacement(s) 
#'    if a site cannot be sampled for any reason. Must be integer from 1 to 10.
#'    Default is NULL. 
#' 
#' @param sites sf point object of selected sites that require replacement sites
#' 
#' @param sframe sf point object that was used as sample frame to select sites.
#' 
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey design
#'
#' @export
################################################################################
replace_near <- function(over.near, sites, sframe) {
  
  # calculate distance between points
  site_dist <- st_distance(sframe)
  
  # set  possible levels for siteuse
  names.siteuse <- c("Near__1st", "Near__2nd", "Near__3rd", "Near__4th", "Near__5th", 
                     "Near__6th", "Near__7th", "Near__8th", "Near__9th", "Near_10th")
  
  site_dist_list <- split(site_dist, 1:nrow(site_dist))
  sites.near <- lapply(sites$idpts, function(x) {
      sites.tmp_ind <- site_dist_list[[x]]
      sites.tmp_ind <- sframe$idpts[order(sites.tmp_ind)][2:(over.near + 1)] # 0 always gets carried along
      sites.tmp <- sframe[sites.tmp_ind, , drop = FALSE]
      sites.tmp$siteuse <- names.siteuse[1:over.near]
      sites.tmp$replsite <- x
      sites.tmp
    }
  )
  sites.near <- do.call("rbind", sites.near)
}


# old
# replace_near <- function(over.near, sites, sframe) {
#   
#   # calculate distance between points
#   site_dist <- st_distance(sframe)
#   class(site_dist) <- "numeric"
#   nr <- nrow(sframe)
#   ns <- nrow(sites)
#   
#   # set  possible levels for siteuse
#   names.siteuse <- c("Near__1st", "Near__2nd", "Near__3rd", "Near__4th", "Near__5th", 
#                      "Near__6th", "Near__7th", "Near__8th", "Near__9th", "Near_10th")
#   
#   # assign over sample sites
#   for (i in 1:ns) {
#     # find frame id for site in sites.
#     keep <- sframe$idpts == sites$idpts[i]
#     tmp <- site_dist[keep,]
#     tmp.id <- order(tmp)[2:(over.near + 1)]
#     sites.tmp <- sframe[sframe$idpts == sframe$idpts[tmp.id[1]],] 
#     sites.tmp$siteuse <- "Near__1st"
#     sites.tmp$replsite <- sites$idpts[i]
#     for( k in 2:over.near){
#       jnk <- sframe[sframe$idpts == sframe$idpts[tmp.id[k]],] 
#       jnk$siteuse <- names.siteuse[k]
#       jnk$replsite <- sites$idpts[i]
#       sites.tmp <- rbind(sites.tmp, jnk)
#     }
#     if(i == 1) {
#       sites.near <- sites.tmp}
#     else { 
#       sites.near <- rbind(sites.near, sites.tmp) 
#     }
#   }
#   
#   return(sites.near)
# }