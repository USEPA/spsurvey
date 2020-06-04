################################################################################
# Function: replace_sites.R
# Programmer:  Tony Olsen
# Date: June 1, 2020
#
#' @param over number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, typically 1 to 3.
#'       Default is NULL. In this function treated as either NULL or not NULL.
#' 
#' @param over.max Maximum number of over sample sites for each site across all strata.
#' 
#' @param sites Data frame of selected sites
#' 
#' @param sites.over Data frame of over sample sites with one site selected for each
#'   base site if possible.
#' 
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################
replace_sites <- function(over, over.max, sites, sites.over) {

  # combine sites and over sample sites
  comb <- rbind(sites, sites.over)
  
  # calculate distance between sites
  site_dist <- st_distance(comb)
  class(site_dist) <- "numeric"
  nr <- nrow(comb)
  ns <- nrow(sites)
  nover <- nrow(sites.over)
  
  # create data frame of upper triangle with i j denoting row/column of matrix
  to.upper <- function(X) X[upper.tri(X, diag = FALSE)]
  
  dist.df <- data.frame(i = sequence(1:(nr - 1)),
                        j = rep.int(2:nr, 1:(nr - 1)),
                        dist = site_dist[upper.tri(site_dist)])
  # drop columns that are for base sites
  dist.df <- dist.df[!(dist.df$j %in% 1:ns),]
  # drop rows that are over sites
  dist.df <- dist.df[dist.df$i %in% 1:ns, ]
  
  # order distance from base site to all over sample sites from shortest to fartherest
  # Assign over sample site id to replacement variables up to over.max variables

  # assign over sample sites
  for (i in 1:ns) {
    tmp <- dist.df[dist.df$i == i,]
    tmp <- tmp[order(tmp$dist),]
    keep <- sites.over$id == sites.over$id[tmp$j[i] - ns]
    if(over.max >= 1 & i == 1) {
      rslts <-  sites.over[keep,]
      rslts$replsite <- sites$id[1]
      rslts$oversite = "First"
    }
    if(over.max >= 1 & i > 1) {
      tmp.r <-  sites.over[keep,]
      tmp.r$replsite <- sites$id[i]
      tmp.r$oversite = "First"
      rslts <-  rbind(rslts, tmp.r)
    }
    if(over.max >= 2) {
      tmp.r <-  sites.over[keep,]
      tmp.r$replsite <- sites$id[i]
      tmp.r$oversite = "Second"
      rslts <-  rbind(rslts, tmp.r)
    }
    if(over.max >= 3) {
      tmp.r <-  sites.over[keep,]
      tmp.r$replsite <- sites$id[i]
      tmp.r$oversite = "Third"
      rslts <-  rbind(rslts, tmp.r)
    }
  }
  
  invisible(rslts)
}



