################################################################################
# Function: grtspts_mindis
# Programmer:  Tony Olsen
# Date: June 18, 2020
#
#' Select a grts sample with minimum distance between sites.
#'
#' @param mindis Minimum distance required between sites in sample.
#'
#' @param sframe The sf object containing variables: id and ip.
#'
#' @param grts_grid The hierarchical grid as a list required for GRTS design
#' 
#' @param samplesize The sample size required for the 
#' 
#' @param over.near Numeric value specifying the number of nearby points to select as
#'   possible replacement sites if a site cannot be sampled. Default is NULL. If specified,
#'   possible values are 1, 2 or 3.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'
#' @param stratum Character string that identifies stratum membership for each element 
#'   in the frame. Cannot be NULL.
#'
#' @param legacy_var Character value for name of column for legacy site variable.
#'   Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return sites A list of sf object of sample points, an sf object of over sample
#'   points if any, warning indicator and warning messages data.frame
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{constructAddr}}{constructs the hierarchical address for
#'       sample points}
#'     \item{\code{ranho}}{constructs the randomized hierarchical address for
#'       sample points}
#'     \item{\code{pickGridCells}}{selects grid cells that get a sample point}
#'     \item{\code{\link{pickFiniteSamplePoints}}}{pick sample point(s) from
#'       selected cells}
#'   }
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

grtspts_mindis <- function(mindis, sframe, grts_grid, samplesize, over.near = NULL, maxtry = 10,
                           stratum, legacy_var = NULL, warn.ind = NULL, warn.df = NULL) {

  # set sites.near to NULL. If required created by grtspts_select
  sites.near <- NULL

  # select initial set of sites
  sites <- grtspts_select(sframe, grts_grid, samplesize = samplesize, over.near = over.near,
                          warn.ind = warn.ind, warn.df = warn.df)
  if(sites$warn.ind) {
    warn.ind <- sites$warn.ind
    warn.df <- sites$warn.df
    warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
  }
  sites.base <- sites$sites.base
  if(!is.null(over.near)){
    sites.near <- sites$sites.near
  }

  # calculate distance between sites
  site_dist <- st_distance(sites.base)
  class(site_dist) <- "numeric"
  nr <- nrow(sites.base)
  
  # create data frame of upper triangle with i j denoting row/column of matrix
  to.upper<-function(X) X[upper.tri(X,diag=FALSE)]
  dist.df <- data.frame(i = sequence(1:(nr-1)), j = rep.int(2:nr, 1:(nr-1)),
                        dist = site_dist[upper.tri(site_dist)])
  # order from smallest to largest distance
  dist.df <- dist.df[order(dist.df$dist), ]
  
  # initialize keep vector as NA. Change to TRUE if any legacy sites
  keep <- rep(NA, NROW(sites.base))
  if(!is.null(legacy_var)) {
    keep[sites.base$legacy == TRUE] <- TRUE
  }
  # find sites less than mindis and change keep status
  for(k in 1:nrow(dist.df)){
    if(dist.df$dist[k] < mindis) {
      i <- dist.df$i[k]
      j <- dist.df$j[k]
      if(is.na(keep[i]) &  is.na(keep[j])) { 
        keep[c(i, j)] <- sample(c(TRUE, FALSE))
      } else {
        if(is.na(keep[i]) & !is.na(keep[j])) { keep[i] <- FALSE
        } else {
          if(!is.na(keep[i]) & is.na(keep[j])) {keep[j] <- FALSE
          }
        }
      }
    }
  }
  # If any sites are NA set to TRUE as they are greater than mindis
  keep[is.na(keep)] <- TRUE

  # see if any sites are less than mindis and check until none or max tries
  ntry <- 1
  while(any(!keep)) {
    # identify sites that will be treated as legacy probability sites in sample frame
    sframe$probdis <- FALSE
    sframe$probdis[sframe$LAKE_ID %in% sites$LAKE_ID[keep]] <- TRUE
    
    # if any true legacy sites add them to sites to be kept
    if(!is.null(legacy_var)) {
      sframe$probdis[sframe$legacy == TRUE] <- TRUE
    }

    # Adjust initial inclusion probabilities to account for current mindis sites
    # and any legacy sites
    sframe$ip <- grtspts_ipleg(sframe$ip_init, sframe$probdis)

    # adjust cell weights to use new inclusion probabilities
    grts_grid$cel.wt <- cellWeight(grts_grid$xc, grts_grid$yc,
                                   grts_grid$dx, grts_grid$dy, sframe)

    # select new sites that include legacy sites
    sites <- grtspts_select(sframe, grts_grid, samplesize = samplesize, over.near = over.near,
                            warn.ind = warn.ind, warn.df = warn.df)
    # check for warnings
    if(sites$warn.ind) {
      warn.ind <- sites$warn.ind
      warn.df <- sites$warn.df
      warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
    }
    sites.base <- sites$sites.base
    if(!is.null(over.near)) {
      sites.near <- sites$sites.near
    }

    # calculate distance between sites
    site_dist <- st_distance(sites.base)
    class(site_dist) <- "numeric"
    nr <- nrow(sites.base)
    
    # create data frame of upper triangle with i j denoting row/column of matrix
    to.upper<-function(X) X[upper.tri(X,diag=FALSE)]
    dist.df <- data.frame(i = sequence(1:(nr-1)), j = rep.int(2:nr, 1:(nr-1)),
                          dist = site_dist[upper.tri(site_dist)])
    # order from smallest to largest distance
    dist.df <- dist.df[order(dist.df$dist), ]
    
    # initialize keep vector as NA. Change to TRUE if any legacy sites
    keep <- rep(NA, NROW(sites.base))
    if(!is.null(legacy_var)) {
      keep[sites.base$legacy == TRUE] <- TRUE
    }
    # find sites less than mindis and change keep status
    for(k in 1:nrow(dist.df)){
      if(dist.df$dist[k] < mindis) {
        i <- dist.df$i[k]
        j <- dist.df$j[k]
        if(is.na(keep[i]) &  is.na(keep[j])) { 
          keep[c(i, j)] <- sample(c(TRUE, FALSE))
        } else {
          if(is.na(keep[i]) & !is.na(keep[j])) { keep[i] <- FALSE
          } else {
            if(!is.na(keep[i]) & is.na(keep[j])) {keep[j] <- FALSE
            }
          }
        }
      }
    }
    # If any sites are NA set to TRUE as they are greater than mindis
    keep[is.na(keep)] <- TRUE

    # check if maxtry reached. If so write out warning message
    if(ntry >= maxtry) {
      keep <- rep(TRUE, nr)
      warn <- paste0("Minimum distance between sites not attained after ", maxtry, " attempts.")
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(stratum = stratum, func = I("grtspts_mindis"),
                                             warning = warn))
      } else {
        warn.ind <- TRUE
        warn.df <- data.frame(stratum = stratum, func = I("grtspts_mindis"), warning = warn)
      }
    } else { ntry <- ntry + 1}
  } # end of ntry loop

  # drop internal variables
  tmp <- names(sites.base)
  sites.base <- subset(sites.base, select = tmp[!(tmp %in% c("probdis", "geometry"))])
  if(!is.null(over.near)){
    tmp <- names(sites.near)
    sites.near <- subset(sites.near, select = tmp[!(tmp %in% c("probdis", "geometry"))])
  }

  sites <- list(sites.base = sites.base, sites.near = sites.near,
                warn.ind = warn.ind, warn.df = warn.df)

  invisible(sites)
}



