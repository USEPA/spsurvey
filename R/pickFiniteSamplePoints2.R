################################################################################
# Function: pickFiniteSamplePoints
# Programmer: Tom Kincad and Tony Olsen
# Date: June 9, 2020
#
#' Select Sample Points from a Simple Features Point Object.This function selects
#'  sample points from an sf object of geometry type point.
#'
#' @param rdx Vector of cell IDs.
#'
#' @param xc Vector of x-coordinates for the grid cells.
#'
#' @param yc Vector of y-coordinates for the grid cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @param sfobject The sf point object.
#' 
#' @param over.near number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, must be 1, 2 or 3.
#'       Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return id.samp A list containing a vector of feature IDs of the selected sample points,
#'   a vector of feature IDs for replacement sites within each cell if any, warn.ind 
#'   logical value and warn.df as a data.frame with warning messages.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickFiniteSamplePoints2 <- function(rdx, xc, yc, dx, dy, sfobject, over.near = NULL,
                                   warn.ind = warn.ind, warn.df = warn.df) {

  # Determine feature IDs for selected sample points

  id <- NULL
  id.near <- NULL
  siteuse <- NULL
  replsite <- NULL
  siteuse.near <- NULL
  replsite.near <- NULL
  coords <- st_coordinates(sfobject)
  for(i in unique(rdx)) {
    xr <- c( xc[i] - dx, xc[i])
    yr <- c( yc[i] - dy, yc[i])
    tstcell <- (xr[1] < coords[,1]) & (coords[,1] <= xr[2]) &
      (yr[1] < coords[,2]) & (coords[,2] <= yr[2])
    npt.sample <- sum(rdx == i)
    npt.cell <- length(sfobject$id[tstcell])
    
    # check if cell has sufficient points to select. If not use all available, give warning
    if(npt.sample > npt.cell) {
      id <- c(id, sfobject$id[tstcell])
      siteuse <- c(siteuse, rep("Base", length(npt.cell)))
      replsite <- c(replsite, rep(NA, length(npt.cell)))
      warn <- paste("The number of points to be selected from the cell with index value", i,
                    "exceeded the number of points in the cell.")
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(func = I("pickFiniteSamplePoints.R"),
                                             warning = warn))
      } else {
        warn.ind <- TRUE
        warn.df <- data.frame(func = I("pickFiniteSamplePoints.R"), warning = warn)
      }
    }
    
    # Check if cell has exactly number of points required and use all.
    if(npt.sample == npt.cell) {
      id <- c(id, sfobject$id[tstcell])
      siteuse <- c(siteuse, rep("Base", npt.sample))
      replsite <- c(replsite, rep(NA, npt.sample))
    }
    
    # select sites when have more points than required and no over.near required
    if(npt.sample < npt.cell & is.null(over.near)){
      nip1 <- 0
      tst1 <- sfobject$ip[tstcell] == 1
      # first select all points with inclusion probability equal to 1
      if(any(tst1)){
        nip1 <- sum(tst1)
        id <- c(id, sfobject$id[tstcell & sfobject$ip == 1])
        siteuse <- c(siteuse, rep("Base", nip1))
        replsite <- c(replsite, rep(NA, nip1))
      }
      # select additional points required if any
      if( nip1 < npt.sample) {
        id <- c(id, sample(sfobject$id[tstcell & sfobject$ip < 1], npt.sample - nip1,
                           prob = sfobject$ip[tstcell & sfobject$ip < 1]))
        siteuse <- c(siteuse, rep("Base", npt.sample - nip1))
        replsite <- c(replsite, rep(NA, npt.sample - nip1))
      }
    }
    
    # Select sites when over.near sample requested and know that base sites can be selected
    if(npt.sample < npt.cell & !is.null(over.near)){
      nip1 <- 0
      tst1 <- sfobject$ip[tstcell] == 1
      # select all points with inclusion probability equal to 1
      # where we know all will be part of base sample plus maybe others
      if(any(tst1)){
        nip1 <- sum(tst1)
        id <- c(id, sfobject$id[tstcell & sfobject$ip == 1])
        siteuse <- c(siteuse, rep("Base", nip1))
        replsite <- c(replsite, rep(NA, nip1))
      }
      # select additional points required for base (if any) and over.near
      if( nip1 < npt.sample + over.near) {
        n.select <- min(npt.sample + over.near - nip1, sum(tstcell & sfobject$ip < 1))
        tmp <- sample(sfobject$id[tstcell & sfobject$ip < 1], n.select,
                      prob = sfobject$ip[tstcell & sfobject$ip < 1])
        nid <- npt.sample - nip1
        id <- c(id, tmp[1:nid])
        siteuse <- c(siteuse, rep("Base", nid))
        replsite <- c(replsite, rep(NA, nid))
        ntmp <- length(tmp) - nid
        id.near <- c(id.near, tmp[(nid + 1):length(tmp)])
        siteuse.near <- c(siteuse.near, 
                          c("First", "Second", "Third", "Fourth", "Fifth")[1:ntmp])
        replsite.near <- c(replsite.near, rep(tmp[1:npt.sample], ntmp))
      }
    }
  }
  # Return the vector of feature IDs
  id.samp <- list(id = list(id=id, siteuse = siteuse, replsite = replsite),
                  id.near = list(id.near=id.near, siteuse.near = siteuse.near,
                                 replsite.near = replsite.near),
                  warn.ind = warn.ind, warn.df = warn.df)
  
  return(id.samp)
}
