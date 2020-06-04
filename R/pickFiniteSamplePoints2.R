################################################################################
# Function: pickFiniteSamplePoints2
# Programmer: Tom Kincaid
# Date: May 15, 2019
# Revised: May 5, 2020 Tony Olsen to include sites with ip = 1
# Revised: May 26, 2020 Tony Olsen to include option for over sample sites in cells
#
#' Select Sample Points from a Simple Features Point Object
#'
#' This function selects sample points from an sf object of geometry type
#' point.
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
#' @param over number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, typically 1 to 3.
#'       Default is NULL. In this function treated as either NULL or not NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return Vector containing feature IDs of the selected sample points.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickFiniteSamplePoints2 <- function(rdx, xc, yc, dx, dy, sfobject, over = NULL,
                                   warn.ind = warn.ind, warn.df = warn.df) {

  # Determine feature IDs for selected sample points

  id <- NULL
  id.over <- NULL
  coords <- st_coordinates(sfobject)
  for(i in unique(rdx)) {
    xr <- c( xc[i] - dx, xc[i])
    yr <- c( yc[i] - dy, yc[i])
    tstcell <- (xr[1] < coords[,1]) & (coords[,1] <= xr[2]) &
      (yr[1] < coords[,2]) & (coords[,2] <= yr[2])
    npt.sample <- sum(rdx == i)
    npt.cell <- length(sfobject$id[tstcell])
    if(npt.sample > npt.cell) {
      id <- c(id, sfobject$id[tstcell])
      warn <- paste("The number of points to be selected from the cell with index value", i,
                    "exceeded the number of points in the cell.")
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(func = I("pickFiniteSamplePoints.R"),
                                             warning = warn))
      } else {
        warn.ind <- TRUE
        warn.df <- data.frame(func = I("pickFiniteSamplePoints.R"), warning = warn)
      }

    } else if(npt.sample == npt.cell) {
      id <- c(id, sfobject$id[tstcell])
    } else {
      if(is.null(over)){
        # No over sample request
        nip1 <- 0
        tst1 <- sfobject$ip[tstcell] == 1
        if(any(tst1)){
          nip1 <- sum(tst1)
          id <- c(id, sfobject$id[tstcell & sfobject$ip == 1])
          }
        if( nip1 < npt.sample) {
          id <- c(id, sample(sfobject$id[tstcell & sfobject$ip < 1],
                             npt.sample - nip1,
                             prob = sfobject$ip[tstcell & sfobject$ip < 1]))
        }
      } else {
        # Over sample request
        nip1 <- 0
        tst1 <- sfobject$ip[tstcell] == 1
        if(any(tst1)){
          nip1 <- sum(tst1)
          id <- c(id, sfobject$id[tstcell & sfobject$ip == 1])
        }
        if( nip1 <= npt.sample + 1) {
          tmp <- sample(sfobject$id[tstcell & sfobject$ip < 1],
                        npt.sample + 1 - nip1,
                        prob = sfobject$ip[tstcell & sfobject$ip < 1])
          nid <- npt.sample - nip1
          id <- c(id, tmp[1:nid])
          id.over <- c(id.over, tmp[(nid + 1):length(tmp)])
        }
      }
    }
  }

  # Return the vector of feature IDs
  id.samp <- list(id = id, id.over = id.over)
  return(id.samp)
}
