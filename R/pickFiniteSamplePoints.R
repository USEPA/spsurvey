################################################################################
# Function: pickFiniteSamplePoints
# Programmer: Tom Kincaid
# Date: May 15, 2019
# Revised: May 5, 2020 Tony Olsen to include sites with ip = 1
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

pickFiniteSamplePoints <- function(rdx, xc, yc, dx, dy, sfobject,
                                   warn.ind = warn.ind, warn.df = warn.df) {

  # Determine feature IDs for selected sample points

  id <- NULL
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
    }
  }

  # Return the vector of feature IDs
  return(id)
}
