################################################################################
# Function: pickFiniteSamplePoints
# Programmer: Tom Kincaid
# Date: May 15, 2019
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
#' @return Vector containing feature IDs of the selected sample points.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickFiniteSamplePoints <- function(rdx, xc, yc, dx, dy, sfobject) {

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
      warning(paste("\nThe number of points to be selected from the cell with index value", i, "exceeded the number of points in the cell.\n"))
    } else if(npt.sample == npt.cell) {
      id <- c(id, sfobject$id[tstcell])
    } else {
      id <- c(id, sample(sfobject$id[tstcell], npt.sample,
        prob=sfobject$mdm[tstcell]))
    }
  }

# Return the vector of feature IDs
  return(id)
}
