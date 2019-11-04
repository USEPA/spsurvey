################################################################################
# Function: make_grid2
# Programmer: Marc Weber
# Date: October 23, 2019
#
#' Create the Grid for a GRTS Survey Design
#'
#' Creates the grid for  a GRTS suvey design.
#'
#' @param xc x-coordinates that define the cells.
#'
#' @param yc y-coordinates that define the cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @param sfobject the sf object containing the survey frame.
#'
#' @return A raster grid.
#'
#' @author Marc Weber \email{Weber.Marc@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

make_grid2 <- function(xc, yc, dx, dy, sfobject) {
  n = length(xc)
  grd <- raster(ncol=sqrt(n), nrow=sqrt(n), xmn=xc[1]-dx, xmx=xc[n], ymn=yc[1]-dy, ymx=yc[n])
  projection(grd) <- st_crs(sfobject)$proj4string
  grd[] <- 1:ncell(grd)
  return(grd)
}


