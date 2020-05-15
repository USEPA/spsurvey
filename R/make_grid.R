################################################################################
# Function: make_grid
# Programmers: Tom Kincaid, Marc Weber
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
#' @return An sf object containing the grid.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################
# in progress function
# make_grid <- function(xc, yc, dx, dy, sfobject) {
#   n = length(xc)
#   grd <- raster(ncol=sqrt(n), nrow=sqrt(n), xmn=xc[1]-dx, xmx=xc[n], ymn=yc[1]-dy, ymx=yc[n])
#   projection(grd) <- st_crs(sfobject)$proj4string
#   grd[] <- 1:ncell(grd)
#   return(grd)
# }


#  working function
 make_grid <- function(xc, yc, dx, dy, sfobject) {

   n = length(xc)
   grd = vector("list", n)
   cell = function(x, dx, y, dy) {
     st_polygon(list(matrix(c(x-dx, x, x, x-dx, x-dx, y-dy, y-dy, y, y, y-dy), 5)))
   }
   for (i in 1:n) {
     grd[[i]] = cell(xc[i], dx, yc[i], dy)
   }
   grd <- st_sfc(grd, crs = st_crs(sfobject))
   grd <- st_sf(poly = 1:n, geometry = grd)
   st_agr(grd) <- "constant"
   return(grd)
 }
