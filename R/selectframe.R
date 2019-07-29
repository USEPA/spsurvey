################################################################################
# Function: selectframe
# Programmer: Tom Kincaid
# Date: February 28, 2006
#
#' Internal Function: Select All Points in Survey Frame
#'
#' Internal Function: Select All Points in Frame
#'
#' @param rord Vector of the index value for all cells.
#'
#' @param xc Vector of x-coordinates that define the cells.
#'
#' @param yc Vector of y-coordinates that define the cells.
#'
#' @param dx Width of the cells along the x-axis.
#'
#' @param dy Width of the cells along the y-axis.
#'
#' @param pts Data frame containing id values, x-coordinates, y-coordinates,
#'   and mdm values.
#'
#' @return The id value for all points in the frame.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

selectframe <- function(rord, xc, yc, dx, dy, pts) {

   id <- NULL
   for(cel in rord) {
      xr <- c( xc[cel] - dx, xc[cel])
      yr <- c( yc[cel] - dy, yc[cel])
      tstcell <- (xr[1] < pts$x) & (pts$x <= xr[2]) & (yr[1] < pts$y) & (pts$y <= yr[2])
      npt.cell <- length(pts$id[tstcell])
      if(npt.cell == 1) {
         id <- c(id, pts$id[tstcell])
      } else {
         id <- c(id, sample(pts$id[tstcell], npt.cell))
      }
   }
   id
}
