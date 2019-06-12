################################################################################
# Function: selectpts
# Programmers: Tony Olsen, Tom Kincaid
# Date: October 27, 2004
# Last Revised: March 9, 2005
#
#' Internal Function: Select Probability Sample from a Set of Cells
#'
#' Selects a sample of size one or larger from a set of cells based on inclusion
#' probabilities.
#'
#' @param rdx Vector of the index value for selected cells.
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
#' @return The id value for the sample points.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'         Tony Olsen \email{olsen.tony@epa.gov}
#'
#' @export
################################################################################

selectpts <- function(rdx, xc, yc, dx, dy, pts) {

   id <- NULL
   for(cel in unique(rdx)) {
      xr <- c( xc[cel] - dx, xc[cel])
      yr <- c( yc[cel] - dy, yc[cel])
      tstcell <- (xr[1] < pts$x) & (pts$x <= xr[2]) & (yr[1] < pts$y) & (pts$y <= yr[2])
      npt.samp <- sum(rdx == cel)
      npt.cell <- length(pts$id[tstcell])
      if(npt.samp > npt.cell) {
         id <- c(id, pts$id[tstcell])
         warning(paste("\nThe number of points to be selected from the cell with index value", cel, "exceeded the number of points in the cell.\n"))
      } else if(npt.samp == npt.cell) {
         id <- c(id, pts$id[tstcell])
      } else {
         id <- c(id, sample(pts$id[tstcell], npt.samp, prob=pts$mdm[tstcell]))
      }
   }
   id
}
