################################################################################
# Function: cell.wt
# Purpose: Calculates the total inclusion probability for a cell.
# Programmer: Tony Olsen
# Date: October 27, 2004
#'Last Revised: September 24, 2018
#'
#' Total Inclusion Probablity for Matrix of Cells
#'
#' Calculates the total inclusion probability for a cell. Used to evaluate
#' spatial balance of a survey design realization.
#'
#' @param  cel = the index value for a cell.
#'
#' @param  xc = x-coordinates that define the cells.
#'
#' @param  yc = y-coordinates that define the cells.
#'
#' @param  dx = width of the cells along the x-axis.
#'
#' @param  dy = width of the cells along the y-axis.
#'
#' @param  pts = an `sf` data frame containing x-coordinates and y-coordinates in the geometry list-column, and mdm
#'   values.
#'
#' @return  The total inclusion probability for a cell.
#'
#' @author Tony Olsen  \email{Olsen.Tony@epa.gov}
#' @author Marc Weber  \email{Weber.Marc@epa.gov}
#' @keywords survey
#'
#' @export
#'
################################################################################
cell.wt <- function(cel, xc, yc, dx, dy, pts) {

   xr <- c( xc[cel] - dx, xc[cel])
   yr <- c( yc[cel] - dy, yc[cel])
   tstcell <- (xr[1] < st_coordinates(pts$geometry)[,1]) & 
     (st_coordinates(pts$geometry)[,1] <= xr[2]) & (yr[1] < st_coordinates(pts$geometry)[,2]) & 
     (st_coordinates(pts$geometry)[,2] <= yr[2])
   wt <- sum(pts$mdm[tstcell])
   wt
}
