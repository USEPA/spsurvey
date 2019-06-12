################################################################################
# Function: insideLinearGridCell
# Programmer: Tom Kincaid
# Date: June 4, 2019
#
#' Calculate Clipped Feature Lengths in a Set of Grid Cells
#'
#' For each grid cell, this function calculates the clipped length of each
#' linestring feature contained in the cell.
#'
#' @param sfobject The sf linestring object.
#'
#' @param rdx.u Vector of cell IDs.
#'
#' @param xc Vector of x-coordinates for the grid cells.
#'
#' @param yc Vector of y-coordinates for the grid cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @return Data frame containing the following variables: cellID, featureLength,
#'   and featureID.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

insideLinearGridCell <- function(sfobject, rdx.u, xc, yc, dx, dy) {

# Create a data frame for results

  cell.df <- data.frame()

# Calculate clipped feature lengths for each cell

  for(i in 1:length(xc)) {
    temp <- rbind(c(xc[i] - dx, yc[i] - dy), c(xc[i], yc[i] - dy),
                  c(xc[i], yc[i]), c(xc[i] - dx, yc[i]),
                  c(xc[i] - dx, yc[i] - dy))
    sfcell <- st_sf(st_sfc(st_polygon(list(temp)), crs = st_crs(sfobject)))
    tempsf <- st_intersection(sfobject, sfcell)
    cell.df <- rbind(cell.df, cbind(
                       cellID = rdx.u[i],
                       featureLength = st_length(tempsf),
                       featureID = tempsf$id))
  }

# Return the data frame containing results
 
  return(cell.df)
}
