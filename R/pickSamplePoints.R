################################################################################
# Function: pickSamplePoints
# Programmer: Tom Kincaid
# Date: June 7, 2019
#
#' Select Sample Points from a Simple Features Object
#'
#' This function selects sample points from an sf object of geometry type
#' polygon or linestring.
#'
#' @param sfobject The sf polygon object.
#'
#' @param featureID Vector identifying features in sfobject from which a sample
#'   point will be selected.
#'
#' @param xc Vector of x-coordinates for the grid cells.
#'
#' @param yc Vector of y-coordinates for the grid cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @return Data frame containing x-coordinates and y-coordinates for sample
#'   points.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickSamplePoints <- function(sfobject, featureID, xc, yc, dx, dy) {

# Create a data frame for results

  points.df <- data.frame()

# Select a sample point for each cell

  for(i in 1:length(xc)) {
    temp <- rbind(c(xc[i] - dx, yc[i] - dy), c(xc[i], yc[i] - dy),
                  c(xc[i], yc[i]), c(xc[i] - dx, yc[i]),
                  c(xc[i] - dx, yc[i] - dy))
    sfcell <- st_sf(st_sfc(st_polygon(list(temp))))
    st_crs(sfcell) <- st_crs(sfobject)
    indx <- match(featureID[i], sfobject$id)
    tempsf <- st_intersection(sfobject[indx,], sfcell)
    if(all(st_geometry_type(tempsf) %in% c("POLYGON", "MULTIPOLYGON"))) {
       samp <- st_sample(tempsf, 1, exact=TRUE)
    } else {
       samp <- st_sample(tempsf, 1)
    }
    pts <- st_coordinates(samp)
    points.df <- rbind(points.df, cbind(
                         xcs = pts[1,1],
                         ycs = pts[1,2]))
  }  

# Return the data frame containing results
 
  return(points.df)
}
