################################################################################
# Function: cellWeight
# Programmer: Tom Kincaid
# Date: May 14, 2019
#
#' Total Inclusion Probablity for a Grid Cell
#'
#' Calculates the total inclusion probability for a grid cell from a GRTS
#' suvey design.
#'
#' @param cel Index value for a cell.
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
#' @return The total inclusion probability for the cell.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

cellWeight <- function(cel, xc, yc, dx, dy, sfobject) {

# Create an sf polygon object for the grid cell
  start_time <- Sys.time()
  temp <- rbind(c(xc[cel] - dx, yc[cel] - dy), c(xc[cel], yc[cel] - dy),
                c(xc[cel], yc[cel]), c(xc[cel] - dx, yc[cel]),
                c(xc[cel] - dx, yc[cel] - dy))
  sfcell <- st_sf(st_sfc(st_polygon(list(temp))))
  st_crs(sfcell) <- st_crs(sfobject)

# Create an sf object containing the intersection of the grid cell with the
# input sf object
  start_time <- Sys.time()
  tempsf <- st_intersection(sfobject, sfcell)
  end_time <- Sys.time()
  end_time - start_time
  
  # tempsf <- sfobject[sfcell,]
  
# Calculate the total inclusion probability for the cell

  if(all(st_geometry_type(sfobject) %in% c("POINT", "MULTIPOINT"))) {
    wgtsum <- sum(tempsf$mdm)
  } else if(all(st_geometry_type(sfobject) %in% c("LINESTRING", "MULTILINESTRING"))) {
    tempsf$length_mdm <- as.numeric(st_length(tempsf))
    wgtsum <- sum(tempsf$length_mdm * tempsf$mdm)
  } else  {
    tempsf$area_mdm <- as.numeric(st_area(tempsf))
    wgtsum <- sum(tempsf$area_mdm * tempsf$mdm)
  }

# Return the total
  end_time <- Sys.time()
  end_time - start_time
  
  return(wgtsum)
}
