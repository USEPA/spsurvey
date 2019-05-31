################################################################################
# Function: cellWeight
# Programmer: Tom Kincaid
# Date: May 17, 2019
#
#' Total Inclusion Probablity for a Grid Cell
#'
#' Calculates the total inclusion probability for a grid cell from a GRTS
#' survey design.
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

cellWeight <- function(xc, yc, dx, dy, sfobject, mdm) {

# Create an sf polygon object for the grid

  samp_grd <- make_grid(xc, yc, dx, dy, sfobject)
  samp_grd <- st_join(samp_grd, sfobject)

# Calculate the total inclusion probability for each grid cell

  if(all(st_geometry_type(sfobject) %in% c("POINT", "MULTIPOINT"))) {
    wgtsum <- with(samp_grd, tapply(mdm, poly, sum))
    wgtsum[is.na(wgtsum)] <- 0
  } else if(all(st_geometry_type(sfobject) %in% c("LINESTRING", "MULTILINESTRING"))) {
    samp_grd$length_mdm <- as.numeric(st_length(samp_grd))
    temp <- samp_grd$length_mdm * samp_grd$mdm
    wgtsum <- with(samp_grd, tapply(temp, poly, sum))
    wgtsum[is.na(wgtsum)] <- 0
  } else {
    samp_grd$area_mdm <- as.numeric(st_area(samp_grd))
    temp <- samp_grd$area_mdm * samp_grd$mdm
    wgtsum <- with(samp_grd, tapply(temp, poly, sum))
    wgtsum[is.na(wgtsum)] <- 0
  }

# Return the total

  return(wgtsum)
}
