################################################################################
# Function: cellWeight
# Programmer: Tom Kincaid
# Date: July 8, 2019
#
#' Total Inclusion Probablity for a Grid Cell
#'
#' Calculates the total inclusion probability for each grid cell from a GRTS
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
#' @return Vector containing the total inclusion probability for each cell.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

cellWeight <- function(xc, yc, dx, dy, sfobject) {

# Calculate the total inclusion probability for each grid cell

  if(all(st_geometry_type(sfobject) %in% c("POINT", "MULTIPOINT"))) {
    samp_grd <- make_grid(xc, yc, dx, dy, sfobject)
    samp_grd <- st_join(samp_grd, sfobject)
    wgtsum <- with(samp_grd, tapply(mdm, poly, sum))
    wgtsum[is.na(wgtsum)] <- 0
  } else {
    samp_grd <- make_grid(xc, yc, dx, dy, sfobject)
    temp <- by(samp_grd, 1:nrow(samp_grd), function(x) x)
    cl <- getDefaultCluster()
    temp <-  parLapply(cl, temp, function(x) st_intersection(x, sfobject))
    wgtsum <- sapply(temp, celwt)
  }

# Return the total

  return(wgtsum)
}

celwt <- function(sfcell) {

  if(nrow(sfcell) == 0) {
    cellsum <- 0
  } else {
    if(all(st_geometry_type(sfcell) %in% c("LINESTRING", "MULTILINESTRING"))) {
      cellsum <- sum(as.numeric(st_length(sfcell)) * sfcell$mdm)
    } else {
     cellsum <- sum(as.numeric(st_area(sfcell)) * sfcell$mdm)
    }
  }
  cellsum
}
