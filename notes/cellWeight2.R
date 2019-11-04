################################################################################
# Function: cellWeight
# Programmer: Tom Kincaid
# Modified by: Marc Weber
# Date: July 8, 2019
# Date Modified: October 23, 2019
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
    samp_grd <- make_grid2(xc, yc, dx, dy, sfobject)
    e <- raster::extract(samp_grd,sfobject)
    samp_df <- as.data.frame(samp_grd)
    names(samp_df)[1] <- 'gridval'
    sfobject$gridval <- e
    sfobject_dt <- data.table(sfobject)
    sample_df_dt <- data.table(samp_df)
    sample_df_dt <- sfobject_dt[sample_df_dt, on='gridval'] # data.table left join
    wgtsum <- with(sample_df_dt, tapply(mdm, gridval, sum))
    wgtsum[is.na(wgtsum)] <- 0
  } else {
    samp_grd <- make_grid2(xc, yc, dx, dy, sfobject)
    temp <- by(samp_grd, 1:nrow(sample_df_dt), 
               function(x) st_as_sf(rasterToPolygons(rasterFromCells(x))))
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
