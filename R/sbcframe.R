################################################################################
# Function: sbcframe
# Programmer: Tom Kincaid
# Date: September 29, 2011
# Date: June 7, 2019
#
#' Calculate Spatial Balance Grid Cell Extent and Proportion for a Survey Frame
#'
#' This function calculates spatial balance grid cell extent and proportion
#' for the sample frame.  
#'      
#' @param sfobject An object of class sf that contains the survey frame.
#'      
#' @param nrows Number of rows (and columns) for the grid of cells.  The
#'   default is 5.
#'      
#' @param dxdy Indicator for equal x-coordinate and y-coordinate grid cell
#'   increments, where TRUE means the increments are equal and FALSE means the
#'   increments are not equal.  The default is TRUE.
#'      
#' @return List containing the following components:
#'   \describe{
#'     \item{extent}{the frame extent for each grid cell}
#'     \item{prop}{the frame proportion for each grid cell}
#'     \item{xmin}{the grid x-coordinate minimum value}
#'     \item{xmax}{the grid x-coordinate maximum value}
#'     \item{ymin}{the grid y-coordinate minimum value}
#'     \item{ymax}{the grid y-coordinate maximum value}
#'     \item{dx}{the grid cell x-coordinate increment value}
#'     \item{dy}{the grid cell y-coordinate increment value}
#'     \item{xc}{the vector of grid cell x-coordinates}
#'     \item{yc}{the vector of grid cell y-coordinates}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

sbcframe <- function(sfobject, nrows = 5, dxdy = TRUE) {

# Calculate the x-coordinate and y-coordinate increment values and create the
# vectors of grid x-coordinates and y-coordinates
   bbox <- st_bbox(sfobject)
   xmin <- bbox$xmin
   ymin <- bbox$ymin
   xmax <- bbox$xmax
   ymax <- bbox$ymax
   if(dxdy) {
      gridExtent = max((xmax - xmin), (ymax - ymin))
      xmin = xmin - gridExtent * 0.001
      xmax = xmin + gridExtent * 1.002
      ymin = ymin - gridExtent * 0.001
      ymax = ymin + gridExtent * 1.002
   } else {
      gridExtent = xmax - xmin;
      xmin = xmin - gridExtent * 0.001
      xmax = xmin + gridExtent * 1.002
      gridExtent = ymax - ymin
      ymin = ymin - gridExtent * 0.001
      ymax = ymin + gridExtent * 1.002
   }
   dx <- (xmax - xmin)/nrows
   dy <- (ymax - ymin)/nrows
   xc <- seq(xmin, xmax, length=(nrows+1))[-1]
   xc <- rep(xc, nrows)
   yc <- seq(ymin, ymax, length=(nrows+1))[-1]
   yc <- rep(yc, rep(nrows, nrows))

# Calculate extent for each grid cell

  if(all(st_geometry_type(sfobject) %in% c("POINT", "MULTIPOINT"))) {
    frame_grd <- make_grid(xc, yc, dx, dy, sfobject)
    frame_grd <- st_join(frame_grd, sfobject)
    frame_grd$point_mdm <- 1
    extent <- with(frame_grd, tapply(point_mdm, poly, sum))
    extent[is.na(extent)] <- 0
  } else if(all(st_geometry_type(sfobject) %in% c("LINESTRING", "MULTILINESTRING"))) {
    extent <- numeric(length(xc))
    for(i in 1:length(xc)) {
      temp <- rbind(c(xc[i] - dx, yc[i] - dy), c(xc[i], yc[i] - dy),
                    c(xc[i], yc[i]), c(xc[i] - dx, yc[i]),
                    c(xc[i] - dx, yc[i] - dy))
      sfcell <- st_sf(st_sfc(st_polygon(list(temp)), crs = st_crs(sfobject)))
      tempsf <- st_intersection(sfobject, sfcell)
      if(nrow(tempsf == 0)) {
        extent[i] <- 0
      } else {
        extent[i] <- sum(st_length(tempsf))
      }
    }
  } else {
    extent <- numeric(length(xc))
    for(i in 1:length(xc)) {
      temp <- rbind(c(xc[i] - dx, yc[i] - dy), c(xc[i], yc[i] - dy),
                    c(xc[i], yc[i]), c(xc[i] - dx, yc[i]),
                    c(xc[i] - dx, yc[i] - dy))
      sfcell <- st_sf(st_sfc(st_polygon(list(temp)), crs = st_crs(sfobject)))
      tempsf <- st_intersection(sfobject, sfcell)
      if(nrow(tempsf == 0)) {
        extent[i] <- 0
      } else {
        extent[i] <- sum(st_area(tempsf))
      }
    }
  }

# Calculate proportion for each grid cell
  prop <- extent/sum(extent)

# Return results
   list(extent=extent, prop=prop, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
        dx=dx, dy=dy, xc=xc, yc=yc)
}
