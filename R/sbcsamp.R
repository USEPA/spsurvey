################################################################################
# Function: sbcsamp
# Programmer: Tom Kincaid
# Date: September 29, 2011
# Last Revised: May 30, 2019
#
#' Calculate Spatial Balance Grid Cell Extent and Proportions for a Survey Design
#'
#' This function calculates spatial balance grid cell extent and proportions
#' for a survey design.  The user must provide either sbc.frame or values for
#' dx, dy, xc, and yc.
#'
#' @param spsample Object of class SpatialDesign produced by either the grts or
#'   irs functions that contains survey design information and additional
#'   attribute (auxiliary) variables.
#'
#' @param sbc.frame The object created by the sbcframe function.  The default is
#'   NULL.
#'
#' @param dx Grid cell x-coordinate increment value.  The default is NULL.
#'
#' @param dy Grid cell y-coordinate increment value.  The default is NULL.
#'
#' @param xc Vector of grid cell x-coordinates.  The default is NULL.
#'
#' @param yc Vector of grid cell y-coordinates.  The default is NULL.
#'
#' @return List containing the following components:
#'   \describe{
#'     \item{extent}{the sample extent for each grid cell}
#'     \item{prop}{the sample proportion for each grid cell}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

sbcsamp <- function(spsample, sbc.frame = NULL, dx = NULL, dy = NULL,
   xc = NULL, yc = NULL) {

# Convert the spsample object to an object of class sf
  sfobject <- st_as_sf(spsample)

# If the sbc.frame object was provided, obtain values for dx, dy, xc, and yc
   if(!is.null(sbc.frame)) {
      dx <- sbc.frame$dx
      dy <- sbc.frame$dy
      xc <- sbc.frame$xc
      yc <- sbc.frame$yc
   }

# Create an sf polygon object containing the survey design grid

  design_grd <- make_grid(xc, yc, dx, dy, sfobject)
  design_grd <- st_join(design_grd, sfobject)

# Calculate extent for each grid cell

  design_grd$point_mdm <- 1
  extent <- with(design_grd, tapply(point_mdm, poly, sum))
  extent[is.na(extent)] <- 0

# Calculate proportion for each grid cell
  prop <- extent/sum(extent)

# Return results
   list(extent=extent, prop=prop)
}
