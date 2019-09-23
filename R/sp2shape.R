################################################################################
# Function: sp2shape
# Programmer: Tom Kincaid
# Date: June 6, 2006
# Last Revised: September 20, 2019
#'
#' Create an ESRI shapefile from an sp package object
#'
#' This function creates an ESRI shapefile from an sp package object.  The
#' function can also accommodate an object created by the grts or irs functions
#' in spsurvey.  The type of shapefile, i.e., point, polyline, or polygon, is
#' determined by the class of the sp object.
#'
#' @param sp.obj the sp package object or object created by either the grts or
#'   irs functions.
#'
#' @param shpfilename Character string containing the name of the shapefile.
#'   The shapefile name should include the ".shp" extension.  If the name does
#'   not include that extension, it will be added.
#'
#' @return An ESRI shapefile.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' 
#' @export
################################################################################

sp2shape <- function (sp.obj, shpfilename) {

# Ensure that the file extension is ".shp"

  if(!grepl(".shp", shpfilename))
    shpfilename <- paste0(shpfilename, '.shp')

# Create an sf object from the sp object

  sf.object <- st_as_sf(sp.obj)

# Write the sf object as a shapefile

  write_sf(sf.object, shpfilename)

# Return a NULL object

  invisible(NULL)
}
