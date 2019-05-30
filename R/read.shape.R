################################################################################
# Function: read.shape
# Programmer: Tom Kincaid
# Date: March 1, 2005
# Last Revised: May 21, 2019
#'
#' Read an ESRI Shapefile
#'
#' This function reads an ESRI shapefile and creates a simple features (sf)
#'   object.
#'
#' @param filename Character string containing the name of the shapefile.  The
#'   shapefile name should include the ".shp" extension.  If the name does not
#'   include that extension, it will be added.
#'
#' @return An object belonging to class sf.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @author Marc Weber  \email{Weber.Marc@epa.gov}
#' 
#' @export
################################################################################

read.shape <- function(filename) {

# Ensure that the file extension is ".shp"

   if(!grepl(".shp",filename))
      filename <- paste0(filename, '.shp')

# Read the shapefile

   sf.object <- st_read(filename, quiet = TRUE)

# Return the sf object

   return(sf.object)
}
