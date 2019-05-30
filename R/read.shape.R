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
#'
#' @export
################################################################################

read.shape <- function(filename) {

# Ensure that the file extension is ".shp"

   nc <- nchar(filename)
   if(substr(filename, nc-3, nc) != ".shp") {
      if(substr(filename, nc-3, nc-3) == ".") {
         filename <- paste(substr(filename, 1, nc-4), ".shp", sep="")
      } else {
         filename <- paste(filename, ".shp", sep="")
      }
   }

# Read the shapefile

   sf.object <- st_read(filename, quiet = TRUE)

# Return the sf object

   return(sf.object)
}
