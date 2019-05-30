################################################################################
# Function: read.dbf
# Programmer: Tom Kincaid
# Date: March 1, 2005
# Last Revised: May 21, 2019
#
#' Read the dbf File of an ESRI Shapefile
#'
#' This function reads the dbf file of an ESRI shapefile and creates a data
#'   frame.
#'
#' @param filename  Character string containing the name of the shapefile.
#'
#' @return Data frame containing contents of the dbf file.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

read.dbf <- function(filename) {

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

   dbffile <- st_read(filename, quiet = TRUE)

# Drop the geometry column from dbffile to create a data frame

   dbffile <- st_set_geometry(dbffile, NULL)

# Return the data frame

   return(dbffile)
}
