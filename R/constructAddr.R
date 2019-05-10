################################################################################
# Function: constructAddr
# Programmer: Tom Kincaid
# Date: May 10, 2019
#
#' Construct the Hierarchical Addresses for a Generalized Random-Tesselation
#' Stratified (GRTS) Survey Design
#'
#' This function constructS the hierarchical addresses for a GRTS survey design.
#'
#' @param xc Vector of x-coordinates for the grid cells.
#'
#' @param yc Vector of y-coordinates for the grid cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @param nlev Number of hierarchical levels for the grid.
#'
#' @return Vector of hierarchical addresses.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

constructAddr <- function(xc, yc, dx, dy, nlev) {
  
  # Construct the matrix containing hierarchical address values
  
  hadrmat <- matrix(0, length(xc), nlev)
  x <- ceiling(xc/dx)
  y <- ceiling(yc/dy)
  for (j in nlev:1){
    hadrmat[,j] <- 2 * (x %% 2) + (y %% 2) + 1
    x <- x %/% 2
    y <- y %/% 2
  }
  
  # Paste the row values in hadrmat to construct the hierarchical addresses
  
  hadr <- apply(hadrmat, 1, paste, collapse="")
  
  # Return the addresses
  
  return(hadr)
}


