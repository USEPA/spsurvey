################################################################################
# Function: pickGridCells
# Programmer: Tom Kincaid
# Date: April 30, 2019
#
#' Identify Grid Cells from which Sample Points Will Be Selected
#'
#' This function identifies grid cells from which sample points will be selected.
#'
#' @param samplesize The desired smaple size.
#'
#' @param idx Vector of values for identifying cell IDs.
#'
#' @return Vector of grid cells IDs.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickGridCells <- function(samplesize, idx) {

# Create the vector for results

  smpdx <- numeric(samplesize)

# Identify the grid cells

  j = 0;
  for (i in 1:samplesize) {
    while (idx[j+1] < i) {
      j <- j+1;
    }
    smpdx[i] = j;
  }

# Return the values identifying cell IDs
 
  return(smpdx)
}
