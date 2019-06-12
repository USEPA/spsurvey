################################################################################
# Function: numLevels
# Programmer: Tom Kincaid
# Date: June 4, 2019
#
#' Determine the Number of Levels for Hierarchical Randomization for a
#' Generalized Random-Tesselation Stratified (GRTS) Survey Design
#'
#' This function determine the bumber of levels of hierarchical randomization
#'  for a GRTS survey design.
#'
#' @param samplesize The desired smaple size.
#'
#' @param shift.grid Logical value indicating whether the GRTS grid should be
#'  randomly shifted.
#'
#' @param startlev The initial number of levels for the GRTS grid.
#'
#' @param maxlev The maximum number of levels for the GRTS grid.
#'
#' @param sfobject The sf object containing the survey frame.
#'
#' @return A list containing the number of levels, x-coordinates, y-coordinates,
#'  x-axis grid cell dimension, y-axis grid cell dimension, cell total weights,
#'  and sampling interval.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cellWeight}}}{calculates total inclusion probability
#'       for each cell in a grid}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

numLevels <- function(samplesize, shift.grid, startlev, maxlev, sfobject) {

# Determine the number of levels for hierarchical randomization

      if(is.null(startlev)) {
         nlev <- ceiling(logb(samplesize, 4))
         if(nlev == 0)
            nlev <- 1
      } else {
         nlev <- startlev
      }
      cel.wt <- 99999
      celmax.ind <- 0
      sint <- 1
      if(shift.grid) {
         roff.x <- runif(1, 0, 1)
         roff.y <- runif(1, 0, 1)
      }
      bbox <- st_bbox(sfobject)
      grid.extent <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
      temp <- 0.04*grid.extent
      grid.xmin <- bbox$xmin - temp
      grid.ymin <- bbox$ymin - temp
      grid.extent <- 1.08*grid.extent
      grid.xmax <- grid.xmin + grid.extent
      grid.ymax <- grid.ymin + grid.extent
      while (any(cel.wt/sint > 1) && celmax.ind < 2 && nlev <= maxlev) {
         cat( "Current number of levels:", nlev, "\n");
         celmax <- max(cel.wt)
         nlv2 <- 2^nlev
         dx <- dy <- grid.extent/nlv2
         xc <- seq(grid.xmin, grid.xmax, length=nlv2+1)
         yc <- seq(grid.ymin, grid.ymax, length=nlv2+1)
         if(shift.grid) {
            xc <- rep(xc, nlv2+1) + (roff.x * dx)
            yc <- rep(yc, rep(nlv2+1, nlv2+1)) + (roff.y * dy)
         } else {
            xc <- rep(xc, nlv2+1)
            yc <- rep(yc, rep(nlv2+1, nlv2+1))
         }

# Determine total inclusion probability for each grid cell and, as necessary,
# adjust the indicator for whether maximum of the total inclusion probabilities
# is changing

         cel.wt <- cellWeight(xc, yc, dx, dy, sfobject)
         if(max(cel.wt) == celmax) {
            celmax.ind <- celmax.ind + 1
    	       if(celmax.ind == 2)
    	          warning("\nSince the maximum value of total inclusion probability for the grid cells was \nnot changing, the algorithm for determining the number of levels for \nhierarchical randomization was terminated.\n")
         }

# Adjust sampling interval and number of hierarchical levels

         sint <- sum(cel.wt)/samplesize
         if(nlev == maxlev) {
            nlev <- nlev + 1
         } else {
            nlev <- nlev + max(1, ceiling(logb(cel.wt[cel.wt > 0]/sint, 4)))
            if(nlev > maxlev) {
               nlev <- maxlev
            }
         }
      }

#  Print the final number of levels

      cat( "Final number of levels:", nlev-1, "\n");

# Create the output list

rslt <- list(nlev=nlev, xc=xc, yc=yc, dx=dx, dy=dy, cel.wt=cel.wt, sint=sint)

# Return the list
 
  return(rslt)
}
