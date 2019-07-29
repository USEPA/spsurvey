################################################################################
# Function: grtspts.r
# Programmers: Tony Olsen, Tom Kincaid, Don Stevens, Christian Platt,
#   			Denis White, Richard Remington
# Date: October 8, 2002
# Last Revised: June 10, 2019
#
#' Select a Generalized Random-Tesselation Stratified (GRTS) Sample of a Finite Resource
#'
#' This function select a GRTS sample of a finite resource.  This function uses
#' hierarchical randomization to ensure that the sample will include no more
#' than one point per grid cell and then picks a point in selected cells.
#'
#' @param ptsframe The sf object containing attributes: id, x, y, mdcaty, and
#'   mdm.
#'
#' @param samplesize Number of points to select in the sample.  The default is
#'   100.
#'
#' @param SiteBegin First number to start siteID numbering.  The default is 1.
#'
#' @param shift.grid Option to randomly shift the hierarchical grid.  The
#'   default is TRUE.
#'
#' @param do.sample Option to select a sample, where TRUE means select a
#'   sample and FALSE means return the entire sample frame in reverse
#'   hierarchical order. The default is TRUE.
#'
#' @param startlev Initial number of hierarchical levels to use for the GRTS
#'   grid, which must be less than or equal to maxlev (if maxlev is specified)
#'   and cannot be greater than 11.  The default is NULL.
#'
#' @param maxlev Maximum number of hierarchical levels to use for the GRTS
#'   grid, which cannot be greater than 11.  The default is 11.
#'
#' @return Data frame of sample points containing: siteID, id, x, y, mdcaty,
#'   and weight.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{numLevels}}{determines the number of levels for hierarchical
#'       randomization}
#'     \item{\code{constructAddr}}{constructs the hierarchical address for
#'       sample points}
#'     \item{\code{ranho}}{constructs the randomized hierarchical address for
#'       sample points}
#'     \item{\code{pickGridCells}}{selects grid cells that get a sample point}
#'     \item{\code{\link{pickFiniteSamplePoints}}}{pick sample point(s) from
#'       selected cells}
#'   }
#'
#' @author
#'  Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'  Tony Olsen \email{Olsen.Tony@epa.gov}\cr
#'
#' @keywords survey
#'
#' @export
################################################################################

grtspts <- function(ptsframe, samplesize = 100, SiteBegin = 1,
   shift.grid = TRUE, do.sample = TRUE, startlev = NULL, maxlev = 11) {

# Determine the number of levels for hierarchical randomization

      temp <- numLevels(samplesize, shift.grid,
         startlev, maxlev, ptsframe)
      nlev <- temp$nlev
      dx <- temp$dx
      dy <- temp$dy
      xc <- temp$xc
      yc <- temp$yc
      cel.wt <- temp$cel.wt
      sint <- temp$sint

# Assign the final number of levels

   endlev <- nlev - 1

# Remove cells with zero weight

   indx <- cel.wt > 0
   xc <- xc[indx]
   yc <- yc[indx]
   cel.wt <- cel.wt[indx]

# Construct the hierarchical address for all cells

   hadr <- constructAddr(xc, yc, dx, dy, nlev)

# Construct randomized hierarchical addresses

   ranhadr <- ranho(hadr)

# Determine order of the randomized hierarchical addresses

   rord <- order(ranhadr)

   if(do.sample) {

# Select grid cells that get a sample point

      rstrt <- runif(1, 0, sint)
      ttl.wt <- c(0, cumsum(cel.wt[rord]))
      idx <- ceiling((ttl.wt - rstrt)/sint)
      smpdx <- pickGridCells(samplesize, idx)
      rdx <- rord[smpdx]
      n.cells <- length(unique(rdx))
      if(length(rdx) > n.cells) {
         temp <- sum(sapply(split(rdx, rdx), length) > 1)
         warning(paste("\nOf the ", n.cells, " grid cells from which sample points were selected,\n", temp, " (", round(100*temp/n.cells, 1), "%) of the cells contained more than one sample point.\n", sep=""))
      }

# Pick sample point(s) in selected cells

      id <- pickFiniteSamplePoints(rdx, xc, yc, dx, dy, ptsframe)
      rho <- ptsframe[match(id, ptsframe$id), ]

   } else {

# Pick all points in the frame

      id <- selectframe(rord, xc, yc, dx, dy, ptsframe)
      rho <- ptsframe[match(id, ptsframe$id), ]
   }

# Construct sample hierarchical address

   np <- nrow(rho)
   nlev <- max(1, trunc(logb(np,4)))
   ifelse(np == 4^nlev, nlev, nlev <- nlev + 1)
   ad <- matrix(0, 4^nlev, nlev)
   rv4 <- 0:3
   pwr4 <- 4.^(0.:(nlev - 1.))
   for(i in 1:nlev)
      ad[, i] <- rep(rep(rv4, rep(pwr4[i], 4.)),pwr4[nlev]/pwr4[i])
   rho4 <- as.vector(ad%*%matrix(rev(pwr4), nlev, 1))

# Place sample in reverse hierarchical order

   rho <- rho[unique(floor(rho4 * np/4^nlev)) + 1.,]

# Create desired attributes

   rho$siteID <- SiteBegin - 1 + 1:nrow(rho)
   temp <- st_coordinates(rho)
   rho$xcoord <- temp[,1]
   rho$ycoord <- temp[,2]
   rho$wgt <- 1/rho$mdm

# Create the output sf object

   rho <- subset(rho, select = c("siteID", "id", "xcoord", "ycoord", "mdcaty",
      "wgt"))
   row.names(rho) <- 1:nrow(rho)

# Assign the final number of levels as an attribute of the output object

   attr(rho, "nlev") <- endlev

# Return the sample

   rho
}
