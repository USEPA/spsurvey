################################################################################
# Function: irspts
# Programmer: Tom Kincaid
# Date: November 16, 2005
# Last Revised: June 10, 2019
#'
#' Select an Independent Random Sample (IRS) of a Finite Resource
#'
#' This function selects an IRS of a finite resource (discrete points).
#'
#' @param ptsframe The sf object containing attributes: id, mdcaty, and mdm.
#'
#' @param samplesize Number of points to select in the sample.  The default is
#'   100.
#'
#' @param SiteBegin First number to start siteID numbering.  The default is 1.
#'
#' @return  An sf object of sample points containing attributes: siteID, id,
#'   mdcaty, and wgt.
#'
#' @author Tom Kincaid   \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

irspts <- function(ptsframe, samplesize = 100, SiteBegin = 1) {

  # Pick sample points

   if(nrow(ptsframe) <= samplesize) {
      samp.id <- ptsframe$id
   } else {
      samp.id <- sample(ptsframe$id, samplesize, prob=ptsframe$mdm)
   }
   ind <- ptsframe$id %in% samp.id
   samp <- subset(ptsframe, ind)

# Assign desired attributes

   samp$siteID <- SiteBegin - 1 + 1:nrow(samp)
   temp <- st_coordinates(samp)
   samp$xcoord <- temp[,1]
   samp$ycoord <- temp[,2]
   samp$wgt <- 1/samp$mdm

# Create the output sf object

   rho <- subset(samp, select = c("siteID", "id", "xcoord", "ycoord", "mdcaty",
      "wgt"))
   row.names(rho) <- 1:nrow(rho)

# Return the sample

   return(rho)
}
