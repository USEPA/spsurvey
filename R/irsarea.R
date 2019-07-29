################################################################################
# Function: irsarea
# Programmer: Tom Kincaid
# Date: November 30, 2005
# Last Revised: May 31, 2019
#'
#' Select an Independent Random Sample (IRS) of an Area Resource
#'
#' This function selects an IRS of an area resource.
#'
#' @param areaframe The sf object containing attributes: id, mdcaty, area_mdm,
#'   and mdm.
#'
#' @param samplesize Number of points to select in the sample.  The default is
#'   100.
#'
#' @param SiteBegin First number to start siteID numbering.  The default is 1.
#'
#' @return  An sf object of sample points containing attributes: siteID, id,
#'   mdcaty, and wgt.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

irsarea <- function (areaframe, samplesize = 100, SiteBegin = 1) {

# Determine IDs for features that will contain sample points

   area.cumsum <- cumsum(areaframe$area_mdm * areaframe$mdm)
   samp.pos <- runif(samplesize, 0, area.cumsum[nrow(areaframe)])
   samp.id <- numeric(samplesize)
   indx <- numeric(samplesize)
   for(j in 1:samplesize) {
      for(i in 1:nrow(areaframe)) {
         if(samp.pos[j] < area.cumsum[i]) {
            samp.id[j] <- areaframe$id[i];
            indx[j] <- i
    	      break;
         }
      }
   }

# Pick sample points

   samp <- st_sample(areaframe[indx,], rep(1, samplesize), exact=TRUE)
   samp <- st_cast(samp, "POINT")

# Create desired attributes

   siteID <- SiteBegin - 1 + 1:length(samp)
   temp <- st_coordinates(samp)
   xcoord <- temp[,1]
   ycoord <- temp[,2]

# Create the output sf object

   rho <- st_sf(siteID=siteID, id=samp.id, xcoord=xcoord, ycoord=ycoord,
      mdcaty=areaframe$mdcaty[indx], wgt=1/areaframe$mdm[indx], geometry=samp)
   row.names(rho) <- 1:nrow(rho)

# Return the sample

   return(rho)
}
