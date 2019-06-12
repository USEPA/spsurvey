################################################################################
# Function: selectFeatureID
# Programmers: Tom Kincaid
# Date: April 26, 2019
#
#' Identify a Feature in an sf Object for Selecting a Sample Point
#'
#' This function determines the ID value of the feature in an sf object from
#' which a sample point will be selected.
#'
#' @param rdx Value of the randomized hierarchical address identifying a grid
#'   cell that will get a sample point.
#'
#' @param cellID Vector of grid cell IDs.
#'
#' @param featureMeasure Vector of grid cell sf feature lengths for
#'   linestring objects or sf feature areas for polygon objects.
#'
#' @param featureID Vector of grid cell sf feature IDs.
#'
#' @param mdm Vector of multidensity multipliers for the shapefile features.
#'
#' @param id Vector of sf feature IDs.
#'
#' @return The ID of an sf feature.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

selectFeatureID <- function(rdx, cellID, featureMeasure, featureID, mdm, id) {

   ind <- rdx == cellID
   nrec <- sum(ind)
   if(nrec > 1) {
      temp <- featureMeasure[ind] * mdm[match(featureID[ind], id)]
      probs <- temp/sum(temp)
      rslt <- sample(featureID[ind], 1, prob=probs)
   } else {
      rslt <- featureID[ind]
   }
   return(rslt)
}
