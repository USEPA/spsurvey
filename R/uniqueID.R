###############################################################################
# Function: uniqueID (not exported)
# Programmer: Tom Kincaid
# Date: February 23, 2004
# Last Revised: June 27, 2005
#
#' Internal Function: Create Unique IDs for a Survey Design
#'
#' This function creates unique site IDs by appending a unique number to each
#' occurrence of a site ID.  It is intended for survey designs that have repeat
#' visits to sites.
#'
#' @param siteID  Vector of site IDs.
#'
#' @return Vector of unique site IDs.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @noRd
###############################################################################

uniqueID <- function(siteID) {

  # If siteID is a factor, convert the input vector to character data

  factor.ind <- FALSE
  if (is.factor(siteID)) {
    siteID <- as.character(siteID)
    factor.ind <- TRUE
  }

  # Assign new site IDs

  for (id in unique(siteID)) {
    temp <- siteID == id
    if (sum(temp) > 1) {
      siteID[temp] <- paste(siteID[temp], 1:sum(temp), sep = ".")
    }
  }

  # If siteID is a factor, convert the result to a factor

  if (factor.ind) {
    siteID <- as.factor(siteID)
  }

  # Return the vector of site IDs

  siteID
}
