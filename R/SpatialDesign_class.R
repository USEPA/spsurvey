################################################################################
# File: SpatialDesign_class
# Programmer: Tom Kincaid
# Date: November 2, 2018
#
#' Class SpatialDesign
#'
#' Create the definition for class \code{SpatialDesign}, a class for spatial survey designs.
#'
#' @name SpatialDesign-class
#'
#' @rdname SpatialDesign-class
#'
#' @exportClass SpatialDesign
#'
#' @slot design Object of class list containing specifications for the survey
#'   design 
#'
#' @slot data Object of class data.frame containing the attribute data 
#'
#' @slot coords.nrs Numeric object that records the column positions in
#'   \code{data} from which the coordinates were obtained 
#'
#' @slot coords Object of class matrix containing the coordinates matrix,
#'   where points are rows in the matrix 
#'
#' @slot bbox Object of class matrix containing the bounding box 
#'
#' @slot proj4string Object of class CRS containing the projection string 
#'
#' @return Object of class \code{SpatialDesign}.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
################################################################################

setClass(Class = "SpatialDesign",
	contains = "SpatialPointsDataFrame",
	slots = c(design = "list"),
	validity = function(object) {
		if(!is.list(object@design))
			stop("The design argument to the SpatialDesign function must be a list.")
		if(length(names(object@design)) == 0)
			stop("The design argument to the SpatialDesign function must be named.")
		return(TRUE)
	}
)

################################################################################
#' Wrapper function for Class SpatialDesign
#'
#' Create the wrapper function for class \code{SpatialDesign}.
#'
#' @name SpatialDesign
#'
#' @rdname SpatialDesign-class
#'
#' @param design Object of class list containing specifications for the
#'   survey design.
#'
#' @param sp_obj Object of class \code{SpatialPointsDataFrame} containing
#'   spatial attributes that have spatial point locations.
#'
#' @export
################################################################################

SpatialDesign <- function(design, sp_obj) {
	new("SpatialDesign", design = design, data = sp_obj@data,
		coords.nrs = sp_obj@coords.nrs, coords = sp_obj@coords, bbox = sp_obj@bbox,
		proj4string = sp_obj@proj4string)
}
