###############################################################################
# Function: sframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an sframe object
#'
#' @description sframe gives \code{sp} and \code{sf} objects class \code{"sframe"} to be used
#' in \code{summary()} and \code{plot()} generics. \code{sp} objects are coerced to \code{sf}
#' objects prior to giving class \code{sframe}.
#'
#' @param object An \code{sp} or \code{sf} object
#'
#' @return An \code{sframe} object
#' 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("NE_lakes")
#' NE_lakes <- sframe(NE_lakes)
#' }
#' ###############################################################################
sframe <- function(object) {
  if ("sframe" %in% class(object)) {
    stop("object is already an sframe object")
  }
  if ("sf" %in% class(object)) {
    sframe <- structure(object, class = c("sframe", class(object)))
    return(sframe)
  } else if (!is.null(attr(class(object), "package")) && "sp" %in% attr(class(object), "package")) {
    sframe <- st_as_sf(object)
    sframe <- structure(sframe, class = c("sframe", class(sframe)))
    return(sframe)
  } else {
    stop("Input must be an sf object or an sp object")
  }
}
