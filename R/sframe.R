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
    new_sframe <- structure(object, class = c("sframe", class(object)[-which(class(object) == "sframe")]))
    return(new_sframe)
  }
  if ("sf" %in% class(object)) {
    new_sframe <- structure(object, class = c("sframe", class(object)))
    return(new_sframe)
  } else if (!is.null(attr(class(object), "package")) && "sp" %in% attr(class(object), "package")) {
    object_sf <- st_as_sf(object)
    new_sframe <- structure(st_as_sf(object), class = c("sframe", class(object_sf)))
    return(new_sframe)
  } else {
    stop("Input must be an sf object or an sp object")
  }
}
