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
#' @param ... Additional arguments to \code{st_as_sf()} if converting an sp object to an sf and sframe object
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
sframe <- function(object, ...) {
  if (inherits(object, "sframe")) {
    new_sframe <- structure(object, class = c("sframe", setdiff(object, "sframe")))
  } else if (inherits(object, "sf")) {
    new_sframe <- structure(object, class = c("sframe", class(object)))
  } else if ("sp" %in% attr(class(object), "package")) {
    object_sf <- st_as_sf(object, ...)
    new_sframe <- structure(object_sf, class = c("sframe", class(object_sf)))
  } else {
    stop("Input must be an sf object or an sp object")
  }
  new_sframe
}
