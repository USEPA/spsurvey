###############################################################################
# Function: sframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an sframe object
#'
#' @description \code{sframe()} gives \code{sp} and \code{sf} objects class \code{"sframe"} to be used
#' in spsurvey's \code{summary()} and \code{plot()} generics. \code{sp} objects are coerced to \code{sf}
#' objects prior to giving class \code{sframe}. \code{unsframe()} removes class \code{"sframe"},
#' which may be useful when calling sf generic functions.
#'
#' @param object For \code{sframe}, an \code{sp} or \code{sf} object. For \code{unsframe()}, an
#' \code{sframe} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} when using \code{sframe()}
#' to convert an \code{sp} object to an \code{sf} and \code{sframe} object
#'
#' @return An \code{sframe} object.
#'
#' @seealso sframe_to_sf
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
    new_sframe <- structure(object, class = c("sframe", setdiff(class(object), "sframe")))
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
