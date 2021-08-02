###############################################################################
# Function: dframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an dframe object
#'
#' @description \code{dframe} gives data frames, \code{sp}, and \code{sf} objects class \code{dframe} to be used
#' in \code{summary()} generics. \code{sp} objects are coerced to \code{sf}
#' objects prior to giving class \code{dframe}.
#'
#' @param object An \code{sp} or \code{sf} object
#'
#' @param ... Additional arguments to \code{st_as_sf()} if converting an \code{sp}
#' object to an \code{sf} and \code{dframe} object
#'
#' @return A \code{dframe} object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("NE_lakes")
#' NE_lakes <- dframe(NE_lakes)
#' }
dframe <- function(object, ...) {
  if (inherits(object, "dframe")) {
    new_dframe <- structure(object, class = c("dframe", setdiff(class(object), "dframe")))
  } else if (inherits(object, "sf")) {
    new_dframe <- structure(object, class = c("dframe", class(object)))
  } else if ("sp" %in% attr(class(object), "package")) {
    object_sf <- st_as_sf(object, ...)
    new_dframe <- structure(object_sf, class = c("dframe", class(object_sf)))
  } else if (inherits(object, "data.frame")) {
    new_dframe <- structure(object, class = c("dframe", class(object)))
  } else {
    stop("Input must be an sf object or an sp object")
  }
  new_dframe
}
