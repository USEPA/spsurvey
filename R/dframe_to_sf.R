###############################################################################
# Function: dframe_to_sf (and _df) (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform a dframe object into an sf object or a data frame object
#'
#' @description \code{dframe_to_sf()} transforms an sframe object into an \code{sf} object,
#' which may be useful when calling generic functions with sf class (e.g. mapview).
#' \code{dframe_to_df()} transforms a \code{dframe} object into an \code{data.frame} object. If
#' the \code{dframe} object is also an \code{sf} object, \code{dframe_to_df()} stores the
#' coordinates as columns in the new \code{data.frame} object.
#' The remaining structure of \code{dframe} stays the same (e.g. if \code{dframe} is also a tibble,
#' it will remain a tibble after \code{dframe_to_df}).
#'
#' @param object A \code{dframe} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} if spatial information is not already
#' present in \code{dframe}
#'
#' @return An \code{sf} object (\code{dframe_to_sf()}) or a data frame object (\code{dframe_to_df()}).
#'
#' @importFrom sf st_as_sf
#'
#' @seealso dframe
#'
#' @export
###############################################################################
dframe_to_sf <- function(object, ...) {
  if (!inherits(object, "dframe")) {
    stop("object must have class dframe")
  }
  if (inherits(object, "sf")) {
    new_object <- structure(object, class = c("sf", setdiff(class(object), c("dframe", "sf"))))
  } else {
    new_object <- st_as_sf(object, ...)
  }
  new_object
}

#' @describeIn dframe_to_sf
#'
#' @importFrom sf st_coordinates st_drop_geometry
#'
#' @export
dframe_to_df <- function(object) {
  if (!inherits(object, "dframe")) {
    stop("object must have class dframe")
  }
  if (inherits(object, "sf")) {
    coords <- st_coordinates(object)
    object <- cbind(st_drop_geometry(object), coords) # drop geometry removes sf class
  }
  new_object <- structure(object, class = c("data.frame", setdiff(class(object), c("dframe", "data.frame"))))
}
