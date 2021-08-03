###############################################################################
# Function: dframe_to_sf (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform a dframe object into an sf object or a data frame object
#'
#' @description \code{dframe_to_sf()} transforms an sframe object into an \code{sf} object,
#' which may be useful when calling generic functions with sf class (e.g. mapview).
#'
#' @param object A \code{dframe} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} if spatial information is not already
#' present in \code{dframe}
#'
#' @return An \code{sf} object.
#'
#' @importFrom sf st_as_sf
#'
#' @seealso dframe dframe_to_df
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
