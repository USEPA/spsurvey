###############################################################################
# Function: dframe_to_sf (and _df) (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform a dframe object into an sf object or a data frame object
#'
#' @description \code{dframe_to_sf()} transforms an sframe object into an sf object,
#' which may be useful when calling generic functions with sf class (e.g. mapview).
#' \code{dframe_to_df()} transforms an sframe object into an data frame object,
#' while retaining the remaining structure of dframe (e.g. tibble).
#'
#' @param object A \code{dframe} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} if spatial information is not already
#' present in \code{dframe}
#'
#' @return An \code{sf} object (\code{dframe_to_sf()}) or a data frame object (\code{dframe_to_df()}).
#'
#' @seealso dframe
#'
#' @export
###############################################################################
dframe_to_sf <- function(object, ...) {
  if (inherits(object, "sf")) {
    new_object <- structure(object, class = c("sf", setdiff(class(object), c("dframe", "sf"))))
  } else {
    new_object <- st_as_sf(object, ...)
  }
  new_object
}

#' @describeIn dframe_to_sf
#'
#' @export
dframe_to_df <- function(object) {
  new_object <- structure(object, class = c(setdiff(class(object), "data.frame"), "data.frame"))
}
