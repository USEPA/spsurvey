###############################################################################
# Function: sframe_to_sf (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform an sframe object into an sf object
#'
#' @description \code{sframe_to_sf()} transforms an sframe object into an sf object,
#' which may be useful when calling generic functions with sf class (e.g. mapview)
#'
#' @param object An \code{sframe} object.
#'
#' @return An \code{sf} object.
#'
#' @seealso sframe
#'
#' @export
###############################################################################
sframe_to_sf <- function(object) {
  if (!inherits(object, "sframe")) {
    stop("object must have class sframe")
  }
  new_unsframe <- structure(object, class = c("sf", setdiff(class(object), c("sframe", "sf"))))
}
