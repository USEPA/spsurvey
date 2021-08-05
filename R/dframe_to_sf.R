###############################################################################
# Function: dframe_to_sf (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform a dframe object into an sf object
#'
#' @description \code{dframe_to_sf()} transforms an \code{sframe} object into an \code{sf} object,
#' which may be useful when calling generic functions with sf class (e.g. mapview::mapview).
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
#' @seealso
#'   \describe{
#'     \item{\code{\link{dframe}}}{ to create a \code{dframe} object}
#'     \item{\code{\link{dframe_to_df}}}{ to turn a \code{dframe} object into a data frame}
#'   }
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @examples
#' data("NLA_PNW")
#' NLA_PNW <- dframe(NLA_PNW)
#' NLA_PNW <- dframe_to_sf(NLA_PNW)
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
