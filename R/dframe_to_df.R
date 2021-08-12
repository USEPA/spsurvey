###############################################################################
# Function: dframe_to_df (exported)
# Programmers: Michael Dumelle
# Date: May 19, 2021
#' Transform a dframe object into a data frame object
#'
#' @description \code{dframe_to_df()} transforms a \code{dframe} object into an \code{data.frame} object. If
#' the \code{dframe} object is also an \code{sf} object, \code{dframe_to_df()} stores the
#' coordinates as columns in the new \code{data.frame} object.
#' The remaining structure of \code{dframe} stays the same (e.g. if \code{dframe} is also a tibble,
#' it will remain a tibble after \code{dframe_to_df()}).
#'
#' @param object A \code{dframe} object.
#'
#' @return A  data frame object (\code{dframe_to_df()}), potentially with coordinates
#' added as extra columns.
#'
#' @importFrom sf st_coordinates st_drop_geometry
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{dframe}}}{ to create a \code{dframe} object}
#'     \item{\code{\link{dframe_to_sf}}}{ to turn a \code{dframe} object into an \code{sf} object}
#'   }
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @examples
#' data("NLA_PNW")
#' NLA_PNW <- dframe(NLA_PNW)
#' NLA_PNW <- dframe_to_df(NLA_PNW)
#' @export
###############################################################################
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
