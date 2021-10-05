###############################################################################
# Function: sframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an sframe object
#'
#' @description \code{sframe()} is used before summarizing or visualizing sample frames
#' (this function need not be run before calling \code{grts()} or \code{irs()}).
#'   \code{sframe()} gives \code{sp} and \code{sf} objects class \code{"sframe"}.
#'   \code{sp} objects are coerced to \code{sf}
#'   objects prior to giving class \code{sframe}.
#'
#' @param object For \code{sframe}, an \code{sp} or \code{sf} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} when using \code{sframe()}
#' to convert an \code{sp} object to an \code{sf} and \code{sframe} object.
#'
#' @return A \code{sframe} object that can be used by
#' spsurvey's \code{summary()} and \code{plot()} generics.
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{sframe_to_sf}}}{ to turn an \code{sframe} object into an \code{sf} object}
#'  }
#'
#' @export
#'
#' @examples
#' data("NE_Lakes")
#' NE_Lakes <- sframe(NE_Lakes)
#' summary(NE_Lakes, formula = ~ ELEV_CAT)
#' plot(NE_Lakes, formula = ~ ELEV_CAT)
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
