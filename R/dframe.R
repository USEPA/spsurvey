###############################################################################
# Function: dframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an dframe object
#'
#' @description \code{dframe()} is used before summarizing or visualizing
#'   data that has been collected (this function need not be run before calling any analysis functions).
#'   \code{dframe()} gives data frames, \code{sp} objects, and \code{sf}
#'   objects class \code{dframe}. \code{sp} objects are coerced to \code{sf}
#'   objects prior to giving class \code{dframe}.
#'
#' @param object An \code{sp} or \code{sf} object.
#'
#' @param ... Additional arguments to \code{st_as_sf()} if converting an \code{sp}
#' object to an \code{sf} and \code{dframe} object.
#'
#' @return A \code{dframe} object that can be used by
#' spsurvey's \code{summary()} and \code{plot()} generics.
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{dframe_to_sf}}}{ to turn a \code{dframe} object into an \code{sf} object}
#'     \item{\code{\link{dframe_to_df}}}{ to turn a \code{dframe} object into a data frame}
#'   }
#'
#' @author Michael Dumelle \email{Dumelle.Michael@@epa.gov}
#'
#' @export
#'
#' @examples
#' data("NLA_PNW")
#' NLA_PNW <- dframe(NLA_PNW)
#' summary(NLA_PNW, formula = ~ BMMI_COND)
#' plot(NLA_PNW, formula = ~ BMMI_COND)
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
