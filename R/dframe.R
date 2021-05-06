###############################################################################
# Function: dframe (exported)
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an dframe object
#'
#' @description dframe gives data frames, \code{sp}, and \code{sf} objects class \code{dframe} to be used
#' in \code{summary()} generics. \code{sp} objects are coerced to \code{sf}
#' objects prior to giving class \code{dframe}.
#'
#' @param object An \code{sp} or \code{sf} object
#'
#' @return An \code{dframe} object
#'
#' @export
#'
#' @author Michael Dumelle
#'
#' @examples
#' \dontrun{
#' data("NE_lakes")
#' NE_lakes <- dframe(NE_lakes)
#' }
dframe <- function(object) {
  if ("dframe" %in% class(object)) {
    new_dframe <- structure(object, class = c("dframe", class(object)[-which(class(object) == "dframe")]))
    return(new_dframe)
  }
  if ("sf" %in% class(object)) {
    new_dframe <- structure(object, class = c("dframe", class(object)))
    return(new_dframe)
  } else if (!is.null(attr(class(object), "package")) && "sp" %in% attr(class(object), "package")) {
    object_sf <- st_as_sf(object)
    new_dframe <- structure(dframe, class = c("dframe", class(object_sf)))
    return(new_dframe)
  } else if ("data.frame" %in% class(object)) {
    new_dframe <- structure(object, class = c("dframe", class(object)))
    return(new_dframe)
  } else {
    stop("Input must be an sf object, sp object, or data frame")
  }
}
