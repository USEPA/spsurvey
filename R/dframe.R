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
    stop("object is already an dframe object")
  }
  if ("sf" %in% class(object)) {
    dframe <- structure(object, class = c("dframe", class(object)))
    return(dframe)
  } else if (!is.null(attr(class(object), "package")) && "sp" %in% attr(class(object), "package")) {
    dframe <- as(object, "sf")
    dframe <- structure(dframe, class = c("dframe", class(dframe)))
    return(dframe)
  } else if ("data.frame" %in% class(object)) {
    dframe <- structure(object, class = c("dframe", class(object)))
    return(dframe)
  } else {
    stop("Input must be an sf object, sp object, or data frame")
  }
}
