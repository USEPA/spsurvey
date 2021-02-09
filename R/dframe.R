###################################################################################
# Function: dframe
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an dframe object
#'
#' @description dframe gives data frames, `sp`, and `sf` objects class `"dframe"` to be used
#' in `summary()` generics. `sp` objects are coerced to `sf`
#' objects prior to giving class `dframe`.
#' 
#' @param object An `sp` or `sf` object
#'
#' @return An `dframe` object
#' @export
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
  