###################################################################################
# Function: sframe
# Programmers: Michael Dumelle
# Date: January 22, 2021
#' Create an sframe object
#'
#' @description sframe gives `sp` and `sf` objects class `"sframe"` to be used
#' in `summary()` and `plot()` generics. `sp` objects are coerced to `sf`
#' objects prior to giving class `sframe`.
#' 
#' @param object An `sp` or `sf` object
#'
#' @return An `sframe` object
#' @export
#'
#' @examples
#' \dontrun{
#' data("NE_lakes")
#' NE_lakes <- sframe(NE_lakes)
#' }
sframe <- function(object) {
  if ("sframe" %in% class(object)) {
    stop("object is already an sframe object")
  }
  if ("sf" %in% class(object)) {
    sframe <- structure(object, class = c("sframe", class(object)))
    return(sframe)
  } else if (!is.null(attr(class(object), "package")) && "sp" %in% attr(class(object), "package")) {
    sframe <- as(object, "sf")
    sframe <- structure(sframe, class = c("sframe", class(sframe)))
    return(sframe)
  } else if ("data.frame" %in% class(object)) {
    sframe <- structure(object, class = c("sframe", class(object)))
    return(sframe)
  } else {
    stop("Input must be an sf object or an sp object")
  }
}
