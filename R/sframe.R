sframe <- function(object) {
  if ("sf" %in% class(object)) {
    sframe <- structure(object, class = c("sframe", class(object)))
    return(sframe)
  } else if ("sp" %in% attr(class(object), "package")) {
    sframe <- as(object, "sf")
    sframe <- structure(sframe, class = c("sframe", class(sframe)))
    return(sframe)
  } else {
    stop("Argument must be an sf object or an sp object")
  }
}
