#' \code{sp_frame} objects
#' 
#' @description Turn sampling frames or analysis data into an \code{sp_frame} object
#'   or transform \code{sp_frame} objects back into their original object.
#'
#' @name sp_frame 
#'
#' @param frame A sampling frame or analysis data
#'
#' @return An \code{sp_frame} object.
#' 
#' @details The \code{sp_frame()} function assigns \code{frame} class \code{sp_frame}
#'   to be used by \code{summary()} and \code{plot()}. \code{sp_frame()} objects
#'   can sometimes clash with other sf and tidyverse generics, so \code{un_spframe()} removes
#'   class \code{sp_frame()}, leaving the original classes of \code{frame} intact.
#' 
#' @export
#'
#' @examples
#' NE_Lakes <- sp_frame(NE_Lakes)
#' class(NE_Lakes)
#' NE_Lakes <- sp_unframe(NE_Lakes)
#' class(NE_Lakes)
sp_frame <- function(frame) {
  new_sp_frame <- structure(frame, class = c("sp_frame", class(frame)))
}

#' @name sp_frame
#' @param sp_frame An \code{sp_frame} object.
#' @export
sp_unframe <- function(sp_frame) {
  new_sp_unframe <- structure(sp_frame, class = setdiff(class(sp_frame), "sp_frame"))
}