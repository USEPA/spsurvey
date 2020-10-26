################################################################################
# Function: errorprnt
# Programmer: Tom Kincaid
# Date: June 24, 2020
#
#' Internal Function: Print the Error Messages Vector
#'
#' This function prints the error messages vector.
#'
#' @param error.vec Data frame that contains error messages.  The default is
#'   "error.vec", which is the name given to the error messagess vector created
#'   by functions in the spsurvey package.
#'
#' @return Invisible return.  Prints errors.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

errorprnt <- function(error.vec=get("error.vec", envir = .GlobalEnv)) {

  m <- 1:length(error.vec)
  for(i in m) {
    cat(paste0("Error Message ", i, ":\n"))
    cat(paste(error.vec[i], "\n"))
  }

  invisible(NULL)
}
