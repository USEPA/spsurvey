###############################################################################
# Function: errorprnt (exported)
# Programmer: Tom Kincaid
# Date: June 24, 2020
#
#' Print errors from analysis functions
#'
#' This function prints the error messages vector in the analysis functions.
#'
#' @param error_vec Data frame that contains error messages.  The default is
#'   \code{"error_vec"}, which is the name given to the error messages vector created
#'   by functions in the spsurvey package.
#'
#' @return Printed errors.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @export
###############################################################################

errorprnt <- function(error_vec = get("error_vec", envir = .GlobalEnv)) {
  m <- 1:length(error_vec)
  for (i in m) {
    cat(paste0("Error Message ", i, ":\n"))
    cat(paste(error_vec[i], "\n"))
  }

  invisible(NULL)
}
