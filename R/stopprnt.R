###############################################################################
# Function: stopprnt (exported)
# Programmer: Tony Olsen
# Date: January 22, 2021
#
#' Internal Function: Print the stop data Frame that contains errors for the design.
#'
#' @param stop_df Data frame that contains stop messages.  The default is
#'   \code{stop_df}, which is the name given to the stop data frame created by
#'   functions in the spsurvey package.
#'
#' @param m Vector of indices for stop messages that are to be printed. The
#'   default is a vector containing the integers from 1 through the number of
#'   rows in \code{stop_df}, which will print all stop messages in the data frame.
#'
#' @return Invisible return.  Prints stop messages
#'
#' @author Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @export
###############################################################################

stopprnt <- function(stop_df = get("stop_df", envir = .GlobalEnv),
                     m = 1:nrow(stop_df)) {
  cat(paste("Input      ", "Error Message\n"))

  for (i in m) {
    cat(paste(stop_df[i, 1], ": ", stop_df[i, 2], "\n"))
  }

  invisible(NULL)
}
