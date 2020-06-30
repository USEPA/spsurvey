################################################################################
# Function: stopprnt
# Programmer: Tony Olsen
# Date: May 1, 2020
#
#' Internal Function: Print the stop data Frame that contains errors for the design.
#'
#' @param stop.df Data frame that contains stop messages.  The default is
#'   "stop.df", which is the name given to the stop data frame created by
#'   functions in the spsurvey package.
#'
#' @param m Vector of indices for stop messages that are to be printed. The
#'   default is a vector containing the integers from 1 through the number of
#'   rows in stop.df, which will print all stop messages in the data frame.
#'
#' @return Invisible return.  Prints stop messages
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

stopprnt <- function(stop.df=get("stop.df", envir = .GlobalEnv),
                     m = 1:nrow(stop.df)) {
  cat(paste("Input      ", "Error Message\n"))

  for(i in m) {
    cat(paste(stop.df[i, 1], ": ", stop.df[i, 2], "\n"))
  }

  invisible(NULL)
}
