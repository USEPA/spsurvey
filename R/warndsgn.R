################################################################################
# Function: warndsgn
# Programmer: Tony Olsen
# Date: September 9, 2020
#
#' Internal Function: Print the warnings data Frame that contains errors for the design.
#'
#' @param warn.df Data frame that contains warning messages.  The default is
#'   "warn.df", which is the name given to the warnings data frame created by
#'   functions in the spsurvey package.
#'
#' @param m Vector of indices for stop messages that are to be printed. The
#'   default is a vector containing the integers from 1 through the number of
#'   rows in stop.df, which will print all stop messages in the data frame.
#'
#' @return Invisible return.  Prints warning messages
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

warndsgn <- function(warn.df=get("warn.df", envir = .GlobalEnv),
                     m = 1:nrow(warn.df)) {

  for(i in m) {
    cat(paste("Message", i, ": Stratum:", warn.df$stratum[i],
              "Function: ", warn.df$func[i], "\n"))
    cat(paste("     ", warn.df$warning[i], "\n"))
  }

  invisible(NULL)

}

