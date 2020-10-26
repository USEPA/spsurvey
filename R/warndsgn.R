################################################################################
# Function: warndsgn
# Programmer: Tony Olsen
# Date: September 9, 2020
#
#' Internal Function: Print the Warnings Data Frame
#'
#' This function prints the warnings data frame.
#'
#' @param warn.df Data frame that contains warning messages.  The default is
#'   "warn.df", which is the name given to the warnings data frame created by
#'   functions in the spsurvey package.
#'
#' @param m Vector of indices for warning messages that are to be printed. The
#'   default is a vector containing the integers from 1 through the number of
#'   rows in warn.df, which will print all warning messages in the data frame.
#'
#' @return Invisible return.  Prints warnings.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
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

   for(i in m) {
      cat(paste0("Warning Message ", i, ":\n"))
      cat(paste("Function:", warn.df$func[i], "\n"))
      if(!is.na(warn.df$subpoptype[i]))
         cat(paste("Population Type:", warn.df$subpoptype[i], "\n"))
      if(!is.na(warn.df$subpop[i]))
         cat(paste("Subpopulation:", warn.df$subpop[i], "\n"))
      if(!is.na(warn.df$indicator[i]))
         cat(paste("Indicator:", warn.df$indicator[i], "\n"))
      if(!is.na(warn.df$stratum[i]))
         cat(paste("Stratum:", warn.df$stratum[i], "\n"))
      cat(paste("Warning:", warn.df$warning[i]))
      cat(paste("Action:", warn.df$action[i], "\n"))
   }

   invisible(NULL)
}

