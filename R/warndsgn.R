################################################################################
# Function: warndsgn
# Programmer: Tony Olsen
# Date: January 22, 2021
#
#' Internal Function: Print the Warnings Data Frame
#'
#' This function prints the warnings data frame.
#'
#' @param warn_df Data frame that contains warning messages.  The default is
#'   "warn_df", which is the name given to the warnings data frame created by
#'   functions in the spsurvey package.
#'
#' @param m Vector of indices for warning messages that are to be printed. The
#'   default is a vector containing the integers from 1 through the number of
#'   rows in warn_df, which will print all warning messages in the data frame.
#'
#' @return Invisible return.  Prints warnings.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

warndsgn <- function(warn_df=get("warn_df", envir = .GlobalEnv),
                     m = 1:nrow(warn_df)) {

  for(i in m) {
    cat(paste("Message", i, ": Stratum:", warn_df$stratum[i],
              "Function: ", warn_df$func[i], "\n"))
    cat(paste("     ", warn_df$warning[i], "\n"))
  }

   for(i in m) {
      cat(paste0("Warning Message ", i, ":\n"))
      cat(paste("Function:", warn_df$func[i], "\n"))
      if(!is.na(warn_df$subpoptype[i]))
         cat(paste("Population Type:", warn_df$subpoptype[i], "\n"))
      if(!is.na(warn_df$subpop[i]))
         cat(paste("Subpopulation:", warn_df$subpop[i], "\n"))
      if(!is.na(warn_df$indicator[i]))
         cat(paste("Indicator:", warn_df$indicator[i], "\n"))
      if(!is.na(warn_df$stratum[i]))
         cat(paste("Stratum:", warn_df$stratum[i], "\n"))
      cat(paste("Warning:", warn_df$warning[i]))
      cat(paste("Action:", warn_df$action[i], "\n"))
   }

   invisible(NULL)
}

