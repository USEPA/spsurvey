###############################################################################
# Function: examine (exported)
# Programmer: Tom Kincaid
# Date: May 18, 2016
#'
#' Examine Variables in a Data Frame
#'
#' This function examines variables in a data frame by printing either a table
#' or the results of a call to the describe function in the Hmisc package.
#'
#' @param dframe Data frame.
#'
#' @param subpop Character string identifying a variable in \code{dframe} that is
#'   used to group output.  A separate table or call to the describe function is
#'   printed for each unique value in the variable.  The default value is \code{NULL}.
#'
#' @param ord Logical value that controls the order in which the variables in
#'   \code{dframe} are processed.  \code{TRUE} means that variables are processed in sorted
#'   order.  \code{FALSE} means that variables are processed in the order in which they
#'   occur in \code{dframe}.  The default value is \code{TRUE}.
#'
#' @param cmax Numeric value that controls whether a call to table or a call
#'   to describe is used to process variables in \code{dframe}.  If the number of
#'   unique values in a variable is less than or equal to \code{cmax}, then table is
#'   called. If the number of unique values in a variable is greater than \code{cmax},
#'   then \code{describe} is called.  The default value is \code{50}.
#'
#' @return Tables and/or the output from calls to describe are printed.  The
#'   function returns \code{NULL} invisibly.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' df <- data.frame(
#'   SiteID = 1:100,
#'   Catvar = sample(LETTERS[1:5], 100, TRUE),
#'   Contvar = rnorm(100, 10, 1),
#'   Gender = rep(c("Male", "Female"), rep(50, 2))
#' )
#' examine(df, ord = FALSE)
#' examine(df, "Gender", FALSE)
#' @export
###############################################################################

examine <- function(dframe, subpop = NULL, ord = TRUE, cmax = 50) {

  # Assign variable names to the varnames object
  if (ord) {
    varnames <- sort(names(dframe))
  } else {
    varnames <- names(dframe)
  }

  # As necessary, assign subpopulation names to the subnames object
  if (!is.null(subpop)) {
    subnames <- sort(unique(dframe[, subpop]))
  }

  # Examine variables
  for (v in varnames) {
    if (is.null(subpop)) {
      if (length(unique(dframe[, v])) <= cmax) {
        cat(paste("\n\n", v, ":\n", sep = ""))
        print(addmargins(table(dframe[, v], useNA = "ifany")))
      } else {
        cat("\n\n")
        print(describe(dframe[, v], descript = paste(v, ":\n", sep = "")))
      }
    } else {
      if (length(unique(dframe[, v])) <= cmax) {
        if (v != subpop) {
          cat(paste("\n\n", v, " grouped by ", subpop, ":\n", sep = ""))
          print(addmargins(table(dframe[, subpop], dframe[, v], useNA = "ifany")))
        } else {
          cat(paste("\n\n", v, ":\n", sep = ""))
          print(addmargins(table(dframe[, v], useNA = "ifany")))
        }
      } else {
        for (s in subnames) {
          cat("\n\n")
          tempdf <- droplevels(dframe[dframe[, subpop] == s, ])
          print(describe(tempdf[, v], descript = paste(v, " for ", subpop,
            " equal to ", s, ":\n",
            sep = ""
          )))
        }
      }
    }
  }

  # Return a NULL value
  invisible(NULL)
}
