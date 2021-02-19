###############################################################################
# Function: vecprint (exported)
# Programmer: Tom Kincaid
# Date: February 6, 2004
# Last Revised:  May 3, 2004
#
#' Internal Function: Create Vector to Print
#'
#' This function takes an input vector and outputs a character string with line
#' breaks inserted so that, whenever possible, no line in the string exceeds the
#' input value n_char, which is set to 78 characters by default.  The input
#' vector is coereced to mode character.  When an element of the input vector is
#' greater than n_char characters in length, then that element is inserted in
#' the output character string as an individual line.
#'
#' @param x Character vector.
#'
#' @param n_char The maximum number of characters per line.  The default is \code{78}.
#'
#' @return  Character string that is suitable for printing by the functions:
#'   \code{stop}, \code{warning}, or \code{cat}.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @examples
#' sites <- paste("Site Number", 1:50)
#' sites_str <- vecprint(sites)
#' cat(sites_str)
#'
#' temp <- c(1, 5, 21:25, 33:37)
#' sites_str <- vecprint(sites[temp])
#' warning(paste("\nThe following site ID values were removed from the
#'   analysis:\n", sites_str, sep=""))
#'
#' @export
###############################################################################

vecprint <- function(x, n_char = 78) {

   x <- as.character(x)
   n <- length(x)
   nc <- nchar(x)

   i <- 1
   j <- 1
   nc_sum <- 0
   x_str <- character()
   while(j <= n) {
      if((nc_sum + nc[j]) <= n_char) {
         nc_sum <- nc_sum + nc[j] + 2
         j <- j+1
      } else {
         if(i == j) {
            x_str <- paste(x_str, x[j], "\n")
         } else {
            j <- j-1
            x_str <- paste(x_str, paste(x[i:j], collapse=", "), "\n")
         }
         j <- j+1
         i <- j
         nc_sum <- 0
      }
   }
   if(i < j) {
      j <- j-1
      x_str <- paste(x_str, paste(x[i:j], collapse=", "), "\n")
   }

   x_str
}

