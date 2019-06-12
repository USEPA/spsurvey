################################################################################
# Function: input.format
# Programmer: Tom Kincaid
# Date: January 25, 2002
# Last Revised: May 7, 2010
#'
#' Format an Input Value
#'
#' This function formats an input value of class numeric, character, or factor.
#' For a numeric value, the number of digits after the decimal point can be
#' specified.  A factor value is converted to character.  Missing values are
#' allowed.
#'
#' @param x The input value.
#'
#' @param n.digits The number of digits after the decimal point, which can be
#'   zero. The default is 2.
#'
#' @param miss The missing value code expressed as a character string.  The
#'   default is "NA".
#'
#' @return A value of mode character that is one of the following, as
#'   appropriate: (1) character representation of a real number with the
#'   specified number of digits after the decimal point when the input numeric
#'   value is a real number, (2) character representation of an integer when the
#'   input numeric value is an integer, (3) the original value when the input
#'   value is class character or factor, or (4) the missing value code when the
#'   input value is missing.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

input.format <- function(x, n.digits = 2, miss = "NA") {

# Convert a factor input value to character
if(is.factor(x))
   x <- as.character(x)

# This section handles numeric values
if(is.numeric(x)) {
   if(is.na(x)) {
      rslt <- miss
   } else if(n.digits == 0) {
      rslt <- format(round(x, 0))
   } else {
      x.int <- ifelse(x >= 0, floor(x), ceiling(x))
      if(x.int == 0) {
         if(x == 0) {
            rslt <- "0"
         } else {
            rslt <- format(round(x, n.digits))
            nd <- ifelse(x >= 0, nchar(rslt) - 2, nchar(rslt) - 3)
            if(nd != n.digits) {
               if(rslt == "0")
                  rslt <- paste("0.", paste(rep("0", n.digits), collapse=""),
                     sep="")
               else
                  rslt <- paste(rslt, paste(rep("0", n.digits - nd),
                     collapse=""), sep="")
            }
         }
      } else {
         if((x %% x.int) == 0) {
            rslt <- format(round(x, 0))
         } else {
            rslt <- format(round(x, n.digits))
            nd <- ifelse(x >= 0, nchar(rslt) - 2, nchar(rslt) - 3)
            if(nd != n.digits) {
               if(nchar(rslt) == nchar(format(x.int)))
                  rslt <- paste(rslt, ".", paste(rep("0", n.digits),
                     collapse=""), sep="")
               else
                  rslt <- paste(rslt, paste(rep("0", n.digits - nd),
                     collapse=""), sep="")
            }
         }
      }
   }

# This section handles character values
} else if(is.character(x)) {
   if(is.na(x)) {
      rslt <- miss
   } else {
      rslt <- x
   }

# Stop execution if the input value is not numeric or character
} else {
   stop("The data frame or matrix input to write.object contains elements that \nare neither numeric nor character values.\n")
}

# Return the result
rslt
}
