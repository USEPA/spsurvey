################################################################################
# Function: sorted
# Programmer: Tom Kincaid
# Date: August 13, 2001
#
#' Internal Function: Determine if a Vector is Sorted
#'
#' This function determines whether the input set of values is a nondecreasing
#' sequence.
#'
#' @param x Vector of values.
#'
#' @return Logical variable, where TRUE = sorted and FALSE = not sorted.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

sorted <- function(x) {

# Calculate additional required values

   n <- length(x)

# Determine whether the input set of values is a nondecreasing sequence

   sorted <- max(order(x) - (1:n)) == 0

# Return the result

   sorted
}
