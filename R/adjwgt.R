################################################################################
# Function: adjwgt
# Programmer: Tony Olsen
# Date: July 17, 2002
# Last Revised: February 25, 2004
#
#' Adjust Survey Design Weights by Categories
#'
#' Purpose of this function is to adjust initial survey design weights when
#'   implementation results in use of oversample sites or when it is desired to
#'   have final weights sum to known frame size.  Adjusted weights are equal to
#'   initial weight * framesize/sum(initial weights).  The adjustment is done
#'   separately for each category specified in wtcat.
#'
#' @param sites Vector of the logical value for each site, where TRUE = include
#'   the site and FALSE = do not include the site.
#'
#' @param wgt Vector of the initial weight (inverse of the sample inclusion
#'   probability) for each site.
#'
#' @param wtcat Vector of the weight adjustment category name for each site.
#'
#' @param framesize Vector of the known size of the frame for each category
#'   name in wtcat, which must have the names attribute set to match the
#'   category names used in wtcat.
#'
#' @return Vector of adjusted weights, where the adjusted weight is set to zero
#'   for sites that have the logical value in the sites argument set to FALSE.
#'
#' @author Tony Olsen \email{olsen.tony@epa.gov}
#'
#' @keywords survey misc
#'
#' @examples
#' sites <- as.logical(rep(rep(c("TRUE","FALSE"), c(9,1)), 5))
#' wgt <- runif(50, 10, 100)
#' wtcat <- rep(c("A","B"), c(30, 20))
#' framesize <- c(15, 10)
#' names(framesize) <- c("A","B")
#' adjwgt(sites, wgt, wtcat, framesize)
#'
#' @export
################################################################################

adjwgt <- function(sites, wgt, wtcat, framesize) {

# Sum initial weights by wtcat for evaluated sites

	wgtsum <- tapply(wgt[sites], wtcat[sites], sum)

# Adjustment factor to be applied to weights for adjustment category

	adjfac <- framesize/wgtsum[match(names(framesize), names(wgtsum))]
	wtadj <- adjfac[match(wtcat, names(adjfac))]
	adjwgt <- wgt * wtadj
	adjwgt[!sites] <- 0
	as.vector(adjwgt)
}
