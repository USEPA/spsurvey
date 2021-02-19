###############################################################################
# Function: adjwgt (exported)
# Programmer: Tony Olsen
# Date: July 17, 2002
# Last Revised: February 25, 2004
#
#' Adjust Survey Design Weights by Categories
#'
#' @description This function adjusts initial survey design weights when
#'   implementation results in the use of oversample sites or if it is desired
#'   to have final weights sum to the known frame size.  Adjusted weights are
#'   equal to \code{initial_weight * framesize/sum(initial_weight)}.  The adjustment
#'   is done separately for each category specified in \code{wtcat}.
#'
#' @param sites Vector indicating site use; \code{TRUE} indicates the site
#'   should be inlcuded in the weight adjustment and \code{FALSE} indicates 
#'   the site should not be included in the weight adjustment. 
#'
#' @param wgt Vector of initial weights for each site. These equal
#'   the reciprocal of the sample inclusion probabilities.
#'
#' @param wtcat Vector containing each site's weight adjustment
#'   category name.
#'
#' @param framesize Vector containing the known size of the frame
#'   for each category name in wtcat. The names attribute much match the
#'   category names in \code{wtcat}. 
#'
#' @return Vector of adjusted weights, where the adjusted weight is set
#'   to \code{0} for sites whose value in the sites argument was set to
#'   \code{FALSE}.
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
#' @export
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
