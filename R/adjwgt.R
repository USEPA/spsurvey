###############################################################################
# Function: adjwgt (exported)
# Programmer: Tony Olsen
# Date: July 17, 2002
# Last Revised: February 25, 2004
#
#' Adjust survey design weights by categories
#'
#' @description Adjust initial survey design weights so that the
#' final weights sum to a desired frame size. Adjusted weights
#' proportionally scale the initial weights to sum to the desired frame size.
#' Separate adjustments are applied to each category specified in \code{wgtcat}.
#'
#' @param wgt Vector of initial weights for each site. These equal
#'   the reciprocal of the site's inclusion probability.
#'
#' @param wgtcat Vector containing each site's weight adjustment
#'   category name. The default is \code{NULL}, which assumes every
#'   site is in the same category.
#'
#' @param framesize Vector containing the known size of the frame
#'   for each category name in \code{wgtcat}. If \code{wgtcat} is provided,
#'   the names in \code{framesize} must match the names in \code{wgtcat}.
#'   If \code{wgtcat} is not provided, an unnamed scalar is given to
#'   \code{framesize}.
#'
#' @param sites Vector indicating site use; \code{TRUE} indicates the site
#'   should be included in the weight adjustment and \code{FALSE} indicates
#'   the site should not be included in the weight adjustment. The default is
#'   \code{NULL}, which assumes every site should be included.
#'
#' @return Vector of adjusted weights, where the adjusted weight is set
#'   to \code{0} for sites whose value in the sites argument was set to
#'   \code{FALSE}.
#'
#' @author Tony Olsen \email{olsen.tony@@epa.gov}
#'
#' @keywords survey misc
#'
#' @examples
#' wgt <- runif(50)
#' wgtcat <- rep(c("A", "B"), c(30, 20))
#' framesize <- c(A = 15, B = 10)
#' sites <- rep(rep(c(TRUE, FALSE), c(9, 1)), 5)
#' adjwgt(wgt, wgtcat, framesize, sites)
#' @export
adjwgt <- function(wgt, wgtcat = NULL, framesize, sites = NULL) {

  # set default wgtcat argument if omitted

  if (is.null(wgtcat)) {
    wgtcat <- rep("wgtgrp", length(wgt))
    names(framesize) <- "wgtgrp"
  }

  # set default sites argument if omitted

  if (is.null(sites)) {
    sites <- rep(TRUE, length(wgt))
  }

  # Sum initial weights by wgtcat for evaluated sites

  wgtsum <- tapply(wgt[sites], wgtcat[sites], sum)

  # Adjustment factor to be applied to weights for adjustment category

  adjfac <- framesize / wgtsum[match(names(framesize), names(wgtsum))]
  wtadj <- adjfac[match(wgtcat, names(adjfac))]
  adjwgt <- wgt * wtadj
  adjwgt[!sites] <- 0
  as.vector(adjwgt)
}
