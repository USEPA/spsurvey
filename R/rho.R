###############################################################################
# Function: rho (exported)
# Programmers: Tony Olsen
# Date: September 9, 2020
#'
#' Place the sites selected by the UPpivotal method in reverse hierarchical order
#'
#' @param sites Sample object with sites selected by GRTS algorithm
#'
#' @return sites Sample object returned in reverse hierarchical order
#'
#' @author Tony Olsen \email{olsen.tony@@epa.gov}
#'
#' @keywords survey
#'
#'
#' @export
###############################################################################

rho <- function(sites) {

  # Construct sample hierarchical address
  np <- nrow(sites)
  nlev <- max(1, trunc(logb(np, 4)))
  ifelse(np == 4^nlev, nlev, nlev <- nlev + 1)
  ad <- matrix(0, 4^nlev, nlev)
  rv4 <- 0:3
  pwr4 <- 4.^(0.:(nlev - 1.))
  for (i in 1:nlev) {
    ad[, i] <- rep(rep(rv4, rep(pwr4[i], 4.)), pwr4[nlev] / pwr4[i])
  }
  rho4 <- as.vector(ad %*% matrix(rev(pwr4), nlev, 1))

  # Place sample in reverse hierarchical order
  sites <- sites[unique(floor(rho4 * np / 4^nlev)) + 1., ]
}
