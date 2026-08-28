################################################################################
# Function: localmean_weight (not exported)
# Programmers: Don Stevens and Tom Kincaid
# Date: February 6, 2020
# Revised: April 28, 2021 to use the modified object returned by the
#          localmean_weight2 function and to return a NULL object when the ginv
#          function fails to return valid output
# Revised: April 29, 2021 to eliminate use of the while loop to achieve valid
#          output from the ginv function, which means that the vincr argument
#          and the localmean_weight2 function are no longer needed
#
#' Internal Function: Local Mean Variance Neighbors and Weights
#'
#' This function calculates the index values of neighboring points and
#' associated weights required by the local mean variance estimator.
#'
#' @param x Vector of x-coordinates for location of the sample points.
#'
#' @param y Vector of y-coordinates for location of the sample points.
#'
#' @param prb Vector of inclusion probabilities for the sample points.
#'
#' @param nbh Number of neighboring points to use in the calculations.
#'
#' @return If ginv fails to return valid output, a NULL object.  Otherwise, a
#'   list containing two elements: a matrix named \code{ij} composed of the
#'   index values of neighboring points and a vector named \code{gwt}
#'   composed of weights.
#'
#' @author  Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @export
################################################################################

localmean_weight <- function(x, y, prb, nbh = 4) {
  # This builds the spatial neighborhoods D(s_i) and weights w_ij of the
  # local neighborhood variance estimator (Stevens & Olsen 2003): for each site i, its neighborhood D(s_i) is
  # itself plus its nbh - 1 (default 3) nearest neighbors, expanded so that
  # membership is symmetric (j in D(i) implies i in D(j)); ij (below)
  # enumerates every such (i, j) neighbor pair. The weights gwt (the paper's
  # w_ij) start from a distance-ranked, inverse-inclusion-probability-
  # weighted taper, then are adjusted, via a Lagrange-multiplier
  # least-squares correction solved with a generalized inverse (Moore-
  # Penrose; MASS::ginv()), so that both the row sums and column sums of
  # w_ij equal 1 (i.e. each neighborhood total is a genuine weighted
  # average, and the neighborhood totals sum to the overall total). If the
  # generalized inverse fails, NULL is returned and callers fall back to a
  # simpler (e.g. SRS) variance estimator.

  # Assign tne number of points

  n <- length(x)

  # Calculate indices of nearest neighbors
  # (idx[, j] holds, for site j, the row-indices into dst of its nbh
  # closest points, i.e. its initial nearest-neighbor set before symmetrizing)

  dst <- as.matrix(dist(cbind(x, y), diag = TRUE, upper = TRUE))
  idx <- apply(dst, 2, order)[1:nbh, ]

  # Make neighbors symmetric
  # (ij lists every (i, j) pair where j is one of i's nearest neighbors OR i
  # is one of j's nearest neighbors, i.e. the symmetrized neighborhood
  # relationship required by h(s, t) = h(t, s) in the paper; sorted by
  # neighborhood i, then by increasing distance within that neighborhood)

  jdx <- rep(1:n, rep(nbh, n))
  kdx <- unique(c((jdx - 1) * n + idx, (idx - 1) * n + jdx)) - 1
  ij <- cbind((kdx) %/% n + 1, (kdx) %% n + 1)
  ij <- ij[order(ij[, 1], dst[ij]), ]

  # Apply linear taper to the  inverse probability weights
  # (within each neighborhood, weight decreases linearly with distance rank;
  # the closest point gets the largest weight and then is scaled by
  # 1 / inclusion probability, matching the paper's rank-based initial
  # weight w*_ij before normalization)

  gct <- tabulate(ij[, 1])
  gwt <- numeric(0)
  for (i in 1:n) {
    gwt <- c(gwt, 1 - (1:gct[i] - 1) / (gct[i]))
  }
  gwt <- gwt / prb[ij[, 2]]

  # Normalize to make true average
  # (rescale each neighborhood's weights to sum to 1, so a neighborhood
  # total is a weighted average rather than a weighted sum)

  smwt <- sapply(split(gwt, ij[, 1]), sum)
  gwt <- gwt / smwt[ij[, 1]]
  smwt <- sapply(split(gwt, ij[, 2]), sum)

  # Make weights doubly stochastic
  # (smwt above are the column sums after row-normalizing, which need not
  # be 1; the block below solves, via the generalized inverse, the
  # constrained least-squares adjustment described in the paper so that
  # column sums also equal 1 while disturbing the row-normalized weights as
  # little as possible; this is what makes the set of neighborhood totals
  # itself sum to the overall total)

  hij <- matrix(0, n, n)
  hij[ij] <- 0.5
  a22 <- try(ginv(diag(gct / 2) - hij %*% diag(2 / gct) %*% hij), TRUE)
  if ("try-error" %in% class(a22)) {
    return(NULL)
  }
  a21 <- -diag(2 / gct) %*% hij %*% a22
  lm <- a21 %*% (1 - smwt)
  gm <- a22 %*% (1 - smwt)
  gwt <- (lm[ij[, 1]] + gm[ij[, 2]]) / 2 + gwt

  # Return the results
  # (ij: the (i, j) neighbor-pair index matrix; gwt: the final weight w_ij
  # for each pair, in the same order as the rows of ij)

  list(ij = ij, gwt = gwt)
}
