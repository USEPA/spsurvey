###############################################################################
# Function: ash1_wgt.R (exported)
# Programmer: Tony Olsen
# 	Based on original script by Susan Hornsby
# Date: February 1, 2005
# Last Revised: January 22, 2021
#
#' Compute the average shifted histogram (ASH) for one-dimensional weighted data
#'
#' Calculate the average shifted histogram estimate of a density based on one-dimensional data
#'   from a survey design with weights.
#'
#' @param x Vector used to estimate the density. \code{NA} values are allowed.
#'
#' @param wgt Vector of weights for each observation from a
#'   probability sample. The default assigns equal weights (equal probability).
#'
#' @param m Number of empty bins to add to the ends when the range is not
#'   completely specified.  The default is \code{5}.
#'
#' @param nbin Number of bins for density estimation.  The default is \code{50}.
#'
#' @param ab Optional range for support associated with the density. Both
#'   values may be equal to \code{NA}.  If equal to \code{NA}, then corresponding limit will
#'   be based on \code{nicerange()}. The default is \code{NULL}.
#'
#' @param support Type of support.  If equal to \code{"Continuous"}, then data are
#'   from a continuous distribution.  If equal to \code{"Ordinal"}, then data are from
#'   a discrete distribution defined for integers only.  The default is
#'   \code{"Continuous"}.
#'
#' @return List containing the ASH density estimate.  List consists of
#'   \describe{
#'     \item{\code{tcen}}{ x-coordinate for center of bin}
#'     \item{\code{f}}{ y-coordinate for density estimate height}
#'   }
#'
#'
#' @author Tony Olsen \email{Olsen.tony@@epa.gov}
#'
#' @references
#'   Scott, D. W. (1985). "Averaged shifted histograms: effective nonparametric
#'   density estimators in several dimensions." \emph{The Annals of Statistics} 13(3):
#'   1024-1040.
#'
#'
#' @examples
#' x <- rnorm(100, 10, sqrt(10))
#' wgt <- runif(100, 10, 100)
#' rslt <- ash1_wgt(x, wgt)
#' plot(rslt)
#' @export
ash1_wgt <- function(x, wgt = rep(1, length(x)), m = 5, nbin = 50, ab = NULL,
                     support = "Continuous") {
  # The averaged shifted histogram approximates the average of
  # many histograms of the same bin width but different origins, smoothing
  # out the arbitrary-origin behavior of a single histogram
  # without actually building multiple histograms. Instead, data are binned
  # once finely (bin1_wgt(), bin width delta), and each fine bin's density
  # height is a weighted average of the counts in the m bins to either side
  # (wgt_lim()), which is mathematically
  # equivalent to averaging m shifted coarser histograms.

  # Bin the possibly weighted data
  v <- bin1_wgt(x, wgt, nbin, ab, support = support)

  # Set delta based on range and number of bins
  delta <- attr(v, "delta")
  h <- m * delta

  # Set up vectors for estimation
  nbin <- attr(v, "nbin")
  a <- attr(v, "ab")[1]
  b <- attr(v, "ab")[2]

  # Add m-1 empty bins on ends when no ab boundary specified
  adj <- 0
  if (is.null(ab)) {
    v <- c(rep(0, m - 1), v, rep(0, m - 1))
    nbin <- nbin + 2 * (m - 1)
    adj <- m - 1
  } else if (is.na(ab[1])) {
    v <- c(rep(0, m - 1), v)
    nbin <- nbin + (m - 1)
    adj <- m - 1
  } else if (is.na(ab[2])) {
    v <- c(v, rep(0, m - 1))
    nbin <- nbin
  }

  # Compute lower limit, center, and upper limit of bins
  tlow <- a - adj * delta + ((1:nbin) - 1.0) * delta
  tcen <- a - adj * delta + ((1:nbin) - 0.5) * delta
  tup <- a - adj * delta + (1:nbin) * delta

  # Compute density height
  f <- rep(0, nbin)
  for (i in 1:nbin) {
    mlow <- max(1, i - m + 1)
    mhi <- min(nbin, i + m - 1)
    for (k in mlow:mhi) {
      if (tup[k] >= a & tlow[k] <= b) {
        f[i] <- f[i] + v[k] * wgt_lim(k - i, m, mlow = mlow - i, mhi = mhi - i)
      }
    }
  }

  # Adjust height so density area equals 1
  f <- f / (delta * sum(f))

  # Construct output
  ash <- list(x = tcen, y = f)
  attr(ash, "delta") <- delta
  attr(ash, "m") <- m
  attr(ash, "h") <- h
  attr(ash, "support") <- support

  # Return the result
  return(ash)
}


#
# BIN algorithm for unequal-probability sample,
#

#' Bin (possibly weighted) data for the averaged shifted histogram
#'
#' @param x Vector used to estimate the density. \code{NA} values are
#'   removed.
#'
#' @param wgt Vector of weights for each observation.
#'
#' @param nbin Number of bins.
#'
#' @param ab Range for support; \code{NA} elements are filled from
#'   \code{nicerange(x)}.
#'
#' @param support \code{"Continuous"} or \code{"Ordinal"} (see
#'   \code{ash1_wgt}).
#'
#' @return A numeric vector of per-bin weighted counts, with attributes
#'   \code{nbin}, \code{ab}, \code{delta} (bin width), and \code{support}.
#'
#' @noRd
bin1_wgt <- function(x, wgt = rep(1, length(x)), nbin = 50,
                     ab = nicerange(x), support = "Continuous") {
  # Remove any missing data
  x <- x[!is.na(x)]
  wgt <- wgt[!is.na(x)]
  n <- length(x)

  # Check that nbin is positive
  if (nbin <= 0) {
    stop("\nNumber of bin intervals nonpositive")
  }

  # Check for ab range
  tmp <- nicerange(x)
  if (is.null(ab)) {
    ab <- tmp
  } else {
    if (is.na(ab[1])) ab[1] <- tmp[1]
    if (is.na(ab[2])) ab[2] <- tmp[2]
    if (ab[1] >= ab[2]) {
      stop("\nInterval vector has negative orientation")
    }
  }

  # Determine delta
  # Continuous data case
  if (support == "Continuous") {
    delta <- (ab[2] - ab[1]) / nbin
  }
  if (support == "Ordinal") {
    delta <- 1
    ab[1] <- floor(ab[1]) - 0.5
    ab[2] <- ceiling(ab[2]) + 0.5
    nbin <- ab[2] - ab[1] + 1
  }

  # Sum weighted data in bins
  v <- rep(0, nbin)
  for (k in 1:nbin) {
    for (i in 1:n) {
      v[k] <- ifelse(((ab[1] + (k - 1) * delta) <= x[i]) & (x[i] < (ab[1] + k * delta)),
        v[k] + wgt[i], v[k]
      )
    }
  }
  attr(v, "nbin") <- nbin
  attr(v, "ab") <- ab
  attr(v, "delta") <- delta
  attr(v, "support") <- support

  # Return the result
  return(v)
}


#
# Define weight function
#
#' Biweight (quartic) kernel weight for the averaged shifted histogram
#'
#' @param i Signed bin offset (distance in bins from the bin being
#'   estimated) to compute the weight for.
#'
#' @param m Number of bins to either side used in smoothing (see
#'   \code{ash1_wgt}).
#'
#' @param mlow,mhi Lowest/highest offset actually available (differs from
#'   \code{-(m - 1)}/\code{m - 1} near the edges of the binned range), used
#'   to renormalize the kernel so weights near an edge still sum
#'   appropriately.
#'
#' @return A single numeric weight.
#'
#' @noRd
wgt_lim <- function(i, m, mlow = (1 - m), mhi = (m - 1)) {
  K <- function(t) {
    (15 / 16) * (1 - t^2)^2
  }
  I <- mlow:mhi
  w <- m * K(i / m) / sum(K(I / m))
  return(w)
}


#
# Find nice range for binning
#

#' Pad a data range for use as default histogram/density support
#'
#' Expands \code{range(x)} outward by \code{beta} (as a fraction of the
#' range) on each side, so density estimates near the extremes of the data
#' are not artificially truncated at the sample min/max.
#'
#' @param x Numeric vector.
#'
#' @param beta Fraction of the range to pad on each side. Default \code{0.1}.
#'
#' @return A length-2 numeric vector, the padded range.
#'
#' @noRd
nicerange <- function(x, beta = 0.1) {
  ab <- range(x)
  del <- ((ab[2] - ab[1]) * beta) / 2
  return(c(ab + c(-del, del)))
}
