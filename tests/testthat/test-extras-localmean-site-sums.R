skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

# Preserve the original expressions for testing direct equality
site_sums_original_var <- function(z, w) {
  zb <- sapply(split(z[w$ij[, 2]] * w$gwt, w$ij[, 1]), sum)
  sum(w$gwt * (z[w$ij[, 2]] - zb[w$ij[, 1]])^2)
}

site_sums_original_cov <- function(zmat, w) {
  m <- ncol(zmat)
  out <- array(0, c(m, m))
  for (k in 1:m) {
    for (l in k:m) {
      z1 <- zmat[, k]
      z2 <- zmat[, l]
      zb1 <- sapply(split(z1[w$ij[, 2]] * w$gwt, w$ij[, 1]), sum)
      zb2 <- sapply(split(z2[w$ij[, 2]] * w$gwt, w$ij[, 1]), sum)
      out[k, l] <- sum(w$gwt * (z1[w$ij[, 2]] - zb1[w$ij[, 1]]) *
        (z2[w$ij[, 2]] - zb2[w$ij[, 1]]))
    }
    if (k > 1) out[k, 1:(k - 1)] <- out[1:(k - 1), k]
  }
  out
}

test_that("site sums preserve grouping, types, and fallback inputs", {
  i <- rep(c(10, 2, 1, 12, 3, 11, 4:9), each = 2)
  for (v in list(seq_along(i), as.double(seq_along(i)),
                 seq_along(i) + 1i, rep(c(0, NA_real_), length.out = length(i)))) {
    for (groups in list(i, as.integer(i), as.character(i), factor(i))) {
      expect_identical(localmean_site_sums(v, groups, 12),
        sapply(split(v, groups), sum))
    }
    keep <- i != 3
    expect_identical(localmean_site_sums(v[keep], i[keep], 12),
      sapply(split(v[keep], i[keep]), sum))
    expect_identical(localmean_site_sums(v, i, 13), sapply(split(v, i), sum))
  }
  expect_identical(localmean_site_sums(numeric(), integer(), 0),
    sapply(split(numeric(), integer()), sum))
})

test_that("large integer and double site labels match the original factor", {
  for (i in list(seq_len(100000), as.double(seq_len(100000)))) {
    v <- rep(1, length(i))
    expect_identical(localmean_site_sums(v, i, length(i)),
      sapply(split(v, i), sum))
  }
})

test_that("local variances and covariances remain identical", {
  set.seed(5)
  n <- 24
  x <- runif(n)
  y <- runif(n)
  prb <- runif(n, .1, 1)
  signed_prb <- prb
  signed_prb[1] <- -signed_prb[1]
  z <- matrix(rnorm(n * 4), n, 4)
  z[seq(1, length(z), by = 3)] <- 0
  z_na <- z
  z_na[3, 2] <- NA_real_
  inputs <- list(z, z[, 1, drop = FALSE], z_na,
    matrix(rep(-2:3, length.out = n * 2), n, 2),
    z + 1i, as.data.frame(z))

  for (prob in list(prb, signed_prb)) {
    w <- localmean_weight(x, y, prob, nbh = 6)
    expect_false(is.null(w))
    order <- sample(nrow(w$ij))
    shuffled <- list(ij = w$ij[order, ], gwt = w$gwt[order])
    keep <- w$ij[, 1] != 3
    missing <- list(ij = w$ij[keep, ], gwt = w$gwt[keep])
    for (weights in list(w, shuffled, missing)) {
      for (values in inputs) {
        expect_identical(localmean_var(values[, 1], weights),
          site_sums_original_var(values[, 1], weights))
        expect_identical(localmean_cov(values, weights),
          site_sums_original_cov(values, weights))
      }
    }
  }
})

test_that("integer weights retain integer arithmetic", {
  w <- list(ij = matrix(c(1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L),
    ncol = 2, byrow = TRUE), gwt = c(2L, -1L, -1L, 2L))
  z <- matrix(c(50000L, 0L, 0L, 50000L), ncol = 2)
  expect_identical(localmean_var(z[, 1], w), site_sums_original_var(z[, 1], w))
  expect_identical(suppressWarnings(localmean_cov(z, w)),
    suppressWarnings(site_sums_original_cov(z, w)))
})
