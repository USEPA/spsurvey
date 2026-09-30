skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

source("tests-extras-greg-helper.R", local = TRUE)

test_that("ordinary risk difference uses common local ratio contributions", {
  d <- greg_data()
  d$response <- factor(ifelse(seq_len(nrow(d)) %% 3 == 0, "Good", "Poor"))
  d$stressor <- d$category
  for (restricted in c(TRUE, FALSE)) for (stratified in c(TRUE, FALSE)) {
    args <- list(dframe = d, vars_response = "response", vars_stressor = "stressor",
      subpops = "region", xcoord = "xcoord", ycoord = "ycoord", subset_local = restricted)
    # Both groups occur in each stratum.
    d$stratum <- factor(rep(c("A", "B"), each = 8, length.out = nrow(d)))
    args$dframe <- d
    if (stratified) args$stratumID <- "stratum"
    actual <- do.call(diffrisk_analysis, args)
    for (domain in levels(d$region)) {
      member <- d$region == domain
      a <- member & d$stressor == "Poor"
      b <- member & d$stressor == "Good"
      y <- d$response == "Poor"
      pa <- weighted.mean(y[a], d$weight[a])
      pb <- weighted.mean(y[b], d$weight[b])
      z <- cbind(d$weight * a * (y - pa) / sum(d$weight[a]),
        d$weight * b * (y - pb) / sum(d$weight[b]))
      strata <- if (stratified) d$stratum else rep("All", nrow(d))
      covariance <- matrix(0, 2, 2)
      for (h in unique(strata)) {
        idx <- strata == h & if (restricted) member else TRUE
        if (!any(idx & member)) next
        nb <- localmean_weight(d$xcoord[idx], d$ycoord[idx], 1 / d$weight[idx])
        covariance <- covariance + localmean_cov(z[idx, ], nb)
      }
      row <- actual[actual$Subpopulation == domain, ]
      expect_equal(row$Estimate, pa - pb)
      expect_equal(row$StdError^2, sum(diag(covariance)) - 2 * covariance[1, 2])
    }
  }
})

test_that("ordinary nonlocal risk contrasts retain full-design covariance", {
  d <- greg_data()
  d$response <- factor(ifelse(seq_len(nrow(d)) %% 3 == 0, "Good", "Poor"))
  d$stressor <- d$category
  d$stratum <- factor(rep(c("A", "B"), each = 8, length.out = nrow(d)))
  d$fpc <- 200
  for (type in c("SRS", "HT", "YG")) {
    # survey's Overton constructor in this version requires numeric strata;
    # factors fail before any estimator is called.
    if (type != "SRS") d$stratum <- as.numeric(factor(d$stratum))
    args <- list(dframe = d, vars_response = "response", vars_stressor = "stressor",
      subpops = "region", vartype = type, stratumID = "stratum")
    base <- if (type == "SRS") {
      args$fpc <- list(A = 200, B = 200)
      survey::svydesign(~siteID, strata = ~stratum, weights = ~weight, fpc = ~fpc, data = d)
    } else survey::svydesign(~siteID, strata = ~stratum, probs = ~I(1 / weight),
      pps = "overton", variance = type, data = d)
    actual <- do.call(diffrisk_analysis, args)
    for (domain in levels(d$region)) {
      a <- d$region == domain & d$stressor == "Poor"
      b <- d$region == domain & d$stressor == "Good"
      y <- d$response == "Poor"
      z <- cbind(ay = a * y, a = a, by = b * y, b = b) * 1
      ref <- survey::svycontrast(survey::svytotal(z, base), quote(ay / a - by / b))
      row <- actual[actual$Subpopulation == domain, ]
      expect_equal(row$Estimate, as.numeric(coef(ref)))
      expect_equal(row$StdError, as.numeric(survey::SE(ref)))
    }
  }
})

test_that("two-stage local risk difference propagates joint contributions", {
  d <- greg_data()
  d$cluster <- rep(1:4, each = 8)
  d$weight1 <- rep(c(2, 3, 2, 3), each = 8)
  d$x1 <- rep(c(0, 1, 0, 1), each = 8)
  d$y1 <- rep(c(0, 0, 1, 1), each = 8)
  d$response <- factor(ifelse(seq_len(nrow(d)) %% 3 == 0, "Good", "Poor"))
  d$stressor <- d$category
  actual <- diffrisk_analysis(d, "response", "stressor", xcoord = "xcoord", ycoord = "ycoord",
    clusterID = "cluster", weight1 = "weight1", xcoord1 = "x1", ycoord1 = "y1")
  a <- d$stressor == "Poor"
  b <- !a
  y <- d$response == "Poor"
  w <- d$weight * d$weight1
  pa <- weighted.mean(y[a], w[a]); pb <- weighted.mean(y[b], w[b])
  z <- d$weight * (a * (y - pa) / sum(w[a]) - b * (y - pb) / sum(w[b]))
  within <- between <- numeric(4)
  for (h in 1:4) {
    i <- d$cluster == h
    within[h] <- unique(d$weight1[i]) * localmean_var(z[i],
      localmean_weight(d$xcoord[i], d$ycoord[i], 1 / d$weight[i]))
    between[h] <- sum(z[i]) * unique(d$weight1[i])
  }
  i <- !duplicated(d$cluster)
  variance <- sum(within) + localmean_var(between,
    localmean_weight(d$x1[i], d$y1[i], 1 / d$weight1[i]))
  expect_equal(actual$Estimate, pa - pb)
  expect_equal(actual$StdError^2, variance)
})
