skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))

ratio_fixture <- function() {
  set.seed(5)
  d <- data.frame(id = 1:120, w = runif(120, 2, 5), xc = runif(120),
    yc = runif(120), h = rep(c("H1", "H2"), each = 60),
    domain = factor(rep(c("A", "B"), 60)))
  d$y <- ifelse(d$h == "H1", 1, 10)
  d$cat <- factor(ifelse(d$h == "H1", "Low", "High"))
  d$wgt <- d$w
  d
}

# Independent linearization and exported local operator, including rows whose
# contribution is zero. No internal ratio or contribution helper is used.
ratio_reference <- function(d, y, member, subset_local = FALSE) {
  y <- as.matrix(y)
  member <- member & complete.cases(y)
  observed <- complete.cases(y)
  y[!member, ] <- 0
  denominator <- sum(d$w * member)
  estimate <- colSums(d$w * y) / denominator
  z <- d$w * (y - outer(member, estimate)) / denominator
  rows <- which(observed & (!subset_local | member))
  covariance <- matrix(0, ncol(z), ncol(z))
  for (i in split(rows, d$h[rows])) {
    nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
    covariance <- covariance + localmean_cov(z[i, , drop = FALSE], nb)
  }
  list(estimate = estimate, covariance = covariance, z = z)
}

test_that("stratified direct means, categories and CDFs use overall ratios", {
  d <- ratio_fixture()
  for (subset_local in c(FALSE, TRUE)) {
    a <- list(dframe = d, vars = "y", subpops = "domain", All_Sites = TRUE,
      siteID = "id", weight = "w", xcoord = "xc", ycoord = "yc",
      stratumID = "h", subset_local = subset_local)
    actual <- do.call(cont_analysis, c(a, list(statistics = c("Mean", "CDF"))))
    ca <- a
    ca$vars <- "cat"
    cats <- do.call(cat_analysis, ca)
    for (domain in c("A", "B", "All Sites")) {
      member <- if (domain == "All Sites") rep(TRUE, nrow(d)) else d$domain == domain
      ref <- ratio_reference(d, d$y, member, subset_local)
      row <- actual$Mean[actual$Mean$Subpopulation == domain, ]
      expect_equal(row$Estimate, as.numeric(ref$estimate))
      expect_equal(row$StdError^2, as.numeric(ref$covariance), tolerance = 1e-10)
      expect_gt(row$StdError, 0)
      ref <- ratio_reference(d, d$y <= 1, member, subset_local)
      row <- cats[cats$Subpopulation == domain & cats$Category == "Low", ]
      expect_equal(row$StdError.P^2 / 10000, as.numeric(ref$covariance), tolerance = 1e-10)
      row <- actual$CDF[actual$CDF$Subpopulation == domain & abs(actual$CDF$Value - 1) < 1e-8, ]
      expect_true(nrow(row) > 0)
      expect_equal(row$StdError.P^2 / 10000, rep(as.numeric(ref$covariance), nrow(row)), tolerance = 1e-10)
    }
  }
})

test_that("global ratio contributions reproduce survey's stratified SRS variance", {
  d <- ratio_fixture()
  base <- survey::svydesign(~id, strata = ~h, weights = ~w, data = d)
  for (member in list(rep(TRUE, nrow(d)), d$domain == "A")) {
    ref <- ratio_reference(d, cbind(d$y, d$y <= 1), member)
    q <- cbind(d$y, d$y <= 1) * member
    expected <- survey::svyratio(q, matrix(as.numeric(member)), base, covmat = TRUE)
    expect_equal(unname(coef(expected)), as.numeric(ref$estimate))
    actual <- vcov(survey::svytotal(ref$z / d$w, base))
    expect_equal(unname(actual), unname(vcov(expected)), tolerance = 1e-12)
  }
})

test_that("stratified CDF comparison uses overall cell proportions", {
  d <- ratio_fixture()
  d$rowvar <- factor(paste("Subpopulation", rep(1:2, 60)))
  d$colvar <- d$cat
  d$rowvar[c(2, 15, 80)] <- NA
  d$colvar[c(4, 85)] <- NA
  base <- survey::svydesign(~id, strata = ~h, weights = ~w, data = d)
  dn <- list(siteID = "id", weight = "w", xcoord = "xc", ycoord = "yc",
    stratumID = "h", clusterID = NULL)
  f <- ~ interaction(factor(rowvar), factor(colvar)) - 1
  q <- model.matrix(f, model.frame(f, d, na.action = na.pass))
  q[is.na(d$rowvar), ] <- 0
  q[is.na(d$colvar), ] <- NA
  for (subset_local in c(FALSE, TRUE)) {
    ref <- ratio_reference(d, q, !is.na(d$rowvar), subset_local)
    actual <- cdftest_localmean_prop(base, dn, FALSE, NULL,
      c("domain", "A", "B", "y"), subset_local)
    expect_equal(unname(actual$varest), unname(ref$covariance), tolerance = 1e-10)
  }
})

test_that("stratified ratio fallback retains overall centering and denominator", {
  d <- ratio_fixture()[c(1:3, 61:80), ]
  base <- survey::svydesign(~id, strata = ~h, weights = ~w, data = d)
  ref <- survey::svymean(~y, base)
  actual <- cont_analysis(d, "y", statistics = "Mean", weight = "w",
    xcoord = "xc", ycoord = "yc", stratumID = "h")$Mean
  expect_equal(actual$StdError, as.numeric(survey::SE(ref)))
  expect_gt(actual$StdError, 0)
})

test_that("two-stage All Sites ratios use overall centering within each stage", {
  d <- ratio_fixture()
  d$psu <- rep(1:12, each = 10)
  d$w1 <- rep(seq(2, 4, length.out = 12), each = 10)
  d$x1 <- rep(seq_len(12), each = 10)
  d$y1 <- rep(rep(c(0, 1), 6), each = 10)
  weight <- d$w * d$w1
  estimate <- weighted.mean(d$y, weight)
  z <- weight * (d$y - estimate) / sum(weight)
  expected <- 0
  for (h in unique(d$h)) {
    groups <- split(which(d$h == h), d$psu[d$h == h])
    first <- vapply(groups, function(i) i[1], integer(1))
    nb <- localmean_weight(d$x1[first], d$y1[first], 1 / d$w1[first])
    totals <- vapply(groups, function(i) sum(z[i]), numeric(1))
    expected <- expected + localmean_var(totals, nb)
    for (i in groups) {
      nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
      expected <- expected + d$w1[i[1]] * localmean_var(z[i] / d$w1[i], nb)
    }
  }
  actual <- cont_analysis(d, "y", statistics = "Mean", weight = "w",
    weight1 = "w1", clusterID = "psu", xcoord1 = "x1", ycoord1 = "y1",
    xcoord = "xc", ycoord = "yc", stratumID = "h")$Mean
  expect_equal(actual$StdError^2, expected, tolerance = 1e-10)
  expect_gt(actual$StdError, 0)
})
