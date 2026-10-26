skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))

change_covariance_fixture <- function(clustered = FALSE, stratified = TRUE,
                                      partial = FALSE, equal = TRUE, vartype = "SRS") {
  set.seed(5)
  n <- 160
  d <- data.frame(siteID = seq_len(n), stratum = rep(c("H1", "H2"), each = n / 2),
    cluster = rep(1:16, each = 10), weight = rep(c(2, 4), each = n / 2),
    weight1 = rep(c(3, 5), each = n / 2), xc = runif(n), yc = runif(n), x = rnorm(n))
  d$x1 <- rep(runif(16), each = 10); d$y1 <- rep(runif(16), each = 10)
  d$y <- d$x + rnorm(n)
  d$category <- factor(ifelse(d$y > 0, "Good", "Poor"))
  d$domain <- factor(rep(c("A", "B"), n / 2))
  second <- transform(d, y = .6 * y + rnorm(n, sd = 1.1))
  second$category <- factor(ifelse(second$y > 0, "Good", "Poor"))
  if (partial) second$siteID[seq_len(n) %% 3 == 0] <- second$siteID[seq_len(n) %% 3 == 0] + 1000
  if (!equal) second$weight <- second$weight * (1 + second$xc / 4)
  args <- list(dframe = rbind(transform(d, year = "one"), transform(second, year = "two")),
    vars_cont = "y", vars_cat = "category", test = c("mean", "total", "median"),
    surveyID = "year", siteID = "siteID", weight = "weight",
    xcoord = "xc", ycoord = "yc", revisitwgt = equal, vartype = vartype,
    stratumID = if (stratified) "stratum" else NULL,
    clusterID = if (clustered) "cluster" else NULL,
    weight1 = if (clustered) "weight1" else NULL,
    xcoord1 = if (clustered) "x1" else NULL, ycoord1 = if (clustered) "y1" else NULL)
  list(first = d, second = second, args = args)
}

change_covariance_run <- function(args) suppressMessages(do.call(change_analysis, args))

change_covariance_components <- function(result) {
  covariance <- function(x, suffix = "") {
    se <- paste0("StdError", suffix)
    (x[[paste0(se, "_1")]]^2 + x[[paste0(se, "_2")]]^2 - x[[se]]^2) / 2
  }
  c(mean = covariance(result$contsum_mean), total = covariance(result$contsum_total),
    proportion = covariance(result$catsum, ".P"), size = covariance(result$catsum, ".U"))
}

test_that("All Sites SRS change agrees with survey on a shared single-stage design", {
  for (stratified in c(FALSE, TRUE)) {
    f <- change_covariance_fixture(stratified = stratified)
    actual <- change_covariance_run(f$args)
    d <- f$first
    d$difference <- f$second$y - d$y
    d$indicator_difference <- as.numeric(f$second$category == "Good") - (d$category == "Good")
    design <- survey::svydesign(~1, strata = if (stratified) ~stratum else NULL,
      weights = ~weight, data = d)
    expect_equal(actual$contsum_mean$StdError,
      as.numeric(survey::SE(survey::svymean(~difference, design))), tolerance = 1e-10)
    expect_equal(actual$contsum_total$StdError,
      as.numeric(survey::SE(survey::svytotal(~difference, design))), tolerance = 1e-10)
    good <- actual$catsum$Category == "Good"
    expect_equal(actual$catsum$StdError.P[good],
      100 * as.numeric(survey::SE(survey::svymean(~indicator_difference, design))), tolerance = 1e-10)
    expect_equal(actual$catsum$StdError.U[good],
      as.numeric(survey::SE(survey::svytotal(~indicator_difference, design))), tolerance = 1e-10)
  }
})

test_that("independent strata combine repeat covariances on the appropriate scale", {
  for (clustered in c(FALSE, TRUE)) for (partial in c(FALSE, TRUE))
    for (equal in c(FALSE, TRUE)) for (type in c("SRS", "Local")) {
      f <- change_covariance_fixture(clustered, TRUE, partial, equal, type)
      # Keep cross-occasion covariance modest: the retained two-stage SRS
      # repeat operator can otherwise trigger the nonpositive-variance fallback.
      f$second$y <- f$second$y - .4 * f$first$y
      f$second$category <- factor(ifelse(f$second$y > 0, "Good", "Poor"))
      f$args$dframe <- rbind(transform(f$first, year = "one"), transform(f$second, year = "two"))
      # The per-stratum means differ from the overall mean, so compare cross
      # covariances, not the marginal mean variances, under the Local operator.
      a <- f$args; a$test <- c("mean", "total")
      all <- change_covariance_run(a)
      parts <- lapply(c("H1", "H2"), function(h) {
        b <- a; b$stratumID <- NULL; b$dframe <- subset(b$dframe, stratum == h)
        change_covariance_run(b)
      })
      shares <- lapply(list(f$first, f$second), function(d) {
        w <- d$weight * if (clustered) d$weight1 else 1
        as.numeric(tapply(w, d$stratum, sum)) / sum(w)
      })
      expected <- Reduce(`+`, lapply(seq_along(parts), function(h) {
        cov <- change_covariance_components(parts[[h]])
        ratio <- grepl("^(mean|proportion)", names(cov))
        cov[ratio] <- cov[ratio] * shares[[1]][h] * shares[[2]][h]
        cov
      }))
      expect_equal(change_covariance_components(all), expected, tolerance = 1e-10,
        info = paste(clustered, partial, equal, type))
      expect_equal(all$contsum_total$StdError^2,
        sum(vapply(parts, function(p) p$contsum_total$StdError^2, numeric(1))), tolerance = 1e-10)
      expect_equal(all$catsum$StdError.U^2,
        Reduce(`+`, lapply(parts, function(p) p$catsum$StdError.U^2)), tolerance = 1e-10)
    }
})

test_that("one stratum is equivalent to an unstratified change analysis", {
  for (clustered in c(FALSE, TRUE)) for (equal in c(FALSE, TRUE)) for (type in c("SRS", "Local")) {
    a <- change_covariance_fixture(clustered, FALSE, TRUE, equal, type)$args
    expected <- change_covariance_run(a)
    a$dframe$stratum <- "H1"; a$stratumID <- "stratum"
    expect_equal(change_covariance_run(a), expected, tolerance = 1e-12)
  }
})

test_that("median change uses the same covariance as its threshold indicators", {
  for (clustered in c(FALSE, TRUE)) for (equal in c(FALSE, TRUE)) for (type in c("SRS", "Local")) {
    f <- change_covariance_fixture(clustered, TRUE, TRUE, equal, type)
    a <- f$args
    actual <- change_covariance_run(a)$contsum_median
    # Obtain the public first-occasion median, then run an explicit categorical
    # analysis of the same two bins. This also exercises the separate median loop.
    cut <- cont_analysis(f$first, vars = "y", weight = "weight",
      stratumID = "stratum", clusterID = a$clusterID, weight1 = a$weight1,
      xcoord = "xc", ycoord = "yc", xcoord1 = a$xcoord1, ycoord1 = a$ycoord1,
      pctval = 50, vartype = type)$Pct$Estimate
    a$dframe$category <- factor(ifelse(a$dframe$y <= cut, "Less_Than_Median", "Greater_Than_Median"))
    a$vars_cont <- NULL
    expected <- change_covariance_run(a)$catsum
    fields <- c("Category", "DiffEst.P", "DiffEst.U", "StdError.P", "StdError.U")
    expect_equal(actual[, fields], expected[, fields], tolerance = 1e-10)
  }
})

test_that("negative repeat covariance increases the change variance", {
  a <- change_covariance_fixture()$args
  n <- nrow(a$dframe) / 2
  a$dframe$y[n + seq_len(n)] <- -a$dframe$y[seq_len(n)]
  a$vars_cat <- NULL; a$test <- c("mean", "total")
  actual <- change_covariance_run(a)
  for (x in actual) if (is.data.frame(x) && "StdError" %in% names(x)) {
    expect_gt(x$StdError^2, x$StdError_1^2 + x$StdError_2^2)
    expect_equal(x$StdError, 2 * x$StdError_1, tolerance = 1e-10)
  }
})

test_that("stratum-indicator GREG and direct local change agree on All Sites ratios", {
  for (clustered in c(FALSE, TRUE)) for (partial in c(FALSE, TRUE)) for (equal in c(FALSE, TRUE)) {
    a <- change_covariance_fixture(clustered, TRUE, partial, equal, "Local")$args
    a$test <- "mean"; a$dframe$stratum <- factor(a$dframe$stratum)
    direct <- change_covariance_run(a)
    a$formula <- ~0 + stratum
    a$popsize <- lapply(split(a$dframe, a$dframe$year), function(d)
      colSums(model.matrix(~0 + stratum, d) * d$weight * if (clustered) d$weight1 else 1))
    greg <- change_covariance_run(a)
    expect_equal(greg$contsum_mean$StdError, direct$contsum_mean$StdError, tolerance = 1e-10)
    expect_equal(greg$catsum$StdError.P, direct$catsum$StdError.P, tolerance = 1e-10)
  }
})

test_that("stratified two-stage same-design GREG SRS change follows survey", {
  f <- change_covariance_fixture(TRUE)
  a <- f$args; a$test <- c("mean", "total")
  a$dframe$stratum <- factor(a$dframe$stratum)
  a$formula <- ~0 + stratum
  a$popsize <- lapply(split(a$dframe, a$dframe$year), function(d)
    colSums(model.matrix(~0 + stratum, d) * d$weight * d$weight1))
  actual <- change_covariance_run(a)
  d <- f$first; d$stratum <- factor(d$stratum)
  d$difference <- f$second$y - d$y
  d$indicator_difference <- as.numeric(f$second$category == "Good") - (d$category == "Good")
  design <- survey::svydesign(~cluster + siteID, strata = ~stratum,
    weights = ~weight1 + weight, data = d, nest = TRUE)
  cal <- survey::calibrate(design, ~0 + stratum, a$popsize$one)
  expect_equal(actual$contsum_mean$StdError,
    as.numeric(survey::SE(survey::svymean(~difference, cal))), tolerance = 1e-10)
  expect_equal(actual$contsum_total$StdError,
    as.numeric(survey::SE(survey::svytotal(~difference, cal))), tolerance = 1e-10)
  expect_equal(actual$catsum$StdError.P[actual$catsum$Category == "Good"],
    100 * as.numeric(survey::SE(survey::svymean(~indicator_difference, cal))), tolerance = 1e-10)
})

test_that("unusable repeat strata preserve the independence fallback", {
  for (equal in c(FALSE, TRUE)) {
    a <- change_covariance_fixture(equal = equal)$args
    # Each stratum has only one repeated site.
    n <- nrow(a$dframe) / 2
    new <- n + setdiff(seq_len(n), c(1, 81))
    a$dframe$siteID[new] <- a$dframe$siteID[new] + 1000
    actual <- change_covariance_run(a)
    for (x in actual) if (is.data.frame(x)) {
      for (suffix in c("", ".P", ".U")) {
        se <- paste0("StdError", suffix)
        if (se %in% names(x)) expect_equal(x[[se]],
          sqrt(x[[paste0(se, "_1")]]^2 + x[[paste0(se, "_2")]]^2), tolerance = 1e-10)
      }
    }
    expect_true(any(grepl("single value|less than two", warn_df$warning)))
  }
})

test_that("absent categories and missing pairs do not contaminate other strata", {
  for (type in c("SRS", "Local")) for (equal in c(FALSE, TRUE)) {
    a <- change_covariance_fixture(equal = equal, vartype = type)$args
    a$dframe$category[a$dframe$stratum == "H1"] <- "Good"
    a$dframe$y[c(3, 90, 170)] <- NA_real_
    actual <- change_covariance_run(a)
    expect_true(all(is.finite(actual$catsum$StdError.P)))
    expect_true(all(is.finite(actual$catsum$StdError.U)))
    expect_true(all(is.finite(actual$contsum_mean$StdError)))
    expect_true(all(is.finite(actual$contsum_total$StdError)))
    expect_true(all(is.finite(actual$contsum_median$StdError.P)))
  }
})

test_that("a continuous indicator total has the same change SE as its category size", {
  for (clustered in c(FALSE, TRUE)) for (stratified in c(FALSE, TRUE)) for (type in c("SRS", "Local")) {
    a <- change_covariance_fixture(clustered, stratified, TRUE, TRUE, type)$args
    a$dframe$y <- as.numeric(a$dframe$category == "Good")
    a$test <- "total"
    actual <- change_covariance_run(a)
    expect_equal(actual$contsum_total$StdError,
      actual$catsum$StdError.U[actual$catsum$Category == "Good"], tolerance = 1e-10)
  }
})

test_that("poststratified repeat subsets exclude retained zero-weight rows locally", {
  a <- change_covariance_fixture(TRUE, TRUE, TRUE, FALSE, "Local")$args
  a$dframe$all <- "All"
  a$subpops <- "all"
  a$popsize <- data.frame(all = "All", Total = 2000)
  actual <- change_covariance_run(a)
  expect_true(all(is.finite(actual$contsum_mean$StdError)))
  expect_true(all(is.finite(actual$contsum_total$StdError)))
  expect_true(all(is.finite(actual$catsum$StdError.P)))
  expect_true(all(is.finite(actual$contsum_median$StdError.P)))
})

test_that("direct repeat covariance retains small-PSU fallback warnings", {
  for (count in 1:3) {
    a <- change_covariance_fixture(TRUE, TRUE, FALSE, TRUE, "Local")$args
    second <- a$dframe$year == "two"
    within <- ave(a$dframe$siteID, interaction(a$dframe$year, a$dframe$cluster), FUN = seq_along)
    new <- second & within > count
    a$dframe$siteID[new] <- a$dframe$siteID[new] + 1000
    a$vars_cat <- NULL; a$test <- c("mean", "total")
    actual <- change_covariance_run(a)
    expect_true(all(is.finite(actual$contsum_mean$StdError)))
    expect_true(all(is.finite(actual$contsum_total$StdError)))
    expect_true(any(grepl("less than four", warn_df$warning)))
  }
})

test_that("public change warnings identify totals with either revisit weight setting", {
  for (clustered in c(FALSE, TRUE)) for (stratified in c(FALSE, TRUE))
    for (equal in c(FALSE, TRUE)) for (statistic in c("mean", "total")) {
      a <- change_covariance_fixture(clustered, stratified, FALSE, equal, "Local")$args
      # Keep three repeat sites per stratum, or three repeat PSUs per stratum,
      # to force a first-stage fallback in each of the four total call sites.
      repeat_site <- if (clustered) (a$dframe$cluster - 1) %% 8 < 3 else
        (a$dframe$siteID - 1) %% 80 < 3
      new <- a$dframe$year == "two" & !repeat_site
      a$dframe$siteID[new] <- a$dframe$siteID[new] + 1000
      # For the unstratified case retain only one group's repeat sites/PSUs.
      if (!stratified) {
        new <- a$dframe$year == "two" & a$dframe$stratum == "H2"
        a$dframe$siteID[new] <- a$dframe$siteID[new] + 1000
      }
      a$vars_cat <- NULL; a$test <- statistic
      change_covariance_run(a)
      messages <- warn_df$warning[warn_df$func == "changevar_mean"]
      label <- paste0("estimate for ", statistic, "s.")
      messages <- gsub("\\s+", " ", messages)
      expect_true(any(grepl(label, messages, fixed = TRUE)))
      other <- if (statistic == "total") "estimate for means." else "estimate for totals."
      expect_false(any(grepl(other, messages, fixed = TRUE)))
    }
})
