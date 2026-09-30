skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

source("tests-extras-greg-helper.R", local = TRUE)

test_that("legacy local percentile inversion retains rounded points and t limits", {
  d <- greg_data()
  d$y <- round(d$y)
  d$y[4] <- NA
  probabilities <- c(0, .25, .5, .75, 1)
  for (stratified in c(FALSE, TRUE)) for (subset in c(FALSE, TRUE)) {
    a <- list(dframe = d, vars = "y", subpops = "region", xcoord = "xcoord", ycoord = "ycoord",
      stratumID = if (stratified) "stratum" else NULL, statistics = "Pct",
      pctval = probabilities * 100, subset_local = subset)
    actual <- suppressMessages(do.call(cont_analysis, a))$Pct
    a$vartype <- "SRS"
    old <- suppressMessages(do.call(cont_analysis, a))$Pct
    expect_identical(actual$Estimate, old$Estimate)
    expect_identical(actual$nResp, old$nResp)
    for (domain in levels(d$region)) {
      z <- actual[actual$Subpopulation == domain, ]
      keep <- d$region == domain & !is.na(d$y)
      y <- d$y[keep]
      w <- d$weight[keep]
      ordered <- sort(unique(y))
      cumulative <- cumsum(as.numeric(rowsum(w, y, reorder = TRUE))) / sum(w)
      inverse <- approxfun(cumulative, ordered, yleft = min(y), yright = max(y), ties = min)
      df <- sum(keep) - if (stratified) length(unique(d$stratum[keep])) else 1
      critical <- qt(.975, df)
      for (i in seq_along(probabilities)) {
        a$dframe$below <- as.numeric(d$y <= z$Estimate[i])
        a$vars <- "below"
        a$statistics <- "Mean"
        a$vartype <- "local"
        cdf <- suppressMessages(do.call(cont_analysis, a))$Mean
        se <- cdf$StdError[cdf$Subpopulation == domain]
        bounds <- inverse(probabilities[i] + c(-1, 1) * critical * se)
        expect_equal(c(z$LCB95Pct[i], z$UCB95Pct[i]), bounds)
        expect_equal(z$StdError[i], diff(bounds) / (2 * critical))
      }
    }
  }
})

test_that("constant legacy responses retain their point and interval convention", {
  d <- greg_data()
  d$y <- 2
  a <- list(dframe = d, vars = "y", subpops = "region", statistics = "Pct", pctval = c(25, 50, 75),
    xcoord = "xcoord", ycoord = "ycoord")
  local <- suppressMessages(do.call(cont_analysis, a))$Pct
  a$vartype <- "SRS"
  old <- suppressMessages(do.call(cont_analysis, a))$Pct
  expect_equal(local, old)
})

test_that("legacy two-stage percentile intervals use the supported local CDF path", {
  d <- greg_data()
  d$cluster <- rep(seq_len(8), each = 4)
  d$weight1 <- 5
  d$weight <- 2
  d$xcoord1 <- rep(seq_len(8), each = 4)
  d$ycoord1 <- rep(c(1, 3, 2, 5, 7, 6, 4, 8), each = 4)
  a <- list(dframe = d, vars = "y", statistics = "Pct", pctval = c(25, 50, 75),
    xcoord = "xcoord", ycoord = "ycoord", clusterID = "cluster", weight1 = "weight1",
    xcoord1 = "xcoord1", ycoord1 = "ycoord1")
  local <- suppressMessages(do.call(cont_analysis, a))$Pct
  a$vartype <- "SRS"
  old <- suppressMessages(do.call(cont_analysis, a))$Pct
  expect_identical(local$Estimate, old$Estimate)
  expect_true(all(is.finite(local$StdError)))
  a$vartype <- "local"
  a$subset_local <- FALSE
  expect_error(do.call(cont_analysis, a), "two-stage")
})
