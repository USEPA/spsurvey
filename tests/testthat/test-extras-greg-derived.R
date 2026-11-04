skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

test_that("GREG percentile inversion matches survey's normal Woodruff intervals", {
  d <- greg_data()
  a <- greg_args(d, "SRS")
  a$statistics <- "Pct"
  a$pctval <- c(25, 50, 75)
  a$popsize <- colSums(model.matrix(~x, d) * d$weight)
  actual <- suppressMessages(do.call(cont_analysis, a))$Pct
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  for (domain in levels(d$region)) {
    ref <- survey::svyquantile(~y, cal[d$region == domain, ], quantiles = a$pctval / 100,
      qrule = "math", interval.type = "mean", df = Inf)
    z <- actual[actual$Subpopulation == domain, ]
    expect_equal(z$Estimate, as.numeric(coef(ref)))
    expect_equal(z$StdError, as.numeric(survey::SE(ref)))
    expect_equal(as.numeric(as.matrix(z[, c("LCB95Pct", "UCB95Pct")])), as.numeric(confint(ref)))
  }
})

test_that("derived nonlocal GREG inference retains survey PPS covariance", {
  d <- greg_data()
  d$response <- factor(rep(c("Poor", "Poor", "Good", "Good"), 8))
  d$stressor <- factor(rep(c("Poor", "Good"), 16))
  totals <- colSums(model.matrix(~x, d) * d$weight)
  for (vartype in c("HT", "YG")) {
    base <- survey::svydesign(~siteID, probs = ~I(1 / weight), pps = "overton", variance = vartype, data = d)
    cal <- survey::calibrate(base, ~x, totals)
    quantile <- cont_analysis(d, vars = "y", formula = ~x, popsize = totals,
      vartype = vartype, statistics = "Pct", pctval = 50)$Pct
    ref <- survey::svyquantile(~y, cal, quantiles = .5, interval.type = "mean", qrule = "math", df = Inf)
    expect_equal(quantile$StdError, as.numeric(survey::SE(ref)))
    q <- cbind(t11 = d$response == "Poor" & d$stressor == "Poor",
      t12 = d$response == "Poor" & d$stressor == "Good",
      t21 = d$response == "Good" & d$stressor == "Poor",
      t22 = d$response == "Good" & d$stressor == "Good") * 1
    expressions <- list(relrisk = quote(log(t11 / (t11 + t21)) - log(t12 / (t12 + t22))),
      diffrisk = quote(t11 / (t11 + t21) - t12 / (t12 + t22)),
      attrisk = quote(log((t11 + t12 + t21 + t22) * t12 / ((t11 + t12) * (t12 + t22)))))
    for (kind in names(expressions)) {
      actual <- do.call(paste0(kind, "_analysis"), list(dframe = d, vars_response = "response",
        vars_stressor = "stressor", formula = ~x, popsize = totals, vartype = vartype))
      ref <- survey::svycontrast(survey::svytotal(q, cal), expressions[[kind]])
      column <- if (kind == "diffrisk") "StdError" else "StdError_log"
      expect_equal(actual[[column]], as.numeric(survey::SE(ref)))
    }
    bounds <- sort(d$y)[floor(seq(nrow(d) / 3, nrow(d), length.out = 3))]
    cal$variables$bin <- cut(d$y, c(-Inf, bounds))
    actual <- cont_cdftest(d, vars = "y", subpops = "region", formula = ~x,
      popsize = totals, vartype = vartype, testname = "Wald")
    ref <- survey::svychisq(~region + bin, cal, statistic = "Wald")
    expect_equal(actual$p_Value, as.numeric(ref$p.value))
  }
})

test_that("local percentiles invert a residual-based CDF interval", {
  d <- greg_data()
  a <- greg_args(d)
  a$subpops <- NULL
  a$statistics <- "Pct"
  a$pctval <- 50
  a$popsize <- colSums(model.matrix(~x, d) * d$weight)
  actual <- do.call(cont_analysis, a)$Pct
  q <- as.numeric(d$y <= actual$Estimate)
  mu <- weighted.mean(q, d$weight)
  e <- residuals(lm(I(q - mu) ~ x, d, weights = weight))
  nb <- localmean_weight(d$xcoord, d$ycoord, 1 / d$weight)
  se <- sqrt(localmean_var(d$weight * e / sum(d$weight), nb))
  ordered <- order(d$y)
  cumulative <- cumsum(d$weight[ordered]) / sum(d$weight)
  invert <- function(p) d$y[ordered[which(cumulative >= p)[1]]]
  bounds <- vapply(mu + c(-1, 1) * qnorm(.975) * se, invert, numeric(1))
  expect_equal(c(actual$LCB95Pct, actual$UCB95Pct), bounds)
  expect_equal(actual$StdError, diff(bounds) / (2 * qnorm(.975)))
  a$popsize <- c("(Intercept)" = 160, x = 1000)
  expect_message(invalid <- do.call(cont_analysis, a), "warning")
  expect_true(is.na(invalid$Pct$Estimate))
})

test_that("GREG risks propagate the full calibrated cell covariance through survey", {
  d <- greg_data()
  d$response <- factor(rep(c("Poor", "Poor", "Good", "Good"), 8))
  d$stressor <- factor(rep(c("Poor", "Good"), 16))
  a <- greg_args(d, "SRS")
  a$statistics <- a$vars <- NULL
  a$vars_response <- "response"
  a$vars_stressor <- "stressor"
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  for (domain in levels(d$region)) {
    keep <- d$region == domain
    q <- cbind(t11 = d$response == "Poor" & d$stressor == "Poor",
      t12 = d$response == "Poor" & d$stressor == "Good",
      t21 = d$response == "Good" & d$stressor == "Poor",
      t22 = d$response == "Good" & d$stressor == "Good") * keep
    stat <- survey::svytotal(q, cal)
    expr <- list(relrisk = quote(log(t11 / (t11 + t21)) - log(t12 / (t12 + t22))),
      diffrisk = quote(t11 / (t11 + t21) - t12 / (t12 + t22)),
      attrisk = quote(log((t11 + t12 + t21 + t22) * t12 / ((t11 + t12) * (t12 + t22)))))
    for (kind in names(expr)) {
      actual <- do.call(paste0(kind, "_analysis"), a)
      actual <- actual[actual$Subpopulation == domain, ]
      ref <- survey::svycontrast(stat, expr[[kind]])
      expected <- as.numeric(coef(ref))
      if (kind == "relrisk") expected <- exp(expected)
      if (kind == "attrisk") expected <- 1 - exp(expected)
      expect_equal(actual$Estimate, expected)
      column <- if (kind == "diffrisk") "StdError" else "StdError_log"
      expect_equal(actual[[column]], as.numeric(survey::SE(ref)))
      local_args <- a
      local_args$vartype <- "local"
      local <- do.call(paste0(kind, "_analysis"), local_args)
      residual <- lm(q ~ x, d, weights = weight)$residuals
      nb <- localmean_weight(d$xcoord, d$ycoord, 1 / d$weight)
      attr(stat, "var") <- localmean_cov(residual * as.numeric(weights(cal)), nb)
      ref_local <- survey::svycontrast(stat, expr[[kind]])
      expect_equal(local[[column]][local$Subpopulation == domain], as.numeric(survey::SE(ref_local)))
      # Restore survey covariance before checking the next nonlocal contrast.
      stat <- survey::svytotal(q, cal)
    }
  }
})

test_that("GREG CDF tests delegate survey tests with complete residual covariance", {
  d <- greg_data()
  a <- greg_args(d, "SRS")
  a$statistics <- NULL
  a$popsize <- colSums(model.matrix(~x, d) * d$weight)
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  bounds <- sort(d$y)[floor(seq(nrow(d) / 3, nrow(d), length.out = 3))]
  cal$variables$bin <- cut(d$y, c(-Inf, bounds))
  for (test in c(Wald = "Wald", adjWald = "adjWald", RaoScott_First = "Chisq", RaoScott_Second = "F")) {
    a$testname <- names(c(Wald = "Wald", adjWald = "adjWald", RaoScott_First = "Chisq", RaoScott_Second = "F"))[
      match(test, c("Wald", "adjWald", "Chisq", "F"))]
    actual <- do.call(cont_cdftest, a)
    ref <- survey::svychisq(~region + bin, cal, statistic = test)
    expect_equal(actual[[5]], as.numeric(ref$statistic))
    expect_equal(actual$p_Value, as.numeric(ref$p.value))
    a$vartype <- "local"
    local <- do.call(cont_cdftest, a)
    expect_true(is.finite(local$p_Value))
    a$vartype <- "SRS"
  }
})

test_that("joint domain calibration retains cross-domain residual covariance", {
  d <- greg_data()
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  d$All_Sites <- factor("All Sites")
  x <- model.matrix(~x, d)
  total <- colSums(x * d$weight)
  masks <- lapply(levels(d$region), function(domain) d$region == domain)
  known <- greg_calibrate(base, x * masks[[1]], colSums(x * masks[[1]] * d$weight), "known")
  unknown <- greg_calibrate(base, x, total, "population")
  q1 <- cbind(first = d$y * masks[[1]], second = 0)
  q2 <- cbind(first = 0, second = d$y * masks[[2]])
  ans <- greg_joint_total(list(known, unknown), list(q1, q2), "Local",
    list(xcoord = "xcoord", ycoord = "ycoord"))
  e1 <- lm.wfit(x * masks[[1]], q1, d$weight)$residuals
  e2 <- lm.wfit(x, q2, d$weight)$residuals
  z <- e1 * as.numeric(weights(known$design)) + e2 * as.numeric(weights(unknown$design))
  nb <- localmean_weight(d$xcoord, d$ycoord, 1 / d$weight)
  expect_equal(vcov(ans$stat), localmean_cov(z, nb), ignore_attr = TRUE)
  expect_gt(abs(vcov(ans$stat)[1, 2]), 1e-6)
})

test_that("GREG change retains shared-panel covariance and occasion totals", {
  d <- greg_data()
  panel <- rbind(transform(d, occasion = "one"), transform(d, occasion = "two", y = y + .5))
  totals <- colSums(model.matrix(~x, d) * d$weight)
  for (type in c("local", "SRS")) {
    a <- list(dframe = panel, vars_cont = "y", vars_cat = "category", test = c("mean", "total", "median"),
      surveyID = "occasion", formula = ~x, popsize = list(one = totals, two = totals),
      xcoord = "xcoord", ycoord = "ycoord", vartype = type)
    actual <- suppressWarnings(change <- do.call(change_analysis, a))
    expect_equal(actual$contsum_mean$DiffEst, .5)
    expect_lt(actual$contsum_mean$StdError, 1e-6)
    expect_equal(actual$contsum_total$DiffEst, unname(.5 * totals[1]))
    expect_true(all(abs(actual$catsum$DiffEst.P) < 1e-8))
    expect_true(all(actual$catsum$StdError.P < 1e-6))
    expect_true(all(is.finite(actual$contsum_median$DiffEst.P)))
    a$dframe$siteID[a$dframe$occasion == "two"] <- paste0("independent-", d$siteID)
    independent <- do.call(change_analysis, a)
    row <- independent$contsum_mean
    expect_equal(row$StdError^2, row$StdError_1^2 + row$StdError_2^2)
    a$dframe$siteID[a$dframe$occasion == "two"][1] <- d$siteID[1]
    sparse <- suppressMessages(do.call(change_analysis, a))$contsum_mean
    expect_equal(sparse$StdError^2, sparse$StdError_1^2 + sparse$StdError_2^2)
  }
})

test_that("partial-overlap GREG covariance follows repeat-site residual scaling", {
  d <- greg_data()
  second <- transform(d, y = .7 * y + .5 + cos(xcoord))
  second$siteID[21:32] <- paste0("new-", second$siteID[21:32])
  for (vartype in c("local", "SRS")) for (same_weights in c(TRUE, FALSE)) {
    two <- second
    if (!same_weights) two$weight <- two$weight * 1.15 * (1 + .1 * two$xcoord)
    panel <- rbind(transform(d, occasion = "one"), transform(two, occasion = "two"))
    x <- model.matrix(~x, d)
    t1 <- colSums(x * d$weight) + c(3, 2)
    t2 <- colSums(x * two$weight) + c(4, -2)
    args <- list(dframe = panel, vars_cont = "y", test = c("mean", "total"),
      surveyID = "occasion", formula = ~x, popsize = list(one = t1, two = t2),
      subpops = "region", xcoord = "xcoord", ycoord = "ycoord", vartype = vartype,
      revisitwgt = same_weights, stratumID = "stratum")
    actual <- do.call(change_analysis, args)
    base1 <- survey::svydesign(~siteID, strata = ~stratum, weights = ~weight, data = d)
    base2 <- survey::svydesign(~siteID, strata = ~stratum, weights = ~weight, data = two)
    w1 <- as.numeric(weights(survey::calibrate(base1, ~x, t1)))
    w2 <- as.numeric(weights(survey::calibrate(base2, ~x, t2)))
    ids <- intersect(d$siteID, two$siteID)
    i <- match(ids, d$siteID)
    j <- match(ids, two$siteID)
    for (domain in levels(d$region)) for (statistic in c("mean", "total")) {
      keep <- d$region == domain
      linear <- function(data, a) {
        q <- data$y * keep
        denominator <- 1
        if (statistic == "mean") {
          denominator <- sum(a * keep)
          q <- q - keep * sum(a * q) / denominator
        }
        lm.wfit(x, q, data$weight)$residuals * a / data$weight / denominator
      }
      v1 <- linear(d, w1)
      v2 <- linear(two, w2)
      # Unknown domain totals retain nonzero residuals outside the domain.
      expect_gt(sum(abs(v1[!keep])), 0)
      cross <- 0
      for (h in levels(d$stratum)) {
        ii <- i[d$stratum[i] == h]
        jj <- j[d$stratum[i] == h]
        v <- cbind(v1[ii], v2[jj])
        z <- cbind((v[, 1] - weighted.mean(v[, 1], d$weight[ii])) * d$weight[ii],
          (v[, 2] - weighted.mean(v[, 2], two$weight[jj])) * two$weight[jj])
        B <- function(q, w) {
          if (vartype == "SRS") return(length(ii) * cov(as.matrix(q)))
          localmean_cov(as.matrix(q), localmean_weight(d$xcoord[ii], d$ycoord[ii], 1 / w))
        }
        if (same_weights) cross <- cross + B(z, d$weight[ii])[1, 2] else {
          eq <- B(v, rep(1, length(ii)))
          cross <- cross + eq[1, 2] / sqrt(prod(diag(eq))) *
            sqrt(B(z[, 1], d$weight[ii])[1, 1] * B(z[, 2], two$weight[jj])[1, 1])
        }
      }
      row <- actual[[paste0("contsum_", statistic)]]
      row <- row[row$Subpopulation == domain, ]
      expected <- row$StdError_1^2 + row$StdError_2^2 - 2 * cross
      expect_gt(expected, 0)
      expect_equal(row$StdError^2, expected, tolerance = 1e-8)
      # Matching uses IDs, never the incoming row order.
      reordered <- args
      reordered$dframe <- panel[rev(seq_len(nrow(panel))), ]
      reordered$survey_names <- c("one", "two")
      expect_equal(do.call(change_analysis, reordered), actual)
    }
  }
})

test_that("GREG change validates revisit weights and trends ignore overlap", {
  d <- greg_data()
  totals <- colSums(model.matrix(~x, d) * d$weight)
  panel <- do.call(rbind, lapply(1:3, function(year) {
    one <- transform(d, year = year, y = y + .5 * year + .1 * year^2 * x^2)
    one$siteID[(8 * year - 7):(8 * year)] <- paste0(year, "-", one$siteID[(8 * year - 7):(8 * year)])
    one
  }))
  a <- list(dframe = panel, vars_cont = "y", yearID = "year", model_cont = "SLR",
    formula = ~x, popsize = list("1" = totals, "2" = totals, "3" = totals), vartype = "SRS")
  partial <- do.call(trend_analysis, a)$contsum
  expect_true(is.finite(partial$Trend_Estimate))
  expect_gt(partial$Trend_Std_Error, 0)
  a$dframe$siteID <- paste(a$dframe$year, a$dframe$siteID)
  independent <- do.call(trend_analysis, a)$contsum
  expect_equal(independent, partial)
  args <- list(dframe = panel[panel$year < 3, ], vars_cont = "y", surveyID = "year",
    formula = ~x, popsize = list("1" = totals, "2" = totals), vartype = "SRS", revisitwgt = TRUE)
  args$dframe$weight[args$dframe$year == 2] <- 2 * args$dframe$weight[args$dframe$year == 2]
  expect_error(do.call(change_analysis, args), "equal original weights")
})

test_that("GREG occasions retain supplied finite population corrections", {
  d <- greg_data()
  totals <- colSums(model.matrix(~x, d) * d$weight)
  panel <- rbind(transform(d, year = 1), transform(d, year = 2, y = .7 * y + .5))
  actual <- change_analysis(panel, vars_cont = "y", surveyID = "year", vartype = "SRS",
    formula = ~x, popsize = list("1" = totals, "2" = totals), fpc = 200)$contsum_mean
  base <- survey::svydesign(~siteID, weights = ~weight, fpc = rep(200, nrow(d)), data = d)
  cal <- survey::calibrate(base, ~x, totals)
  reference <- survey::svymean(~I(-.3 * y + .5), cal)
  expect_equal(actual$DiffEst, as.numeric(coef(reference)))
  expect_equal(actual$StdError, as.numeric(survey::SE(reference)))
  panel <- do.call(rbind, lapply(1:4, function(year) transform(d, year = year,
    y = y + .4 * year + .03 * year^2 * x^2)))
  actual <- trend_analysis(panel, vars_cont = "y", yearID = "year", model_cont = "WLR",
    formula = ~x, popsize = setNames(rep(list(totals), 4), 1:4), fpc = 200, vartype = "SRS")$contsum
  annual <- lapply(1:4, function(year) cont_analysis(panel[panel$year == year, ], vars = "y",
    formula = ~x, popsize = totals, fpc = 200, vartype = "SRS", statistics = "Mean")$Mean)
  value <- vapply(annual, function(z) z$Estimate, numeric(1))
  variance <- vapply(annual, function(z) z$StdError^2, numeric(1))
  year <- 0:3
  fit <- lm(value ~ year, weights = 1 / variance)
  expect_equal(actual$Trend_Std_Error, unname(summary(fit)$coefficients[2, 2]))
})

test_that("GREG SLR/WLR trends preserve lm inference on annual estimates", {
  d <- greg_data()
  panel <- do.call(rbind, lapply(1:4, function(year) transform(d, year = 2000 + year,
    y = y + .5 * year + c(.1, -.2, .3, -.1)[year] * x^2)))
  totals <- colSums(model.matrix(~x, d) * d$weight)
  for (model in c("SLR", "WLR")) {
    a <- list(dframe = panel, vars_cont = "y", yearID = "year", model_cont = model,
      formula = ~x, popsize = setNames(rep(list(totals), 4), 2001:2004),
      xcoord = "xcoord", ycoord = "ycoord", vartype = "SRS")
    actual <- suppressWarnings(do.call(trend_analysis, a))$contsum
    annual <- lapply(2001:2004, function(year) cont_analysis(panel[panel$year == year, ],
      vars = "y", statistics = "Mean", formula = ~x, popsize = totals, vartype = "SRS")$Mean)
    estimates <- vapply(annual, function(x) x$Estimate, numeric(1))
    variance <- vapply(annual, function(x) x$StdError^2, numeric(1))
    year <- 0:3
    w <- if (model == "WLR") 1 / variance else rep(1, 4)
    ref <- lm(estimates ~ year, weights = w)
    expect_equal(actual$Trend_Estimate, unname(coef(ref)[2]))
    expect_equal(actual$Trend_Std_Error, unname(summary(ref)$coefficients[2, 2]))
    expect_equal(actual$Trend_LCB95Pct, unname(confint(ref)[2, 1]))
    expect_equal(actual$Intercept_Estimate, unname(coef(ref)[1]))
    a$dframe$siteID <- paste(a$dframe$year, a$dframe$siteID)
    independent <- do.call(trend_analysis, a)$contsum
    expect_equal(independent, actual)
    a$model_cont <- "LMM"
    expect_error(do.call(trend_analysis, a), "SLR/WLR")
  }
})
