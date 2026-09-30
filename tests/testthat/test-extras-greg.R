skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

test_that("GREG totals, ratios and nonlocal uncertainty agree with survey", {
  d <- greg_data()
  for (type in c("SRS", "HT", "YG")) {
    a <- greg_args(d, type)
    actual <- do.call(cont_analysis, a)
    base <- if (type == "SRS") {
      survey::svydesign(~siteID, weights = ~weight, data = d)
    } else {
      survey::svydesign(~siteID, probs = ~I(1 / weight), pps = "overton",
        variance = type, data = d)
    }
    calibrated <- survey::calibrate(base, ~x, a$popsize)
    for (i in seq_along(levels(d$region))) {
      delta <- as.numeric(d$region == levels(d$region)[i])
      numerator <- matrix(delta * d$y, ncol = 1, dimnames = list(NULL, "y"))
      denominator <- matrix(delta, ncol = 1, dimnames = list(NULL, "size"))
      for (stat in c("Mean", "Total")) {
        ref <- if (stat == "Mean") survey::svyratio(numerator, denominator, calibrated) else survey::svytotal(numerator, calibrated)
        expect_equal(actual[[stat]]$Estimate[i], unname(coef(ref)[1]))
        expect_equal(actual[[stat]]$StdError[i], unname(survey::SE(ref)[1]))
        expect_equal(unlist(actual[[stat]][i, c("LCB95Pct", "UCB95Pct")], use.names = FALSE),
          as.numeric(confint(ref)))
      }
    }
  }
})

test_that("nonlocal GREG retains survey FPC and never constructs neighborhoods", {
  local_mocked_bindings(localmean_weight = function(...) stop("Unexpected local calculation"))
  d <- greg_data()
  a <- greg_args(d, "SRS")
  a$fpc <- 200
  actual <- do.call(cont_analysis, a)
  d$population_size <- 200
  base <- survey::svydesign(~siteID, weights = ~weight, fpc = ~population_size, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  for (i in seq_along(levels(d$region))) {
    sub <- cal[d$region == levels(d$region)[i], ]
    expect_equal(actual$Mean$StdError[i], as.numeric(survey::SE(survey::svymean(~y, sub))))
    expect_equal(actual$Total$StdError[i], as.numeric(survey::SE(survey::svytotal(~y, sub))))
  }
})

test_that("large calibration matrices work with formula model.matrix methods loaded", {
  set.seed(189)
  x <- cbind(1, matrix(rnorm(180 * 45), 180, 45))
  colnames(x) <- c("(Intercept)", paste0("auxiliary", 1:45))
  data <- data.frame(weight = rep(5, 180), y = rnorm(180))
  base <- survey::svydesign(~1, weights = ~weight, data = data)
  totals <- colSums(x * data$weight)
  actual <- greg_calibrate(base, x, totals, "population")
  expect_equal(colSums(x * as.numeric(weights(actual$design))), totals, ignore_attr = TRUE)
  expect_equal(as.numeric(weights(actual$design)), data$weight)
})

test_that("exact calibration-model responses have no local residual variance", {
  d <- greg_data()
  d$y <- 3 + 2 * d$x
  a <- greg_args(d)
  a$subpops <- NULL
  actual <- do.call(cont_analysis, a)
  expect_equal(actual$Total$Estimate, 3 * 160 + 2 * 25)
  expect_lt(actual$Total$StdError, 1e-10)
  expect_lt(actual$Mean$StdError, 1e-10)
})

test_that("sample auxiliary signs do not restrict population totals", {
  d <- greg_data()
  d$x <- seq_len(nrow(d)) / nrow(d)
  a <- greg_args(d, "SRS")
  a$subpops <- NULL
  a$popsize <- c("(Intercept)" = 160, x = -25)
  expect_message(actual <- do.call(cont_analysis, a), "warning")
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  expect_true(any(weights(cal) < 0))
  expect_equal(actual$Total$Estimate, as.numeric(coef(survey::svytotal(~y, cal))))
  expect_equal(actual$Total$StdError, as.numeric(survey::SE(survey::svytotal(~y, cal))))
})

test_that("local GREG uses full-stratum weighted residuals and ratio covariance", {
  d <- greg_data()
  # North occupies just A; its regression still contributes in B.
  d$stratum <- d$region
  a <- greg_args(d)
  a$stratumID <- "stratum"
  actual <- do.call(cont_analysis, a)
  base <- survey::svydesign(~siteID, weights = ~weight, strata = ~stratum, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  delta <- as.numeric(d$region == "North")
  q <- delta * d$y
  fit <- stats::lm(q ~ x, d, weights = weight)
  expect_true(any(abs(residuals(fit)[delta == 0]) > 1e-6))
  for (stat in c("Mean", "Total")) {
    ref <- if (stat == "Mean") survey::svymean(~y, cal[delta == 1, ]) else survey::svytotal(~y, cal[delta == 1, ])
    u <- if (stat == "Mean") q - as.numeric(coef(ref)) * delta else q
    e <- residuals(stats::lm(u ~ x, d, weights = weight))
    z <- as.numeric(weights(cal)) * e
    if (stat == "Mean") z <- z / sum(weights(cal) * delta)
    variance <- sum(vapply(split(seq_len(nrow(d)), d$stratum), function(i) {
      nb <- localmean_weight(d$xcoord[i], d$ycoord[i], 1 / d$weight[i])
      localmean_var(z[i], nb)
    }, numeric(1)))
    expect_equal(actual[[stat]]$Estimate[1], as.numeric(coef(ref)))
    expect_equal(actual[[stat]]$StdError[1]^2, variance)
  }
})

test_that("known and unknown domain totals use distinct calibration contexts", {
  d <- greg_data()
  a <- greg_args(d)
  a$subpopsize <- list(region = list(North = c("(Intercept)" = 70, x = 5)))
  actual <- do.call(cont_analysis, a)
  d$inside <- as.numeric(d$region == "North")
  d$dx <- d$inside * d$x
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~inside + dx - 1, c(inside = 70, dx = 5))
  q <- d$inside * d$y
  e <- residuals(stats::lm(q ~ inside + dx - 1, d, weights = weight))
  expect_equal(unname(e[d$inside == 0]), rep(0, sum(d$inside == 0)), tolerance = 1e-10)
  nb <- localmean_weight(d$xcoord, d$ycoord, 1 / d$weight)
  var <- localmean_var(weights(cal) * e, nb)
  expect_equal(actual$Total$Estimate[1], sum(weights(cal) * q))
  expect_equal(actual$Total$StdError[1]^2, var)
  expect_equal(actual$Mean$Estimate[1], actual$Total$Estimate[1] / 70)
  expect_equal(actual$Mean$StdError[1]^2, var / 70^2)
  unknown <- do.call(cont_analysis, greg_args(d))
  expect_equal(actual$Mean[2, ], unknown$Mean[2, ])
  # All requested domains may supply totals without population totals.
  a$popsize <- NULL
  a$subpopsize$region$South <- c("(Intercept)" = 90, x = 20)
  expect_silent(do.call(cont_analysis, a))
})

test_that("calibrated domain denominator preserves a constant domain mean", {
  d <- greg_data()
  d$y <- 7
  actual <- do.call(cont_analysis, greg_args(d))
  expect_equal(actual$Mean$Estimate, c(7, 7))
  expect_equal(actual$Mean$StdError, c(0, 0), tolerance = 1e-10)
})

test_that("stratum-specific formula columns reproduce survey calibration", {
  d <- greg_data()
  for (f in list(~x, ~stratum + x, ~stratum + stratum:x - 1)) {
    mm <- model.matrix(f, d)
    totals <- colSums(mm * (d$weight * (1 + 0.01 * d$x)))
    a <- greg_args(d, "SRS")
    a$formula <- f
    a$popsize <- rev(totals)
    a$stratumID <- "stratum"
    a$subpops <- NULL
    actual <- do.call(cont_analysis, a)
    base <- survey::svydesign(~siteID, strata = ~stratum, weights = ~weight, data = d)
    cal <- survey::calibrate(base, f, totals)
    expect_equal(actual$Total$Estimate, as.numeric(coef(survey::svytotal(~y, cal))))
    expect_equal(actual$Mean$StdError, as.numeric(survey::SE(survey::svymean(~y, cal))))
  }
})

test_that("factor contrasts, transformations and structural zeros are preserved", {
  d <- greg_data()
  d$stratum <- factor(d$stratum, levels = c("B", "A", "unused"))
  contrasts(d$stratum) <- contr.sum(3)
  f <- ~stratum + I(x^2)
  # The contrast-coded unused level is aliased, so use observed levels here.
  d$stratum <- droplevels(d$stratum)
  contrasts(d$stratum) <- contr.sum(2)
  totals <- colSums(model.matrix(f, d) * d$weight) + c(5, 1, 2)
  a <- greg_args(d, "SRS")
  a$formula <- f
  a$popsize <- totals
  actual <- do.call(cont_analysis, a)
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, f, totals)
  expect_equal(actual$Mean$Estimate[1], as.numeric(coef(survey::svymean(~y, cal[d$region == "North", ]))))
  d$zero <- 0
  a$dframe <- d
  a$formula <- ~x + zero
  a$popsize <- c("(Intercept)" = 160, x = 25, zero = 0)
  expect_silent(do.call(cont_analysis, a))
  a$popsize["zero"] <- 1
  expect_error(do.call(cont_analysis, a), "no sample support")
})

test_that("missing responses follow survey without recalibrating respondents", {
  d <- greg_data()
  d$y[c(2, 17)] <- NA
  a <- greg_args(d, "SRS")
  actual <- do.call(cont_analysis, a)
  base <- survey::svydesign(~siteID, weights = ~weight, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  for (i in 1:2) {
    sub <- cal[d$region == levels(d$region)[i], ]
    expect_equal(actual$Mean$StdError[i], as.numeric(survey::SE(survey::svymean(~y, sub, na.rm = TRUE))))
    expect_equal(actual$Total$StdError[i], as.numeric(survey::SE(survey::svytotal(~y, sub, na.rm = TRUE))))
  }
})

test_that("category and CDF inference residualizes indicator responses", {
  d <- greg_data()
  a <- greg_args(d)
  a$vars <- "category"
  a$statistics <- NULL
  actual <- do.call(cat_analysis, a)
  a$vars <- "y"
  a$statistics <- "CDF"
  cdf <- do.call(cont_analysis, a)$CDF
  d$indicator <- as.numeric(d$category == "Good")
  b <- greg_args(d)
  b$vars <- "indicator"
  ref <- do.call(cont_analysis, b)
  expect_equal(actual$StdError.P[actual$Category == "Good"], 100 * ref$Mean$StdError)
  expect_equal(actual$Estimate.U[actual$Category == "Good"], ref$Total$Estimate)
  cut <- sort(d$y)[10]
  d$indicator <- as.numeric(d$y <= cut)
  b$dframe <- d
  ref <- do.call(cont_analysis, b)
  expect_equal(cdf$StdError.P[cdf$Value == cut], 100 * ref$Mean$StdError)
  expect_equal(cdf$Estimate.U[cdf$Value == cut], ref$Total$Estimate)
})

test_that("small-stratum fallback retains calibrated survey uncertainty", {
  d <- greg_data()
  d$stratum <- factor(c(rep("small", 3), rep("large", nrow(d) - 3)))
  a <- greg_args(d)
  a$stratumID <- "stratum"
  expect_message(actual <- do.call(cont_analysis, a), "warning")
  a$vartype <- "SRS"
  ref <- do.call(cont_analysis, a)
  expect_equal(actual, ref)
})

test_that("GREG input guards do not silently discard requested calibration", {
  a <- greg_args()
  a$formula <- y ~ x
  expect_error(do.call(cont_analysis, a), "one-sided")
  a$formula <- ~.
  expect_error(do.call(cont_analysis, a), "explicitly")
  a$formula <- ~offset(x)
  expect_error(do.call(cont_analysis, a), "Offsets")
  a$formula <- ~x
  a$popsize <- list(region = c(North = 70, South = 90))
  expect_error(do.call(cont_analysis, a), "named numeric vector")
  a$popsize <- c("(Intercept)" = 160, misspelled = 25)
  expect_error(do.call(cont_analysis, a), "model-matrix columns")
  a <- greg_args()
  a$dframe$x[1] <- NA
  expect_error(do.call(cont_analysis, a), "finite values")
  a <- greg_args()
  a$dframe$duplicate <- a$dframe$x
  a$formula <- ~x + duplicate
  a$popsize <- c(a$popsize, duplicate = 25)
  expect_error(do.call(cont_analysis, a), "rank-deficient")
  for (v in c("local", "SRS", "HT", "YG")) {
    a <- greg_args(vartype = v)
    a$clusterID <- "cluster"
    expect_error(do.call(cont_analysis, a), "two-stage")
  }
  a <- greg_args()
  a$formula <- NULL
  a$subpopsize <- list(region = list())
  expect_error(do.call(cont_analysis, a), "requires a GREG formula")
})

test_that("only formula enables GREG and explicit subset override is reported", {
  a <- greg_args()
  a$subset_local <- TRUE
  expect_message(actual <- do.call(cont_analysis, a), "warning")
  a$subset_local <- FALSE
  expect_silent(ref <- do.call(cont_analysis, a))
  expect_equal(actual, ref)
  d <- greg_data()
  p <- data.frame(region = c("North", "South"), Total = c(70, 90))
  named <- cont_analysis(d, "y", subpops = "region", xcoord = "xcoord",
    ycoord = "ycoord", popsize = p, statistics = "Mean")
  explicit <- cont_analysis(d, "y", subpops = "region", xcoord = "xcoord",
    ycoord = "ycoord", formula = NULL, popsize = p, statistics = "Mean")
  expect_identical(named, explicit)
  # Full legacy positional call, including arguments after popsize.
  positional <- cont_analysis(d, "y", "region", NULL, "weight", "xcoord",
    "ycoord", NULL, NULL, NULL, NULL, NULL, FALSE, NULL, NULL, NULL,
    p, "local", "overton", 95, c(5, 50, 95), "Mean", FALSE, TRUE)
  expect_identical(named, positional)
  positional_alias <- cont_analysis(d, "y", subpop = "region", NULL, "weight", "xcoord",
    "ycoord", NULL, NULL, NULL, NULL, NULL, FALSE, NULL, NULL, NULL,
    p, "local", "overton", 95, c(5, 50, 95), "Mean", FALSE, TRUE)
  expect_identical(named, positional_alias)
  abbreviated <- cont_analysis(d, "y", subpop = "region", xcoord = "xcoord",
    ycoord = "ycoord", popsize = p, statistics = "Mean")
  expect_identical(named, abbreviated)
  expect_identical(cat_analysis(d, "category", subpop = "region", vartype = "SRS"),
    cat_analysis(d, "category", subpops = "region", vartype = "SRS"))
})

test_that("risk roundoff guards preserve scale and genuine variance", {
  gradient <- c(1, -1)
  for (units in c(1e-20, 1, 1e20)) {
    covariance <- units * matrix(c(1, 1 + 4 * .Machine$double.eps,
      1 + 4 * .Machine$double.eps, 1), 2)
    variance <- drop(t(gradient) %*% covariance %*% gradient)
    expect_lt(variance, 0)
    expect_identical(risk_variance(variance, gradient, covariance), 0)
    expect_identical(risk_variance(units * 1e-16, gradient, covariance), units * 1e-16)
    expect_identical(risk_variance(-units * .01, gradient, covariance), -units * .01)
  }
  expect_identical(risk_variance(0, gradient, diag(2)), 0)
  expect_identical(risk_variance(NA_real_, gradient, diag(2)), NA_real_)
  expect_identical(risk_variance(NaN, gradient, diag(2)), NaN)
  expect_identical(risk_variance(-Inf, gradient, diag(2)), -Inf)
})

test_that("survey risk contrasts give zero margins only for negative roundoff", {
  cells <- c(t11 = 20, t12 = 30, t21 = 40, t22 = 50)
  expressions <- list(
    quote(t11 / (t11 + t21) - t12 / (t12 + t22)),
    quote(log(t11 / (t11 + t21)) - log(t12 / (t12 + t22))),
    quote(log((t11 + t12 + t21 + t22) * t12 / ((t11 + t12) * (t12 + t22)))),
    quote(t11 / t12 - t21 / t22))
  for (expression in expressions) {
    gradient <- as.numeric(attr(eval(deriv(expression, names(cells)),
      as.list(cells)), "gradient"))
    direction <- gradient / sqrt(sum(gradient^2))
    covariance <- diag(4) - (1 + 4 * .Machine$double.eps) * tcrossprod(direction)
    stat <- greg_stat(cells, covariance)
    reference <- survey::svycontrast(stat, expression)
    expect_lt(drop(vcov(reference)), 0)
    actual <- risk_contrast(stat, expression)
    expect_identical(coef(actual), coef(reference))
    expect_equal(as.numeric(survey::SE(actual)), 0)
    expect_equal(as.numeric(confint(actual)), rep(as.numeric(coef(actual)), 2))
    positive <- greg_stat(cells, diag(4))
    expect_identical(risk_contrast(positive, expression), survey::svycontrast(positive, expression))
    invalid <- greg_stat(cells, diag(4) - 1.01 * tcrossprod(direction))
    expect_identical(risk_contrast(invalid, expression), survey::svycontrast(invalid, expression))
  }
})

test_that("single-column calibration is warning-free and matches survey", {
  d <- greg_data()
  for (formula in list(~1, ~x - 1, ~1 + I(x * 0))) {
    x <- model.matrix(formula, d)
    totals <- colSums(x * d$weight)
    base <- survey::svydesign(~siteID, weights = ~weight, data = d)
    args <- greg_args(d, "SRS")
    args$formula <- formula
    args$popsize <- totals
    expect_warning(actual <- do.call(cont_analysis, args), NA)
    # survey's structural-zero reduction itself fails for this one-column
    # edge case in the installed version. Compare the reduced model.
    reduced <- colSums(abs(x)) != 0
    ref <- if (all(reduced)) survey::calibrate(base, formula, totals) else
      survey::calibrate(base, ~1, totals[reduced])
    for (domain in levels(d$region)) {
      refmean <- survey::svymean(~y, ref[d$region == domain, ])
      row <- actual$Mean[actual$Mean$Subpopulation == domain, ]
      expect_equal(row$Estimate, as.numeric(coef(refmean)))
      expect_equal(row$StdError, as.numeric(survey::SE(refmean)))
    }
  }
})

test_that("GREG status output follows legacy domain order", {
  d <- greg_data()
  d$region <- factor(d$region, levels = c("South", "North"))
  args <- greg_args(d, "SRS")
  args$statistics <- c("Mean", "Total", "CDF", "Pct")
  greg <- do.call(cont_analysis, args)
  args$formula <- args$popsize <- NULL
  legacy <- do.call(cont_analysis, args)
  for (part in names(legacy)) {
    expect_equal(as.character(greg[[part]]$Subpopulation), as.character(legacy[[part]]$Subpopulation))
  }
})

test_that("intercept calibration changes totals but preserves ratio means", {
  d <- greg_data()
  args <- greg_args(d)
  args$subpops <- NULL
  args$formula <- ~1
  extent <- 1.2 * sum(d$weight)
  args$popsize <- c("(Intercept)" = extent)
  calibrated <- do.call(cont_analysis, args)
  args$formula <- args$popsize <- NULL
  legacy <- do.call(cont_analysis, args)
  expect_equal(calibrated$Mean$Estimate, legacy$Mean$Estimate)
  expect_equal(calibrated$Total$Estimate, extent * legacy$Mean$Estimate)
  expect_equal(legacy$Total$Estimate, sum(d$weight * d$y))
  expect_gt(abs(calibrated$Total$Estimate - legacy$Total$Estimate), 1)
})

test_that("GREG size weights equal explicit analysis-weight products", {
  d <- greg_data()
  d$size <- seq(1, 3, length.out = nrow(d))
  d$product <- d$weight * d$size
  x <- model.matrix(~x, d)
  # External benchmarks differ from the sample's weighted auxiliary totals.
  benchmark <- d$product * (1 + .03 * d$x)
  totals <- colSums(x * benchmark)
  domains <- lapply(levels(d$region), function(level) {
    colSums(x * benchmark * (d$region == level))
  })
  names(domains) <- levels(d$region)
  for (vartype in c("local", "SRS")) for (known in c(FALSE, TRUE)) {
    args <- greg_args(d, vartype)
    args$sizeweight <- TRUE
    args$sweight <- "size"
    args$popsize <- totals
    if (known) args$subpopsize <- list(region = domains)
    actual <- do.call(cont_analysis, args)
    args$weight <- "product"
    args$sizeweight <- FALSE
    args$sweight <- NULL
    expect_equal(actual, do.call(cont_analysis, args))
    base <- survey::svydesign(~siteID, weights = ~product, data = d)
    for (i in seq_along(domains)) {
      member <- as.numeric(d$region == names(domains)[i])
      base$variables$auxiliary <- I(x * if (known) member else 1)
      target <- if (known) domains[[i]] else totals
      names(target) <- colnames(model.matrix(~auxiliary - 1, base$variables))
      cal <- survey::calibrate(base, ~auxiliary - 1, target)
      ref <- survey::svyratio(matrix(d$y * member, ncol = 1),
        matrix(member, ncol = 1), cal)
      expect_equal(actual$Mean$Estimate[i], as.numeric(coef(ref)))
      if (vartype == "SRS") expect_equal(actual$Mean$StdError[i], as.numeric(survey::SE(ref)))
    }
  }
})

test_that("size-weighted GREG reproduces known extent and model totals", {
  d <- greg_data()
  d$size <- seq(1, 3, length.out = nrow(d))
  d$extent <- 1
  d$y <- 3 + 2 * d$x
  x <- model.matrix(~x, d)
  benchmark <- d$weight * d$size * (1 + .03 * d$x)
  domains <- lapply(levels(d$region), function(level) {
    colSums(x * benchmark * (d$region == level))
  })
  names(domains) <- levels(d$region)
  args <- greg_args(d)
  args$vars <- c("extent", "y")
  args$sizeweight <- TRUE
  args$sweight <- "size"
  args$popsize <- Reduce(`+`, domains)
  args$subpopsize <- list(region = domains)
  actual <- do.call(cont_analysis, args)
  for (domain in names(domains)) {
    target <- domains[[domain]]
    rows <- actual$Total[actual$Total$Subpopulation == domain, ]
    expect_equal(rows$Estimate[rows$Indicator == "extent"], unname(target[1]))
    expect_equal(rows$Estimate[rows$Indicator == "y"], unname(3 * target[1] + 2 * target[2]))
    expect_true(all(rows$StdError < 1e-10))
  }
  args$subpops <- NULL
  args$subpopsize <- NULL
  actual <- do.call(cont_analysis, args)
  expect_equal(actual$Total$Estimate[actual$Total$Indicator == "extent"], unname(args$popsize[1]))
  expect_equal(actual$Total$Estimate[actual$Total$Indicator == "y"],
    unname(3 * args$popsize[1] + 2 * args$popsize[2]))
  expect_true(all(actual$Mean$StdError < 1e-10))
})
