# Only fast checks for the CRAN suite.
test_that("GREG status estimates agree with survey", {
  d <- data.frame(siteID = 1:16, weight = 5, x = seq(-1, 1, length.out = 16),
    y = cos(1:16))
  totals <- c("(Intercept)" = 80, x = 4)
  actual <- cont_analysis(d, vars = "y", formula = ~x, popsize = totals,
    vartype = "SRS", statistics = c("Mean", "Total"))
  design <- survey::calibrate(survey::svydesign(~1, weights = ~weight, data = d), ~x, totals)
  ref <- survey::svymean(~y, design)
  expect_equal(actual$Mean$Estimate, as.numeric(coef(ref)))
  expect_equal(actual$Mean$StdError, as.numeric(survey::SE(ref)))
  expect_equal(actual$Total$Estimate, as.numeric(coef(survey::svytotal(~y, design))))
})

test_that("local GREG removes an exact auxiliary response", {
  d <- data.frame(siteID = 1:16, weight = 5, x = seq(-1, 1, length.out = 16),
    xc = rep(1:4, 4), yc = rep(1:4, each = 4))
  d$y <- 3 + 2 * d$x
  actual <- cont_analysis(d, vars = "y", formula = ~x,
    popsize = c("(Intercept)" = 80, x = 4), xcoord = "xc", ycoord = "yc",
    statistics = "Mean")$Mean
  expect_equal(actual$Estimate, 3.1)
  expect_lt(actual$StdError, 1e-10)
})

test_that("a NULL formula retains direct estimation", {
  d <- data.frame(siteID = 1:16, weight = 5, y = cos(1:16))
  expected <- cont_analysis(d, vars = "y", vartype = "SRS", statistics = "Mean")
  actual <- cont_analysis(d, vars = "y", formula = NULL, vartype = "SRS", statistics = "Mean")
  expect_identical(actual, expected)
})

test_that("cached GREG residuals agree with lm.wfit on the response scale", {
  d <- data.frame(w = seq(2, 5, length.out = 12), x = seq(-1, 2, length.out = 12))
  base <- survey::svydesign(~1, weights = ~w, data = d)
  X <- model.matrix(~x, d)
  context <- greg_calibrate(base, X, c("(Intercept)" = 45, x = 20), "test")
  y <- sin(seq_len(nrow(d)))
  y[c(2, 8)] <- NA
  v <- as.numeric(!is.na(y) & d$x > 0)
  q <- y
  q[v == 0] <- 0
  ratio <- sum(context$calibrated_weights * q) / sum(context$calibrated_weights * v)
  u <- cbind(total = q, mean = q - ratio * v)
  expected <- lm.wfit(x = X, y = u, w = d$w)$residuals
  expect_equal(unname(greg_residual(context, u)), unname(expected), tolerance = 1e-12)
  expect_equal(as.numeric(greg_residual(context, u)[, 2]),
    as.numeric(greg_residual(context, q) - ratio * greg_residual(context, v)), tolerance = 1e-12)
})
