test_that("contrast_analysis assigns a domain difference to survey", {
  d <- data.frame(y = c(2, 4, 8, 3, 7, 9), weight = 2,
    domain = factor(rep(c("A", "B"), each = 3)))
  actual <- contrast_analysis(d, vars_cont = "y", subpops = "domain",
    contrasts = "difference",
    vartype = "SRS", statistics = "Mean", return_joint = TRUE)
  base <- survey::svydesign(~1, weights = ~weight, data = d)
  ref <- survey::svyby(~y, ~domain, base, survey::svymean, covmat = TRUE)
  expected <- survey::svycontrast(ref, c(A = 1, B = -1))
  expect_equal(actual$contsum_mean$Estimate, as.numeric(coef(expected)))
  expect_equal(actual$contsum_mean$StdError, as.numeric(survey::SE(expected)))
  expect_null(actual$catsum)
  expect_null(actual$contsum_total)
  expect_error(contrast_analysis(d, vars_cont = "y", subpops = "domain",
    contrasts = "ratio", vartype = "SRS"),
    'contrasts must be "difference"')
})
