skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))

contrast_fixture <- function() {
  set.seed(5)
  n <- 160
  d <- data.frame(id = seq_len(n), w = runif(n, 2, 4), xc = runif(n),
    yc = runif(n), x = runif(n), h = factor(rep(c("H1", "H2"), each = n / 2)),
    domain = factor(rep(c("East", "West"), n / 2)))
  d$y <- 4 + d$x + rnorm(n)
  d$category <- factor(ifelse(d$y > 4.5, "Good", "Poor"))
  d$wgt <- d$w
  d
}

contrast_args <- function(d, vartype = "SRS") {
  list(dframe = d, vars_cont = "y", subpops = "domain", siteID = "id",
    contrasts = "difference",
    weight = "w", xcoord = "xc", ycoord = "yc", vartype = vartype,
    return_joint = TRUE)
}

test_that("direct joint estimates and contrasts reproduce survey", {
  for (stratified in c(FALSE, TRUE)) for (fpc in c(FALSE, TRUE)) {
    d <- contrast_fixture()
    d$N <- if (stratified) 500 else 1000
    a <- contrast_args(d)
    if (stratified) a$stratumID <- "h"
    if (fpc) a$fpc <- if (stratified) list(H1 = 500, H2 = 500) else 1000
    actual <- do.call(contrast_analysis, a)
    base <- survey::svydesign(~id, strata = if (stratified) ~h else NULL,
      weights = ~w, data = d, fpc = if (fpc) ~N else NULL)
    for (statistic in c("Mean", "Total")) {
      ref <- survey::svyby(~y, ~domain, base,
        if (statistic == "Mean") survey::svymean else survey::svytotal,
        covmat = TRUE, keep.var = TRUE)
      joint <- actual$joint$domain$y[[statistic]]
      expect_equal(as.numeric(coef(joint)), as.numeric(coef(ref)))
      expect_equal(unname(vcov(joint)), unname(vcov(ref)), tolerance = 1e-10)
      expect_identical(attr(joint, "statistic"), tolower(statistic))
      table <- actual[[paste0("contsum_", tolower(statistic))]]
      for (name in "East - West") {
        expected <- survey::svycontrast(ref, c(East = 1, West = -1))
        row <- table[table$Contrast == name, ]
        expect_equal(row$Estimate, as.numeric(coef(expected)), tolerance = 1e-12)
        expect_equal(row$StdError^2, as.numeric(vcov(expected)), tolerance = 1e-10)
      }
    }
  }
})

test_that("HT and YG joint covariance follows survey PPS designs", {
  d <- contrast_fixture()
  for (type in c("HT", "YG")) {
    a <- contrast_args(d, type)
    actual <- do.call(contrast_analysis, a)
    base <- survey::svydesign(~id, probs = ~I(1 / w), data = d,
      pps = "overton", variance = type)
    for (statistic in c("Mean", "Total")) {
      # svyby cannot supply joint influence for every PPS adapter. Build the
      # unweighted ratio contributions independently for survey's svytotal.
      u <- sapply(levels(d$domain), function(domain) {
        v <- as.numeric(d$domain == domain)
        if (statistic == "Total") return(v * d$y)
        v * (d$y - weighted.mean(d$y[v == 1], d$w[v == 1])) / sum(d$w * v)
      })
      expect_equal(unname(vcov(actual$joint$domain$y[[statistic]])),
        unname(vcov(survey::svytotal(u, base))), tolerance = 1e-10)
    }
  }
})

test_that("two-stage joint estimation follows survey and the retained local formula", {
  d <- contrast_fixture()
  d$psu <- rep(1:16, each = 10)
  d$w1 <- rep(seq(3, 6, length.out = 16), each = 10)
  d$x1 <- rep(seq(0, 1, length.out = 16), each = 10)
  d$y1 <- rep(rep(c(0, 1), 8), each = 10)
  d$wgt1 <- d$w1
  d$wgt2 <- d$w
  for (type in c("SRS", "Local")) for (greg in c(FALSE, TRUE)) {
    a <- contrast_args(d, type)
    a$stratumID <- "h"
    a$clusterID <- "psu"
    a$weight1 <- "w1"
    a$xcoord1 <- "x1"
    a$ycoord1 <- "y1"
    base <- survey::svydesign(~psu + id, strata = ~h, weights = ~w1 + w,
      data = d, nest = TRUE)
    cal <- base
    if (greg) {
      a$formula <- ~x
      a$popsize <- colSums(model.matrix(~x, d) * weights(base)) * 1.03
      cal <- survey::calibrate(base, ~x, a$popsize)
    }
    actual <- do.call(contrast_analysis, a)
    for (statistic in c("Mean", "Total")) {
      z <- sapply(levels(d$domain), function(domain) {
        v <- as.numeric(d$domain == domain)
        q <- d$y * v
        est <- if (statistic == "Mean") coef(survey::svyratio(q, v, cal)) else coef(survey::svytotal(q, cal))
        u <- if (statistic == "Mean") q - as.numeric(est) * v else q
        if (greg) u <- lm.wfit(model.matrix(~x, d), u, weights(base))$residuals
        z <- weights(cal) * u
        if (statistic == "Mean") z <- z / sum(weights(cal) * v)
        z
      })
      ref <- vcov(survey::svytotal(z / weights(base), base))
      if (type == "Local") {
        ref <- matrix(0, 2, 2)
        for (h in levels(d$h)) {
          groups <- split(which(d$h == h), d$psu[d$h == h])
          first <- vapply(groups, function(i) i[1], integer(1))
          totals <- do.call(rbind, lapply(groups, function(i) colSums(z[i, ])))
          nb <- localmean_weight(d$x1[first], d$y1[first], 1 / d$w1[first])
          ref <- ref + localmean_cov(totals, nb)
          for (i in groups) {
            nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
            ref <- ref + d$w1[i[1]] * localmean_cov(z[i, ] / d$w1[i], nb)
          }
        }
      }
      expect_equal(unname(vcov(actual$joint$domain$y[[statistic]])),
        unname(ref), tolerance = 1e-10)
    }
  }
})

test_that("pairwise differences use represented levels and numeric labels", {
  d <- contrast_fixture()
  d$domain <- factor(rep(c("c", "t", "EPA Region 1"), length.out = nrow(d)),
    levels = c("t", "c", "EPA Region 1", "Unused"))
  a <- contrast_args(d)
  for (greg in c(FALSE, TRUE)) {
    if (greg) {
      a$formula <- ~x
      a$popsize <- colSums(model.matrix(~x, d) * d$w)
    }
    actual <- do.call(contrast_analysis, a)
    expect_identical(actual$contsum_mean$Contrast, c("t - c", "t - EPA Region 1", "c - EPA Region 1"))
    joint <- actual$joint$domain$y$Mean
    for (pair in combn(c("t", "c", "EPA Region 1"), 2, simplify = FALSE)) {
      expected <- survey::svycontrast(joint, setNames(c(1, -1), pair))
      row <- actual$contsum_mean[actual$contsum_mean$Contrast == paste(pair, collapse = " - "), ]
      expect_equal(row$Estimate, as.numeric(coef(expected)))
      expect_equal(row$StdError, as.numeric(survey::SE(expected)))
    }
  }
  a$dframe$y[d$domain == "c"] <- NA
  actual <- suppressMessages(do.call(contrast_analysis, a))
  expect_true(all(is.na(actual$contsum_mean$Estimate[c(1, 3)])))
  expect_true(is.finite(actual$contsum_mean$Estimate[2]))
  a$dframe$domain <- factor(rep("one", nrow(d)))
  expect_error(do.call(contrast_analysis, a), "at least two represented levels")
})

test_that("joint indefinite covariance uses one survey fallback while singular PSD is valid", {
  d <- contrast_fixture()
  a <- contrast_args(d, "Local")
  original <- localmean_cov
  local_mocked_bindings(localmean_cov = function(z, weight, ...) {
    if (ncol(as.matrix(z)) == 2L) return(matrix(c(1, 2, 2, 1), 2))
    original(z, weight, ...)
  })
  actual <- suppressMessages(do.call(contrast_analysis, a))
  marginal <- cont_analysis(d, "y", subpops = "domain", weight = "w",
    xcoord = "xc", ycoord = "yc", statistics = "Mean")$Mean
  expect_false(isTRUE(all.equal(as.numeric(diag(vcov(actual$joint$domain$y$Mean))),
    marginal$StdError^2)))
  warning_table <- get("warn_df", envir = .GlobalEnv)
  expect_true(any(warning_table$subpoptype == "domain" &
    warning_table$indicator == "y: Mean", na.rm = TRUE))
  a$vartype <- "SRS"
  ref <- do.call(contrast_analysis, a)
  expect_equal(actual$joint$domain$y, ref$joint$domain$y)
  expect_true(contrast_valid_covariance(matrix(c(1, -1, -1, 1), 2)))
  expect_false(contrast_valid_covariance(matrix(c(1, 2, 2, 1), 2)))
  expect_true(contrast_valid_covariance(matrix(0, 2, 2)))
})

test_that("zero and unavailable differences retain their meaning", {
  d <- contrast_fixture()
  a <- contrast_args(d, "Local")
  a$dframe$y <- 3
  a$statistics <- "Mean"
  actual <- do.call(contrast_analysis, a)
  expect_equal(actual$contsum_mean$Estimate, 0, tolerance = 1e-12)
  expect_lt(actual$contsum_mean$StdError, 1e-12)
  a$dframe$y <- NA_real_
  actual <- suppressMessages(do.call(contrast_analysis, a))
  expect_true(is.na(actual$contsum_mean$Estimate))
})

test_that("local covariance uses independently constructed domain contributions", {
  for (stratified in c(FALSE, TRUE)) {
    d <- contrast_fixture()
    a <- contrast_args(d, "Local")
    if (stratified) a$stratumID <- "h" else d$h <- "H"
    actual <- do.call(contrast_analysis, a)
    for (statistic in c("Mean", "Total")) {
      z <- sapply(levels(d$domain), function(domain) {
        member <- d$domain == domain
        if (statistic == "Total") return(d$w * member * d$y)
        mean <- sum(d$w * member * d$y) / sum(d$w * member)
        d$w * member * (d$y - mean) / sum(d$w * member)
      })
      ref <- matrix(0, 2, 2)
      contrast_var <- 0
      for (i in split(seq_len(nrow(d)), d$h)) {
        nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
        ref <- ref + localmean_cov(z[i, ], nb)
        contrast_var <- contrast_var + localmean_var(z[i, 1] - z[i, 2], nb)
      }
      joint <- actual$joint$domain$y[[statistic]]
      expect_equal(unname(vcov(joint)), ref, tolerance = 1e-10)
      expect_equal(ref, t(ref), tolerance = 1e-12)
      expect_gt(abs(ref[1, 2]), 1e-9)
      expect_equal(as.numeric(vcov(survey::svycontrast(joint, c(East = 1, West = -1)))),
        contrast_var, tolerance = 1e-10)
      ca <- a
      ca$vars_cont <- ca$contrasts <- ca$return_joint <- NULL
      ca$vars <- "y"
      marginal <- do.call(cont_analysis, ca)[[statistic]]
      expect_equal(diag(vcov(joint)), setNames(marginal$StdError^2, levels(d$domain)), tolerance = 1e-10)
    }
  }
})

test_that("GREG joint covariance matches independent weighted fits and survey calibration", {
  d <- contrast_fixture()
  d$y[c(1, 15)] <- NA
  base <- survey::svydesign(~id, strata = ~h, weights = ~w, data = d)
  for (formula in list(~x, ~0 + x, ~0 + h)) for (known in c(FALSE, TRUE)) {
    mm <- model.matrix(formula, d)
    totals <- colSums(mm * d$w) * 1.05
    a <- contrast_args(d)
    a$stratumID <- "h"
    a$formula <- formula
    a$popsize <- totals
    if (known) {
      a$subpopsize <- list(domain = lapply(levels(d$domain), function(domain)
        colSums(mm * (d$domain == domain) * d$w) * 1.05))
      names(a$subpopsize$domain) <- levels(d$domain)
    }
    actual <- do.call(contrast_analysis, a)
    for (statistic in c("Mean", "Total")) {
      estimates <- numeric(2)
      z <- matrix(0, nrow(d), 2)
      for (j in 1:2) {
        domain <- levels(d$domain)[j]
        member <- d$domain == domain
        X <- if (known) mm * member else mm
        aux <- X
        colnames(aux) <- paste0("x", seq_len(ncol(X)))
        augmented <- base
        augmented$variables$aux <- I(aux)
        population <- if (known) a$subpopsize$domain[[domain]] else totals
        names(population) <- survey::cal_names(~aux - 1, augmented)
        cal <- survey::calibrate(augmented, ~aux - 1, population)
        v <- as.numeric(member & !is.na(d$y))
        q <- d$y
        q[v == 0] <- 0
        ref <- if (statistic == "Mean") survey::svyratio(q, v, cal) else survey::svytotal(q, cal)
        estimates[j] <- as.numeric(coef(ref))
        u <- if (statistic == "Mean") q - estimates[j] * v else q
        z[, j] <- weights(cal) * lm.wfit(X, u, d$w)$residuals
        if (statistic == "Mean") z[, j] <- z[, j] / sum(weights(cal) * v)
        expect_equal(vcov(actual$joint$domain$y[[statistic]])[j, j],
          as.numeric(vcov(ref)), tolerance = 1e-10)
      }
      ref_cov <- vcov(survey::svytotal(z / d$w, base))
      joint <- actual$joint$domain$y[[statistic]]
      expect_equal(as.numeric(coef(joint)), estimates, tolerance = 1e-10)
      expect_equal(unname(vcov(joint)), unname(ref_cov), tolerance = 1e-10)
    }
  }
})

test_that("subset-local covariance is diagonal and otherwise has no effect", {
  d <- contrast_fixture()
  a <- contrast_args(d, "Local")
  a$subset_local <- TRUE
  actual <- suppressMessages(do.call(contrast_analysis, a))
  ca <- a
  ca$vars_cont <- ca$contrasts <- ca$return_joint <- NULL
  ca$vars <- "y"
  marginal <- do.call(cont_analysis, ca)
  for (statistic in c("Mean", "Total")) {
    joint <- actual$joint$domain$y[[statistic]]
    expect_equal(unname(vcov(joint)), diag(marginal[[statistic]]$StdError^2))
    difference <- actual[[paste0("contsum_", tolower(statistic))]][1, ]
    expect_equal(difference$StdError^2, sum(marginal[[statistic]]$StdError^2))
    expect_true(attr(joint, "subset_independence"))
  }
  a$vartype <- "SRS"
  yes <- do.call(contrast_analysis, a)
  a$subset_local <- FALSE
  expect_identical(do.call(contrast_analysis, a), yes)
  a$vartype <- "local"
  a$formula <- ~x
  a$popsize <- c("(Intercept)" = sum(d$w), x = sum(d$w * d$x))
  no <- do.call(contrast_analysis, a)
  a$subset_local <- TRUE
  expect_identical(suppressMessages(do.call(contrast_analysis, a)), no)
})

test_that("categorical contrasts use percentages and responses have separate masks", {
  d <- contrast_fixture()
  d$y2 <- d$y
  d$y2[1:25] <- NA
  a <- contrast_args(d)
  a$vars_cat <- "category"
  a$vars_cont <- c("y", "y2")
  actual <- do.call(contrast_analysis, a)
  a$vars_cont <- "y"
  simpler <- do.call(contrast_analysis, a)
  expect_identical(actual$joint$domain$y, simpler$joint$domain$y)
  base <- survey::svydesign(~id, weights = ~w, data = d)
  ref <- survey::svyby(~y2, ~domain, base, survey::svymean, na.rm = TRUE, covmat = TRUE)
  expect_equal(unname(vcov(actual$joint$domain$y2$Mean)), unname(vcov(ref)), tolerance = 1e-10)
  for (category in levels(d$category)) {
    base$variables$binary <- as.numeric(d$category == category)
    ref <- survey::svyby(~binary, ~domain, base, survey::svymean, covmat = TRUE)
    joint <- actual$joint$domain$category$Proportion[[category]]
    expect_equal(as.numeric(coef(joint)), 100 * as.numeric(coef(ref)))
    expect_equal(unname(vcov(joint)), 10000 * unname(vcov(ref)), tolerance = 1e-10)
    expect_identical(attr(joint, "statistic"), "proportion")
  }
})

test_that("subset categorical variances are the standard category marginals", {
  d <- contrast_fixture()
  d$category <- factor(rep(c("Low", "Middle", "High"), length.out = nrow(d)))
  a <- contrast_args(d, "Local")
  a$vars_cont <- NULL
  a$vars_cat <- "category"
  a$subset_local <- TRUE
  a$stratumID <- "h"
  actual <- suppressMessages(do.call(contrast_analysis, a))
  b <- a
  b$vars_cat <- b$contrasts <- b$return_joint <- NULL
  b$vars <- "category"
  marginal <- do.call(cat_analysis, b)
  for (category in levels(d$category)) {
    rows <- marginal[marginal$Category == category, ]
    for (statistic in c("Proportion", "Total")) {
      joint <- actual$joint$domain$category[[statistic]][[category]]
      expected <- if (statistic == "Proportion") rows$StdError.P else rows$StdError.U
      expect_equal(unname(vcov(joint)), diag(expected^2), tolerance = 1e-10)
    }
  }
})

test_that("Two-stage FPC and population calibration reproduce survey end to end", {
  data("api", package = "survey", envir = environment())
  d <- apiclus2
  d$w1 <- d$fpc1 / length(unique(d$dnum))
  d$w <- d$fpc2 / ave(d$snum, d$dnum, FUN = length)
  d$group <- factor(ifelse(d$stype == "E", "Elementary", "Other"))
  a <- list(dframe = d, vars_cont = "api00", subpops = "group",
    contrasts = "difference", weight = "w", weight1 = "w1",
    clusterID = "dnum", siteID = "snum", vartype = "SRS", return_joint = TRUE)
  sizes <- tapply(d$fpc2, d$dnum, function(x) x[1])
  a$fpc <- c(Ncluster = d$fpc1[1], sizes)
  base <- survey::svydesign(~dnum + snum, weights = ~w1 + w,
    fpc = ~fpc1 + fpc2, data = d)
  for (greg in c(FALSE, TRUE)) {
    cal <- base
    if (greg) {
      a$formula <- ~api99
      a$popsize <- c("(Intercept)" = nrow(apipop), api99 = sum(apipop$api99))
      cal <- survey::calibrate(base, ~api99, a$popsize)
    }
    actual <- do.call(contrast_analysis, a)
    for (statistic in c("Mean", "Total")) {
      if (!greg) {
        ref <- survey::svyby(~api00, ~group, cal,
          if (statistic == "Mean") survey::svymean else survey::svytotal,
          covmat = TRUE, influence = TRUE)
      } else {
        # survey's svyby influence assembly errors on this calibrated two-stage
        # design. Its joint totals and svycontrast still provide an entirely
        # survey-computed reference, including every cross-domain covariance.
        v <- as.numeric(d$group == "Elementary")
        totals <- survey::svytotal(cbind(Ey = v * d$api00, Wy = (1 - v) * d$api00,
          En = v, Wn = 1 - v), cal)
        ref <- if (statistic == "Mean") survey::svycontrast(totals,
          list(Elementary = quote(Ey / En), Other = quote(Wy / Wn))) else
          survey::svycontrast(totals, list(Elementary = c(Ey = 1), Other = c(Wy = 1)))
      }
      joint <- actual$joint$group$api00[[statistic]]
      expect_equal(as.numeric(coef(joint)), as.numeric(coef(ref)), tolerance = 1e-10)
      expect_equal(unname(vcov(joint)), unname(vcov(ref)), tolerance = 1e-10)
      for (comparison in "Elementary - Other") {
        expected <- survey::svycontrast(ref, c(Elementary = 1, Other = -1))
        table <- actual[[paste0("contsum_", tolower(statistic))]]
        row <- table[table$Contrast == comparison, ]
        expect_equal(row$Estimate, as.numeric(coef(expected)), tolerance = 1e-10)
        expect_equal(row$StdError, as.numeric(survey::SE(expected)), tolerance = 1e-10)
      }
    }
  }
})

test_that("GREG local covariance matches independent fits with signed weights", {
  d <- contrast_fixture()
  a <- contrast_args(d, "Local")
  a$formula <- ~x
  a$popsize <- c("(Intercept)" = sum(d$w), x = 1.5 * sum(d$w))
  base <- survey::svydesign(~id, weights = ~w, data = d)
  cal <- survey::calibrate(base, ~x, a$popsize)
  expect_true(any(weights(cal) < 0))
  actual <- suppressMessages(do.call(contrast_analysis, a))
  nb <- localmean_weight(d$xc, d$yc, 1 / d$w)
  for (statistic in c("Mean", "Total")) {
    z <- sapply(levels(d$domain), function(domain) {
      v <- as.numeric(d$domain == domain)
      q <- v * d$y
      est <- if (statistic == "Mean") coef(survey::svyratio(q, v, cal)) else coef(survey::svytotal(q, cal))
      u <- if (statistic == "Mean") q - as.numeric(est) * v else q
      z <- weights(cal) * lm.wfit(model.matrix(~x, d), u, d$w)$residuals
      if (statistic == "Mean") z <- z / sum(weights(cal) * v)
      z
    })
    expected <- localmean_cov(z, nb)
    expect_gt(min(eigen(expected, symmetric = TRUE)$values), 0)
    expect_equal(unname(vcov(actual$joint$domain$y[[statistic]])), expected, tolerance = 1e-10)
  }
})

test_that("several domain partitions and labels remain separate", {
  d <- contrast_fixture()
  d$other <- factor(ifelse(d$x > .5, "Upper half", "Lower half"))
  a <- contrast_args(d)
  a$subpops <- c("domain", "other")
  actual <- do.call(contrast_analysis, a)
  expect_identical(names(actual$joint), a$subpops)
  a$subpops <- "other"
  expected <- do.call(contrast_analysis, a)
  expect_identical(actual$joint$other, expected$joint$other)
  expect_identical(actual$catsum, NULL)
})

test_that("local categorical covariance and response masks agree with hand contributions", {
  d <- contrast_fixture()
  d$category[c(1, 21, 122)] <- NA
  a <- contrast_args(d, "Local")
  a$vars_cont <- NULL
  a$vars_cat <- "category"
  a$stratumID <- "h"
  actual <- do.call(contrast_analysis, a)
  for (category in levels(d$category)) for (statistic in c("Proportion", "Total")) {
    observed <- !is.na(d$category)
    q <- as.numeric(d$category == category)
    q[!observed] <- 0
    z <- sapply(levels(d$domain), function(domain) {
      v <- as.numeric(observed & d$domain == domain)
      if (statistic == "Total") return(d$w * v * q)
      estimate <- sum(d$w * v * q) / sum(d$w * v)
      100 * d$w * v * (q - estimate) / sum(d$w * v)
    })
    ref <- matrix(0, 2, 2)
    for (i in split(which(observed), d$h[observed])) {
      nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
      ref <- ref + localmean_cov(z[i, ], nb)
    }
    expect_equal(unname(vcov(actual$joint$domain$category[[statistic]][[category]])),
      ref, tolerance = 1e-10)
  }
})

test_that("the wrapper accepts only the difference string", {
  a <- contrast_args(contrast_fixture())
  for (value in list("ratio", "dif", NA_character_, character(), c("difference", "difference"),
    list(domain = list(difference = c(East = 1, West = -1))), quote(East - West))) {
    a$contrasts <- value
    expect_error(do.call(contrast_analysis, a, quote = TRUE), 'contrasts must be "difference"')
  }
})

test_that("sparse direct domains use a coherent survey fallback", {
  d <- contrast_fixture()
  d$domain <- factor(ifelse(d$id <= 3, "East", "West"))
  a <- contrast_args(d, "Local")
  actual <- suppressMessages(do.call(contrast_analysis, a))
  a$vartype <- "SRS"
  expected <- do.call(contrast_analysis, a)
  expect_equal(actual$joint, expected$joint)
})
