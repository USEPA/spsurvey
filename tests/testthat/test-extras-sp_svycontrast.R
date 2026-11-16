skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))

joint_fixture <- function() {
  set.seed(5)
  n <- 240
  d <- data.frame(id = seq_len(n), psu = rep(1:12, each = 20),
    h = factor(rep(c("H1", "H2"), each = n / 2)),
    domain = factor(rep(c("East", "West"), n / 2)), w = runif(n, 2, 4),
    x = runif(n), xc = runif(n), yc = runif(n))
  d$w1 <- rep(seq(2, 4, length.out = 12), each = 20)
  d$x1 <- rep(seq(0, 1, length.out = 12), each = 20)
  d$yc1 <- rep(rep(c(0, 1), 6), each = 20)
  d$y1 <- 20 + 2 * d$x + 5 * d$xc + rnorm(n)
  d$y2 <- 10 + d$x + 3 * d$xc + rnorm(n, sd = .5)
  d$category <- factor(ifelse(d$y1 > median(d$y1), "Good", "Poor"))
  d
}

joint_args <- function(d, kind = "single", vartype = "SRS", greg = "direct") {
  a <- list(dframe = d, vars_cont = c("y1", "y2"), vars_cat = "category",
    subpops = "domain", siteID = "id", weight = "w", xcoord = "xc", ycoord = "yc",
    vartype = vartype, statistics = "Mean")
  w <- d$w
  if (kind != "single") a$stratumID <- "h"
  if (kind == "two_stage") {
    a$clusterID <- "psu"; a$weight1 <- "w1"; a$xcoord1 <- "x1"; a$ycoord1 <- "yc1"
    w <- w * d$w1
  }
  if (greg != "direct") {
    a$formula <- ~x
    a$popsize <- colSums(model.matrix(~x, d) * w) * 1.03
    if (greg == "domain") a$subpopsize <- list(domain = lapply(
      split(seq_len(nrow(d)), d$domain), function(i)
        colSums(model.matrix(~x, d)[i, ] * w[i]) * 1.03))
  }
  a
}

# Independent reference: survey computes each point estimate and its marginal
# variance; lm.wfit constructs the projected columns for cross-context covariance.
joint_reference <- function(a, terms) {
  d <- a$dframe
  base <- if (!is.null(a$clusterID)) survey::svydesign(~psu + id, strata = ~h,
    weights = ~w1 + w, data = d, nest = TRUE) else survey::svydesign(~id,
      strata = if (is.null(a$stratumID)) NULL else ~h, weights = ~w, data = d)
  w <- as.numeric(weights(base))
  estimates <- marginal_variance <- numeric(nrow(terms))
  z <- matrix(0, nrow(d), nrow(terms))
  for (j in seq_len(nrow(terms))) {
    term <- terms[j, ]
    y <- d[[term$variable]]
    if (!is.na(term$category)) y <- as.numeric(y == term$category)
    member <- if (is.na(term$level)) rep(TRUE, nrow(d)) else d$domain == term$level
    v <- as.numeric(member & !is.na(y))
    q <- y; q[v == 0] <- 0
    cal <- base
    if (!is.null(a$formula)) {
      X <- model.matrix(a$formula, d)
      target <- a$popsize
      if (!is.null(a$subpopsize)) {
        X <- X * member
        target <- a$subpopsize$domain[[term$level]]
      }
      augmented <- base
      augmented$variables$aux <- I(X)
      names(target) <- survey::cal_names(~aux - 1, augmented)
      cal <- survey::calibrate(augmented, ~aux - 1, target)
    }
    mean <- a$statistics == "Mean"
    ref <- if (mean) survey::svyratio(q, v, cal) else survey::svytotal(q, cal)
    estimates[j] <- as.numeric(coef(ref)) * term$scale
    marginal_variance[j] <- as.numeric(vcov(ref)) * term$scale^2
    u <- if (mean) q - as.numeric(coef(ref)) * v else q
    if (!is.null(a$formula)) u <- lm.wfit(X, u, w)$residuals
    z[, j] <- weights(cal) * u * term$scale / if (mean) sum(weights(cal) * v) else 1
  }
  V <- vcov(survey::svytotal(z / w, base))
  if (a$vartype == "Local") {
    rows <- if (!is.null(a$formula)) seq_len(nrow(d)) else which(!is.na(d$y1))
    V <- matrix(0, ncol(z), ncol(z))
    strata <- if (is.null(a$stratumID)) rep("All", nrow(d)) else d$h
    for (idx in split(rows, strata[rows])) {
      if (is.null(a$clusterID)) {
        nb <- localmean_weight(d$xc[idx], d$yc[idx], 1 / w[idx])
        V <- V + localmean_cov(z[idx, , drop = FALSE], nb)
      } else {
        groups <- split(idx, d$psu[idx])
        first <- vapply(groups, function(i) i[1], integer(1))
        psu_totals <- do.call(rbind, lapply(groups, function(i) colSums(z[i, , drop = FALSE])))
        nb <- localmean_weight(d$x1[first], d$yc1[first], 1 / d$w1[first])
        V <- V + localmean_cov(psu_totals, nb)
        for (i in groups) {
          nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$w[i])
          V <- V + d$w1[i[1]] * localmean_cov(z[i, , drop = FALSE] / d$w1[i], nb)
        }
      }
    }
  }
  list(estimate = estimates, covariance = V, marginal = marginal_variance)
}

test_that("joint matrices match independent survey and local calculations", {
  d <- joint_fixture()
  for (kind in c("single", "stratified", "two_stage")) {
    for (greg in c("direct", "population", "domain")) for (vt in c("SRS", "Local")) {
      for (statistic in c("Mean", "Total")) {
        a <- joint_args(d, kind, vt, greg); a$statistics <- statistic
        joint <- do.call(sp_svycontrast, a)
        ref <- joint_reference(a, attr(joint, "terms"))
        expect_equal(as.numeric(coef(joint)), ref$estimate, tolerance = 1e-10)
        expect_equal(unname(vcov(joint)), unname(ref$covariance), tolerance = 1e-9)
        if (vt == "SRS") expect_equal(unname(diag(vcov(joint))), ref$marginal, tolerance = 1e-9)
        expect_null(attr(joint, "warnings"))
        expect_true(contrast_valid_covariance(vcov(joint)))
        reference <- greg_stat(ref$estimate, ref$covariance, names(coef(joint)))
        for (expr in list(quote(y1.East - y2.East), quote(y1.East / y2.East),
            quote((y1.East - y2.East) - (y1.West - y2.West)))) {
          actual <- survey::svycontrast(joint, expr)
          expected <- survey::svycontrast(reference, expr)
          expect_equal(coef(actual), coef(expected), tolerance = 1e-10)
          expect_equal(vcov(actual), vcov(expected), tolerance = 1e-9)
        }
      }
    }
  }
})

test_that("population estimates and common missing rows preserve the intended design", {
  d <- joint_fixture()
  d[1:12, c("y1", "y2", "category")] <- NA
  for (kind in c("single", "stratified", "two_stage")) {
    for (greg in c("direct", "population")) for (vt in c("SRS", "Local")) {
      a <- joint_args(d, kind, vt, greg); a$subpops <- NULL
      joint <- do.call(sp_svycontrast, a)
      ref <- joint_reference(a, attr(joint, "terms"))
      expect_equal(as.numeric(coef(joint)), ref$estimate, tolerance = 1e-10)
      expect_equal(unname(vcov(joint)), unname(ref$covariance), tolerance = 1e-9)
      expect_true(all(is.na(attr(joint, "terms")$level)))
      b <- a; b$vars_cont <- b$vars_cat <- NULL; b$vars <- c("y1", "y2")
      marginal <- suppressMessages(do.call(cont_analysis, b))$Mean
      expect_equal(as.numeric(coef(joint)[c("y1", "y2")]), marginal$Estimate, tolerance = 1e-10)
      expect_equal(as.numeric(sqrt(diag(vcov(joint)))[1:2]), marginal$StdError, tolerance = 1e-9)
      b <- a; b$subset_local <- TRUE
      if (greg == "direct") expect_equal(vcov(suppressMessages(do.call(sp_svycontrast, b))), vcov(joint))
    }
  }
  a <- joint_args(d)
  a$dframe$y2[20] <- NA
  expect_error(do.call(sp_svycontrast, a), "same missing rows")
  a$vars_cont <- "y1"; a$vars_cat <- NULL
  expect_s3_class(do.call(sp_svycontrast, a), "svystat")
  a$dframe$y1 <- NA_real_
  expect_error(do.call(sp_svycontrast, a), "No requested estimates")
})

test_that("subset blocks retain within-domain covariance and drop nonresponding PSUs", {
  for (missing_psu in c(FALSE, TRUE)) for (kind in c("single", "stratified", "two_stage")) {
    d <- joint_fixture()
    d$domain <- factor(ifelse(d$psu %in% c(1:5, 7:11), "East", "West"))
    if (missing_psu) d[d$psu == 1, c("y1", "y2", "category")] <- NA
    d$difference <- d$y1 - d$y2
    for (statistic in c("Mean", "Total")) {
      a <- joint_args(d, kind, "Local"); a$subset_local <- TRUE; a$statistics <- statistic
      joint <- suppressMessages(do.call(sp_svycontrast, a))
      terms <- attr(joint, "terms")
      east <- terms$level == "East"
      expect_true(all(vcov(joint)[east, !east] == 0))
      expect_true(attr(joint, "subset_independence"))
      b <- a; b$vars_cont <- b$vars_cat <- NULL; b$vars <- c("y1", "y2", "difference")
      marginal <- suppressMessages(do.call(cont_analysis, b))[[statistic]]
      rows <- marginal$Subpopulation == "East"
      expect_equal(as.numeric(sqrt(diag(vcov(joint)))[match(c("y1.East", "y2.East"), names(coef(joint)))]),
        marginal$StdError[rows & marginal$Indicator != "difference"], tolerance = 1e-9)
      diff <- survey::svycontrast(joint, quote(y1.East - y2.East))
      expect_equal(as.numeric(survey::SE(diff)), marginal$StdError[rows & marginal$Indicator == "difference"], tolerance = 1e-9)
      b$vars <- "category"
      b$statistics <- NULL
      cat <- suppressMessages(do.call(cat_analysis, b))
      cat <- cat[cat$Subpopulation == "East" & cat$Category %in% levels(d$category), ]
      idx <- match(paste("category", cat$Category, "East", sep = "."), names(coef(joint)))
      expect_equal(as.numeric(sqrt(diag(vcov(joint)))[idx]),
        if (statistic == "Mean") cat$StdError.P else cat$StdError.U, tolerance = 1e-9)
    }
  }
})

test_that("unavailable domains do not contaminate surviving expressions", {
  d <- joint_fixture()
  d$domain <- factor(rep(c("East", "West", "Missing"), length.out = nrow(d)))
  d[d$domain == "Missing", c("y1", "y2", "category")] <- NA
  a <- joint_args(d)
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  expect_true(all(attr(joint, "unavailable")$level == "Missing"))
  expect_true(is.finite(survey::SE(survey::svycontrast(joint, quote(y1.East / y2.West)))))
  expect_identical(attr(joint, "terms")$term, names(coef(joint)))
})

test_that("failed subset blocks are omitted while full-design failures error", {
  a <- joint_args(joint_fixture(), vartype = "Local")
  original <- contrast_covariance
  local_mocked_bindings(contrast_covariance = function(...) {
    args <- list(...)
    result <- original(...)
    if (identical(args[[7]][2], "East") || is.na(args[[7]][2])) {
      result$covariance[,] <- NA_real_; result$method <- "Unavailable"
    }
    result
  })
  a$subset_local <- TRUE
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  expect_true(all(attr(joint, "terms")$level == "West"))
  expect_true(all(attr(joint, "unavailable")$level == "East"))
  expect_true(is.finite(survey::SE(survey::svycontrast(joint, quote(y1.West - y2.West)))))
  a$subset_local <- FALSE
  expect_error(do.call(sp_svycontrast, a), "joint covariance is unavailable")
})

test_that("constructor names, scopes, and signed calibration are explicit", {
  d <- joint_fixture()
  d$category <- factor(rep(c("a", "a..1"), length.out = nrow(d)))
  d$category.a <- d$y1
  d$category.a..1 <- d$y2
  a <- joint_args(d); a$subpops <- NULL
  a$vars_cont <- c("category.a", "category.a..1")
  joint <- do.call(sp_svycontrast, a)
  expect_identical(names(coef(joint)), c("category.a", "category.a..1", "category.a..2", "category.a..1..1"))
  expect_identical(attr(joint, "terms")$term, names(coef(joint)))
  a$subpops <- c("domain", "h")
  expect_error(do.call(sp_svycontrast, a), "one column name")
  a <- joint_args(d); a$statistics <- c("Mean", "Total")
  expect_error(do.call(sp_svycontrast, a), "statistics must")
  a <- joint_args(d, greg = "population"); a$subpops <- NULL
  a$popsize <- c("(Intercept)" = sum(d$w), x = 1.5 * sum(d$w))
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  expect_true(any(grepl("negative weights", attr(joint, "warnings")$warning)))
  a$popsize <- NULL
  expect_error(do.call(sp_svycontrast, a), "require popsize")
})

test_that("joint calibration supports no intercept and size constraints", {
  d <- joint_fixture()
  for (form in list(~0 + x, ~0 + h)) for (domain in c(FALSE, TRUE)) {
    a <- joint_args(d, greg = "population"); a$formula <- form
    X <- model.matrix(form, d)
    a$popsize <- colSums(X * d$w) * 1.03
    if (domain) a$subpopsize <- list(domain = lapply(split(seq_len(nrow(d)), d$domain),
      function(i) colSums(X[i, , drop = FALSE] * d$w[i]) * 1.03))
    for (statistic in c("Mean", "Total")) {
      a$statistics <- statistic
      joint <- do.call(sp_svycontrast, a)
      ref <- joint_reference(a, attr(joint, "terms"))
      expect_equal(as.numeric(coef(joint)), ref$estimate, tolerance = 1e-9)
      expect_equal(unname(vcov(joint)), unname(ref$covariance), tolerance = 1e-9)
    }
  }
})

# A survey total of response and membership columns supplies a separate joint
# reference. survey::svycontrast then forms all domain means at once.
joint_survey_reference <- function(d, design, statistic) {
  E <- as.numeric(d$domain == "East"); W <- 1 - E
  totals <- survey::svytotal(cbind(y1E = d$y1 * E, y1W = d$y1 * W,
    y2E = d$y2 * E, y2W = d$y2 * W, E = E, W = W), design)
  if (statistic == "Mean") survey::svycontrast(totals, list(
    y1.East = quote(y1E / E), y1.West = quote(y1W / W),
    y2.East = quote(y2E / E), y2.West = quote(y2W / W))) else
    survey::svycontrast(totals, list(y1.East = c(y1E = 1), y1.West = c(y1W = 1),
      y2.East = c(y2E = 1), y2.West = c(y2W = 1)))
}

test_that("constructor supports PPS, size weights, FPC and direct poststratification", {
  d <- joint_fixture(); d$N <- 1500; d$area <- 1 + d$x
  for (option in c("HT", "YG", "size", "FPC", "poststrata")) {
    a <- joint_args(d); a$vars_cat <- NULL
    base <- survey::svydesign(~id, weights = ~w, data = d)
    if (option %in% c("HT", "YG")) {
      a$vartype <- option
      base <- survey::svydesign(~id, probs = ~I(1 / w), data = d,
        pps = "overton", variance = option)
    } else if (option == "size") {
      a$sizeweight <- TRUE; a$sweight <- "area"
      base <- survey::svydesign(~id, weights = ~I(w * area), data = d)
    } else if (option == "FPC") {
      a$fpc <- d$N[1]
      base <- survey::svydesign(~id, weights = ~w, fpc = ~N, data = d)
    } else {
      a$popsize <- data.frame(domain = factor(c("East", "West")), Freq = c(500, 550))
      base <- survey::postStratify(base, ~domain, a$popsize)
    }
    for (statistic in c("Mean", "Total")) {
      a$statistics <- statistic
      joint <- do.call(sp_svycontrast, a)
      ref <- joint_survey_reference(d, base, statistic)
      expect_equal(as.numeric(coef(joint)), as.numeric(coef(ref)), tolerance = 1e-9)
      expect_equal(unname(vcov(joint)), unname(vcov(ref)), tolerance = 1e-9)
      coefficient <- c(y1.East = 2, y1.West = -1, y2.East = .5)
      out <- survey::svycontrast(joint, coefficient)
      idx <- names(coefficient)
      expect_equal(as.numeric(vcov(out)), as.numeric(t(coefficient) %*%
        vcov(joint)[idx, idx] %*% coefficient), tolerance = 1e-10)
    }
  }
})

test_that("constructor two-stage FPC matches a survey joint ratio calculation", {
  data("api", package = "survey", envir = environment())
  d <- apiclus2
  d$w1 <- d$fpc1 / length(unique(d$dnum))
  d$w <- d$fpc2 / ave(d$snum, d$dnum, FUN = length)
  d$domain <- factor(ifelse(d$stype == "E", "East", "West"))
  d$y1 <- d$api00; d$y2 <- d$api99
  a <- list(dframe = d, vars_cont = c("y1", "y2"), subpops = "domain",
    weight = "w", weight1 = "w1", clusterID = "dnum", siteID = "snum", vartype = "SRS",
    fpc = c(Ncluster = d$fpc1[1], tapply(d$fpc2, d$dnum, function(x) x[1])))
  base <- survey::svydesign(~dnum + snum, weights = ~w1 + w, fpc = ~fpc1 + fpc2, data = d)
  for (greg in c(FALSE, TRUE)) {
    cal <- base
    if (greg) {
      a$formula <- ~api99
      a$popsize <- c("(Intercept)" = nrow(apipop), api99 = sum(apipop$api99))
      cal <- survey::calibrate(base, ~api99, a$popsize)
    }
    for (statistic in c("Mean", "Total")) {
      a$statistics <- statistic
      joint <- do.call(sp_svycontrast, a)
      ref <- joint_survey_reference(d, cal, statistic)
      expect_equal(as.numeric(coef(joint)), as.numeric(coef(ref)), tolerance = 1e-9)
      expect_equal(unname(vcov(joint)), unname(vcov(ref)), tolerance = 1e-9)
    }
  }
})

test_that("small domains fall back coherently but rare categories alone do not", {
  d <- joint_fixture(); d$domain <- factor(c(rep("East", 3), rep("West", nrow(d) - 3)))
  a <- joint_args(d, vartype = "Local")
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  b <- a; b$vartype <- "SRS"
  ref <- do.call(sp_svycontrast, b)
  expect_equal(vcov(joint), vcov(ref))
  expect_identical(attr(joint, "covariance_method")$used, "Survey fallback")
  a$subset_local <- TRUE
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  idx <- attr(joint, "terms")$level == "East"
  expect_equal(vcov(joint)[idx, idx], vcov(ref)[idx, idx])
  expect_true(all(vcov(joint)[idx, !idx] == 0))
  d <- joint_fixture(); d$category <- factor(c("Rare", rep("Common", nrow(d) - 1)))
  a <- joint_args(d, vartype = "Local"); a$vars_cont <- NULL
  joint <- do.call(sp_svycontrast, a)
  expect_identical(attr(joint, "covariance_method")$used, "Local")
  expect_null(attr(joint, "warnings"))
})

test_that("no valid subset covariance produces an informative error", {
  a <- joint_args(joint_fixture(), vartype = "Local"); a$subset_local <- TRUE
  original <- contrast_covariance
  local_mocked_bindings(contrast_covariance = function(...) {
    result <- original(...); result$covariance[,] <- NA_real_; result
  })
  expect_error(suppressMessages(do.call(sp_svycontrast, a)), "No requested estimates have usable covariance")
})

test_that("constructor uses represented domains in the supplied factor order", {
  d <- joint_fixture()
  d$domain <- factor(rep(c("West", "East", "North"), length.out = nrow(d)),
    levels = c("West", "Unused", "East", "North"))
  domains <- c("West", "East", "North")
  expected <- as.vector(t(outer(c("y1", "y2", "category.Good", "category.Poor"),
    domains, paste, sep = ".")))
  for (greg in c("direct", "population")) for (vt in c("SRS", "Local")) {
    for (statistic in c("Mean", "Total")) {
      a <- joint_args(d, vartype = vt, greg = greg); a$statistics <- statistic
      joint <- do.call(sp_svycontrast, a)
      expect_identical(names(coef(joint)), expected)
      expect_identical(attr(joint, "terms")$term, expected)
      expect_identical(rownames(vcov(joint)), expected)
      expect_identical(colnames(vcov(joint)), expected)
      expect_null(attr(joint, "unavailable"))
      expect_null(attr(joint, "warnings"))
      # Independent references also verify that reordering keeps every estimate,
      # count and cross-covariance attached to the correct domain.
      ref <- joint_reference(a, attr(joint, "terms"))
      expect_equal(as.numeric(coef(joint)), ref$estimate, tolerance = 1e-10)
      expect_equal(unname(vcov(joint)), unname(ref$covariance), tolerance = 1e-9)
      expect_equal(attr(joint, "terms")$nResp, rep(80L, length(expected)))

      a$dframe[a$dframe$domain == "East", c("y1", "y2", "category")] <- NA
      joint <- suppressMessages(do.call(sp_svycontrast, a))
      expect_identical(names(coef(joint)), expected[!grepl("East$", expected)])
      unavailable <- attr(joint, "unavailable")
      expect_identical(unavailable$term, expected[grepl("East$", expected)])
      expect_true(all(unavailable$level == "East"))
      expect_true(all(unavailable$nResp == 0))
    }
  }
  # The subset path must retain the same order and align its domain blocks.
  a <- joint_args(d, vartype = "Local"); a$subset_local <- TRUE
  joint <- suppressMessages(do.call(sp_svycontrast, a))
  expect_identical(names(coef(joint)), expected)
  expect_identical(attr(joint, "covariance_method")$domain, domains)
  expect_null(attr(joint, "unavailable"))
  levels <- attr(joint, "terms")$level
  expect_true(all(vcov(joint)[outer(levels, levels, "!=")] == 0))
})
