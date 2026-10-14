skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"),
  "set Sys.setenv(SPSURVEY_RUN_EXTRAS = 'true') before devtools::test() to run the extras suite"
)

# Independent dense constrained least-squares reference, kept small in tests.
dense_local_reference <- function(x, y, prb, k) {
  n <- length(x)
  distance <- as.matrix(dist(cbind(x, y)))
  neighbors <- apply(distance, 2, order)[seq_len(min(k, n)), , drop = FALSE]
  i <- rep(seq_len(n), each = nrow(neighbors))
  j <- as.vector(neighbors)
  pairs <- unique(rbind(cbind(i, j), cbind(j, i)))
  pairs <- pairs[order(pairs[, 1], distance[pairs], pairs[, 2]), ]
  i <- pairs[, 1]; j <- pairs[, 2]
  degree <- tabulate(i, n)
  w <- (1 - (sequence(degree) - 1) / degree[i]) / prb[j]
  w <- w / as.numeric(rowsum(w, i))[i]
  A <- matrix(0, n, n); A[pairs] <- 1
  r <- 1 - as.numeric(rowsum(w, j))
  g <- MASS::ginv((diag(degree) - A %*% diag(1 / degree) %*% A) / 2) %*% r
  ell <- -as.numeric(A %*% g) / degree
  list(ij = unname(pairs), gwt = w + (ell[i] + g[j]) / 2)
}

test_that("single sparse solve matches dense weights and covariances", {
  set.seed(129)
  random <- cbind(runif(60), runif(60))
  grid <- as.matrix(expand.grid(x = (0:9) / 10, y = (0:9) / 10))
  duplicate <- rbind(random[1:20, ], random[1:20, ], random[1:20, ])
  disconnected <- rbind(random[1:25, ], random[26:50, ] + 100)
  for (xy in list(random, grid, grid * 10, cbind(seq_len(60), 0),
                 duplicate, disconnected, matrix(1, 20, 2))) {
    n <- nrow(xy); prb <- runif(n, .01, 2)
    z <- cbind(rnorm(n), rnorm(n))
    for (k in c(4, 15)) {
      ref <- dense_local_reference(xy[, 1], xy[, 2], prb, k)
      ans <- localmean_weight(xy[, 1], xy[, 2], prb, k)
      expect_equal(ans$ij, ref$ij)
      expect_equal(ans$gwt, ref$gwt, tolerance = 1e-10)
      expect_equal(localmean_cov(z, ans), localmean_cov(z, ref), tolerance = 1e-10)
      expect_equal(as.numeric(rowsum(ans$gwt, ans$ij[, 1])), rep(1, n), tolerance = 1e-7)
      expect_equal(as.numeric(rowsum(ans$gwt, ans$ij[, 2])), rep(1, n), tolerance = 1e-7)
    }
  }
})

test_that("neighbor ties use distance then index with bounded query/neighbor chunks", {
  queried <- numeric()
  nn2 <- RANN::nn2
  local_mocked_bindings(nn2 = function(data, query, k, ...) {
    queried <<- c(queried, nrow(query) * k)
    nn2(data, query, k = k, ...)
  }, .package = "RANN")
  actual <- localmean_neighbors(rep(0, 128), rep(0, 128), 15, max_neighbor_entries = 256)
  expect_equal(actual, matrix(rep(1:15, each = 128), nrow = 128))
  expect_lte(max(queried), 256)
  expect_gt(length(queried), 4)
})

test_that("calibrated weights including negative values preserve the dense estimator", {
  set.seed(77)
  n <- 160
  d <- data.frame(siteID = paste0("s", seq_len(n)), weight = runif(n, 5, 25),
    xcoord = runif(n), ycoord = runif(n))
  d$region <- factor(ifelse(runif(n) < .5, "R1", "R2"))
  d$stratum <- factor(ifelse(d$ycoord < .5, "S1", "S2"))
  d$value <- rnorm(n)
  d$response <- factor(ifelse(runif(n) < .4, "Poor", "Good"))
  des <- survey::svydesign(~1, weights = ~weight, data = d)
  cw <- weights(survey::calibrate(des, ~region + stratum,
    c(`(Intercept)` = 4200, regionR2 = 200, stratumS2 = 4000)))
  expect_gt(sum(cw < 0), 0)
  z <- cbind(d$value, as.numeric(d$response == "Good")) * cw
  for (k in c(4, 15)) {
    ref <- dense_local_reference(d$xcoord, d$ycoord, 1 / cw, k)
    ans <- localmean_weight(d$xcoord, d$ycoord, 1 / cw, k)
    expect_equal(ans$ij, ref$ij)
    expect_equal(ans$gwt, ref$gwt, tolerance = 1e-10)
    expect_equal(localmean_cov(z, ans), localmean_cov(z, ref), tolerance = 1e-10)
    args <- list(dframe = d, vars = "response", xcoord = "xcoord", ycoord = "ycoord",
      local_nbh = k, popsize = list(region = c(R1 = 4000, R2 = 200),
        stratum = c(S1 = 200, S2 = 4000)))
    actual <- suppressMessages(suppressWarnings(do.call(cat_analysis, args)))
    reference <- with_mocked_bindings(
      suppressMessages(suppressWarnings(do.call(cat_analysis, args))),
      localmean_weight = function(x, y, prb, nbh = 15) dense_local_reference(x, y, prb, nbh))
    expect_equal(actual, reference, tolerance = 1e-10)
    expect_true(all(is.finite(actual$Estimate.P)))
    expect_true(all(is.finite(actual$StdError.P)))
  }
})

test_that("input checks, small groups and failure caching retain their structure", {
  for (n in 2:15) {
    ans <- localmean_weight(seq_len(n), rep(0, n), rep(2, n))
    expect_equal(nrow(ans$ij), n * n)
  }
  expect_null(localmean_weight(numeric(), numeric(), numeric()))
  expect_null(localmean_weight(0, 0, 1))
  expect_gte(nrow(localmean_weight(1:16, rep(0, 16), rep(1, 16))$ij), 16 * 15)
  for (k in list(1, 2.5, NA, Inf, numeric(), c(4, 15), "15")) {
    expect_error(localmean_weight(1:4, 1:4, rep(1, 4), k), "nbh")
  }
  expect_error(localmean_weight(1:4, 1:3, rep(1, 4)), "aligned")
  for (p in c(0, NA, NaN, Inf, -Inf)) {
    expect_null(localmean_weight(1:4, 1:4, c(1, 1, 1, p)))
  }
  expect_error(localmean_weight(c(1:3, Inf), 1:4, rep(1, 4)), "finite coordinates")
  expect_error(localmean_weight(1:4, 1:4, letters[1:4]), "numeric")
  ctx <- localmean_context(4)
  expect_error(localmean_weight_internal(1:4, 1:4, rep(1, 4), 15, ctx), "does not match")
  calls <- 0L
  local_mocked_bindings(localmean_solve = function(...) { calls <<- calls + 1L; stop("singular") })
  for (i in 1:2) expect_null(localmean_weight_internal(1:4, 1:4, rep(1, 4), 4, ctx))
  expect_equal(calls, 1L)
})


local_data <- function() {
  set.seed(450)
  n <- 80
  data.frame(siteID = seq_len(n), weight = runif(n, 4, 8),
    xcoord = runif(n), ycoord = runif(n), auxiliary = rnorm(n),
    value = round(rnorm(n), 1), category = factor(rep(c("Good", "Poor"), n / 2)),
    stressor = factor(rep(c("Good", "Good", "Poor", "Poor"), n / 4)),
    domain = factor(rep(c("D1", "D2"), each = n / 2)),
    comparison = factor(rep(c("D1", "D2"), each = 4, length.out = n)),
    stratum = factor(rep(c("A", "B"), each = n / 2)))
}

test_that("complete direct and GREG analyses construct one neighborhood per block", {
  d <- local_data()
  original <- localmean_weight
  calls <- list()
  local_mocked_bindings(localmean_weight = function(x, y, prb, nbh = 15) {
    calls[[length(calls) + 1L]] <<- c(n = length(x), k = nbh)
    original(x, y, prb, nbh)
  })
  for (greg in c(FALSE, TRUE)) for (strat in c(FALSE, TRUE)) for (k in c(4, 15, 20)) {
    args <- list(dframe = d, subpops = "domain", xcoord = "xcoord", ycoord = "ycoord", local_nbh = k)
    if (strat) args$stratumID <- "stratum"
    if (greg) {
      args$formula <- ~auxiliary
      args$popsize <- colSums(model.matrix(~auxiliary, d) * d$weight)
    }
    cases <- list(cont_analysis = list(vars = "value", pctval = c(25, 50, 75)),
      cat_analysis = list(vars = "category"),
      cont_cdftest = list(vars = "value"),
      attrisk_analysis = list(vars_response = "category", vars_stressor = "stressor"),
      relrisk_analysis = list(vars_response = "category", vars_stressor = "stressor"),
      diffrisk_analysis = list(vars_response = "category", vars_stressor = "stressor"))
    for (name in names(cases)) {
      calls <- list()
      one <- args
      if (!name %in% c("cont_analysis", "cat_analysis")) one$subpops <- "comparison"
      actual <- suppressMessages(suppressWarnings(do.call(name, c(one, cases[[name]]))))
      expect_equal(length(calls), if (strat) 2 else 1, info = paste(name, greg, strat, k))
      expect_true(all(vapply(calls, function(x) x['k'] == k, logical(1))))
      expect_true(all(vapply(calls, function(x) x['n'] == if (strat) 40 else 80, logical(1))))
      uncached <- with_mocked_bindings(
        suppressMessages(suppressWarnings(do.call(name, c(one, cases[[name]])))),
        localmean_context = function(...) NULL)
      expect_identical(actual, uncached)
    }
  }
})

test_that("missing response indicators and changed weights cannot reuse incompatible weights", {
  d <- local_data(); d$value2 <- d$value; d$value2[1:5] <- NA
  original <- localmean_weight; sizes <- integer()
  local_mocked_bindings(localmean_weight = function(x, y, prb, nbh = 15) {
    sizes <<- c(sizes, length(x)); original(x, y, prb, nbh)
  })
  cont_analysis(d, c("value", "value2"), subpops = "domain", xcoord = "xcoord",
    ycoord = "ycoord", statistics = c("Mean", "Total"))
  expect_equal(sort(sizes), c(75, 80))
  ctx <- localmean_context()
  localmean_weight_internal(d$xcoord, d$ycoord, 1 / d$weight, local_context = ctx)
  localmean_weight_internal(d$xcoord, d$ycoord, 2 / d$weight, local_context = ctx)
  expect_length(ctx$neighborhoods, 2)
})

test_that("CDF warning uses double cell counts, is early and is deduplicated", {
  ctx <- localmean_context()
  expect_null(localmean_cdf_warning(10000L, 10000L, NULL, ctx))
  expect_warning(w <- localmean_cdf_warning(25000L, 25000L, NULL, ctx), "large matrices")
  expect_equal(nrow(w), 1)
  expect_silent(w2 <- localmean_cdf_warning(25000L, 25000L, w, ctx))
  expect_identical(w, w2)
  expect_warning(localmean_cdf_warning(100000L, 100000L, NULL), "large matrices")
  expect_warning(localmean_cdf_warning(25000L, 25000L, NULL, greg = TRUE), "site-by-threshold evaluations")
  d <- local_data()
  # Force a large threshold count without allocating the matrices.
  design <- survey::svydesign(~1, weights = ~weight, data = d)
  expect_warning(size_warning <- localmean_cdf_check_size(design, list(stratumID = NULL, clusterID = NULL),
    "value", "domain", "D1", 2e6, FALSE, NULL, localmean_context()), "large matrices")
  expect_true(is.na(size_warning$stratum))
  expect_silent(localmean_cdf_check_size(design, list(stratumID = NULL, clusterID = NULL),
    "value", "domain", "D1", 2e6, TRUE, NULL, localmean_context()))
})

test_that("single-stratum domains equal independent within-stratum calculations", {
  d <- local_data()
  # A domain occupying only part of A; the other domain spans both strata.
  d$domain <- factor(ifelse(d$stratum == "A" & d$siteID <= 28, "inside", "other"))
  args <- list(dframe = d, subpops = "domain", xcoord = "xcoord", ycoord = "ycoord",
    stratumID = "stratum", subset_local = FALSE, local_nbh = 4)
  for (fun in c("cat_analysis", "cont_analysis")) {
    args$vars <- if (fun == "cat_analysis") "category" else "value"
    actual <- do.call(fun, args)
    one <- args; one$dframe <- d[d$stratum == "A", ]; one$stratumID <- NULL
    ref <- do.call(fun, one)
    compare <- function(a, b) {
      a <- a[a$Subpopulation == "inside", ]; b <- b[b$Subpopulation == "inside", ]
      # Full CDF grids include values observed only in the other stratum.
      if ("Value" %in% names(a)) a <- a[a$Value %in% b$Value, ]
      rownames(a) <- rownames(b) <- NULL
      expect_equal(a, b, tolerance = 1e-10)
    }
    if (is.data.frame(actual)) compare(actual, ref) else {
      for (stat in names(actual)) compare(actual[[stat]], ref[[stat]])
    }
  }
})

test_that("CDF comparisons retain margins when groups occupy different strata", {
  d <- local_data()
  original <- localmean_weight; sizes <- numeric()
  local_mocked_bindings(localmean_weight = function(x, y, prb, nbh = 15) {
    sizes <<- c(sizes, length(x)); original(x, y, prb, nbh)
  })
  for (test in c("adjWald", "RaoScott_First")) {
    sizes <- numeric()
    expect_error(suppressMessages(cont_cdftest(d, "value", subpops = "domain",
      stratumID = "stratum", xcoord = "xcoord", ycoord = "ycoord", testname = test)), NA)
    expect_equal(sizes, c(40, 40))
    # The third stratum is outside either group in one of the comparisons.
    third <- d[1:40, ]; third$siteID <- 81:120
    third$stratum <- "C"; third$domain <- "D3"
    expect_error(suppressMessages(cont_cdftest(rbind(d, third), "value", subpops = "domain",
      stratumID = "stratum", xcoord = "xcoord", ycoord = "ycoord", testname = test)), NA)
  }
})

test_that("CDF cell and margin covariance matches independent stratum contrasts", {
  d <- local_data()
  third <- d[1:40, ]; third$siteID <- 81:120; third$stratum <- "C"
  d <- rbind(d, third)
  d$rowvar <- factor(ifelse(d$stratum == "A", "Subpopulation 1",
    ifelse(d$stratum == "B", "Subpopulation 2", NA)),
    levels = c("Subpopulation 1", "Subpopulation 2"))
  d$colvar <- factor(ifelse(d$value > 0, "high", "low"))
  d$wgt <- d$weight
  design <- survey::svydesign(~1, strata = ~stratum, weights = ~weight, data = d)
  dn <- list(stratumID = "stratum", clusterID = NULL, xcoord = "xcoord", ycoord = "ycoord")
  cells <- outer(as.character(d$rowvar), c("Subpopulation 1", "Subpopulation 2"), "==") * 1
  bins <- outer(as.character(d$colvar), levels(d$colvar), "==") * 1
  mm <- cbind(cells * bins[, 1], cells * bins[, 2])
  mm[is.na(mm)] <- 0; cells[is.na(cells)] <- 0
  member <- !is.na(d$rowvar)
  total_mm <- cbind(mm, cells, bins, 1) * member
  ref_total <- matrix(0, 9, 9); ref_prop <- matrix(0, 4, 4)
  for (s in c("A", "B")) {
    ids <- which(d$stratum == s)
    w <- d$weight[ids]
    edge <- dense_local_reference(d$xcoord[ids], d$ycoord[ids], 1 / w, 4)
    # Independent dense contrast formula, without either production wrapper.
    W <- matrix(0, length(ids), length(ids)); W[edge$ij] <- edge$gwt
    covariance <- function(z) crossprod(z) - crossprod(W %*% z)
    ref_total <- ref_total + covariance(total_mm[ids, ] * w)
    means <- colSums(mm[ids, ] * w) / sum(w)
    residuals <- sweep(mm[ids, ], 2, means) * w
    ref_prop <- ref_prop + covariance(residuals) / sum(d$weight)^2
  }
  a <- list(design = design, design_names = dn, warn_ind = FALSE, warn_df = NULL,
    warn_vec = c("domain", "D1", "value"), subset_local = FALSE, local_nbh = 4)
  actual_total <- do.call(cdftest_localmean_total, a)
  actual_prop <- do.call(cdftest_localmean_prop, a)
  expect_false(actual_total$warn_ind)
  expect_false(actual_prop$warn_ind)
  expect_equal(unname(actual_total$varest), ref_total, tolerance = 1e-10)
  expect_equal(unname(actual_prop$varest), ref_prop, tolerance = 1e-10)
})

test_that("two-stage defaults and uncached calls reflect the requested count", {
  d <- local_data()
  d$cluster <- rep(1:8, each = 10)
  d$weight1 <- 2
  d$x1 <- rep(seq_len(8), each = 10); d$y1 <- rep(c(0, 1), each = 40)
  a <- list(dframe = d, vars = "value", xcoord = "xcoord", ycoord = "ycoord",
    clusterID = "cluster", weight1 = "weight1", xcoord1 = "x1", ycoord1 = "y1",
    statistics = c("Mean", "Total"), local_nbh = 4)
  original <- localmean_weight; calls <- list()
  local_mocked_bindings(localmean_weight = function(x, y, prb, nbh = 15) {
    calls[[length(calls) + 1L]] <<- c(n = length(x), k = nbh)
    original(x, y, prb, nbh)
  })
  actual <- do.call(cont_analysis, a)
  expect_true(all(vapply(calls, function(x) x['k'] == 4, logical(1))))
  expect_true(all(c(8, 10) %in% vapply(calls, function(x) x['n'], numeric(1))))
  expect_identical(actual, do.call(cont_analysis, c(a, list(subset_local = TRUE))))
  expect_error(do.call(cont_analysis, c(a, list(subset_local = FALSE))), "two-stage")
  expect_error(do.call(cont_analysis, c(a, list(vartype = "SRS", subset_local = FALSE))), NA)
  for (k in c(4, 15, 20)) {
    calls <- list()
    ans <- mean_var(d$value, d$weight, d$xcoord, d$ycoord,
      weighted.mean(d$value, d$weight), FALSE, NULL, FALSE,
      warn_ind = FALSE, warn_df = NULL, warn_vec = rep(NA, 3),
      subset_local = TRUE, local_nbh = k)
    expect_equal(calls[[1]]['k'], c(k = k))
    expect_true(is.finite(ans$varest))
  }
})

test_that("change and trend keep occasion context and propagate local_nbh", {
  d <- local_data()
  panel <- rbind(transform(d, occasion = "one"), transform(d, occasion = "two", value = value + .3))
  years <- do.call(rbind, lapply(1:3, function(j) transform(d, year = j, value = value + .1 * j)))
  original <- localmean_weight; counts <- numeric()
  local_mocked_bindings(localmean_weight = function(x, y, prb, nbh = 15) {
    counts <<- c(counts, nbh); original(x, y, prb, nbh)
  })
  for (greg in c(FALSE, TRUE)) for (k in c(4, 20)) {
    counts <- numeric()
    a <- list(dframe = panel, vars_cont = "value", test = c("mean", "total"),
      surveyID = "occasion", xcoord = "xcoord", ycoord = "ycoord", local_nbh = k)
    if (greg) {
      a$formula <- ~auxiliary
      total <- colSums(model.matrix(~auxiliary, d) * d$weight)
      a$popsize <- list(one = total, two = total)
    }
    suppressWarnings(suppressMessages(do.call(change_analysis, a)))
    expect_gte(length(counts), 2)
    expect_true(all(counts == k))
    counts <- numeric()
    a <- list(dframe = years, vars_cont = "value", yearID = "year", model_cont = "WLR",
      xcoord = "xcoord", ycoord = "ycoord", local_nbh = k)
    if (greg) { a$formula <- ~auxiliary; a$popsize <- list('1' = total, '2' = total, '3' = total) }
    suppressWarnings(suppressMessages(do.call(trend_analysis, a)))
    expect_length(counts, 3)
    expect_true(all(counts == k))
  }
})

test_that("time analyses retain strata when the stratum column identifies domains", {
  d <- local_data()
  set.seed(983)
  years <- do.call(rbind, lapply(1:4, function(j) {
    one <- d
    one$year <- j
    one$value <- one$value + j * .2 + rnorm(nrow(d), sd = .4)
    one$category <- factor(ifelse(runif(nrow(d)) < .3 + j * .08, "Good", "Poor"))
    one
  }))
  panel <- subset(years, year %in% 1:2)
  panel$occasion <- ifelse(panel$year == 1, "one", "two")
  for (fun in c("change_analysis", "trend_analysis")) {
    a <- list(dframe = if (fun == "change_analysis") panel else years,
      vars_cont = "value", vars_cat = "category", subpops = "stratum",
      stratumID = "stratum", xcoord = "xcoord", ycoord = "ycoord",
      subset_local = FALSE, local_nbh = 4)
    if (fun == "change_analysis") a$surveyID <- "occasion" else {
      a$yearID <- "year"; a$model_cont <- a$model_cat <- "WLR"
    }
    actual <- suppressMessages(suppressWarnings(do.call(fun, a)))
    for (s in levels(d$stratum)) {
      one <- a
      one$dframe <- one$dframe[one$dframe$stratum == s, ]
      one$stratumID <- NULL
      ref <- suppressMessages(suppressWarnings(do.call(fun, one)))
      for (stat in names(actual)) {
        x <- actual[[stat]]; y <- ref[[stat]]
        if (!is.data.frame(x) || !"Subpopulation" %in% names(x)) next
        x <- x[x$Subpopulation == s, ]; y <- y[y$Subpopulation == s, ]
        rownames(x) <- rownames(y) <- NULL
        expect_gt(nrow(x), 0)
        expect_equal(x, y, tolerance = 1e-10, info = paste(fun, stat, s))
      }
    }
  }
})

test_that("public defaults and small-domain fallbacks are independent of neighbor count", {
  for (name in c("cat_analysis", "cont_analysis", "cont_cdftest", "attrisk_analysis",
                 "relrisk_analysis", "diffrisk_analysis", "change_analysis", "trend_analysis")) {
    args <- formals(get(name))
    expect_identical(tail(names(args), 1), "local_nbh")
    expect_identical(args$local_nbh, 15)
    expect_null(args$subset_local)
  }
  d <- local_data()
  expect_error(suppressMessages(cont_analysis(d, "value", xcoord = "xcoord", ycoord = "ycoord",
    statistics = "Mean", local_nbh = 3.5)), "preceding")
  expect_identical(cont_analysis(d, "value", vartype = "SRS", statistics = "Mean", local_nbh = "ignored"),
    cont_analysis(d, "value", vartype = "SRS", statistics = "Mean"))
  local_mocked_bindings(localmean_weight = function(...) stop("Unexpected construction"))
  ans <- mean_var(d$value, d$weight, d$xcoord, d$ycoord,
    weighted.mean(d$value[1:3], d$weight[1:3]), FALSE, NULL, FALSE,
    warn_ind = FALSE, warn_df = NULL, warn_vec = rep(NA, 3),
    subset_local = FALSE, subpop_ind = as.numeric(seq_len(nrow(d)) <= 3), local_nbh = 15)
  expect_identical(ans$vartype, "SRS")
})
