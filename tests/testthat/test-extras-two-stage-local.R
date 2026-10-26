skip_on_cran()
skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))

test_that("default clustered CDF tests handle exhaustive groups in NLA_IN", {
  data <- new.env()
  load(system.file("extdata", "NLA_IN.rda", package = "spsurvey"), envir = data)
  for (year in unique(data$NLA_IN$YEAR)) for (group in c("LAKE_ORGN", "URBN_NLA17")) {
    d <- subset(data$NLA_IN, YEAR == year)
    actual <- cont_cdftest(d, vars = "ContVar", subpops = group,
      siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
      clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1")
    expect_true(all(is.finite(actual$p_Value)))
  }
})

cluster_fixture <- function(stratified = FALSE) {
  set.seed(5)
  n <- 160L
  d <- data.frame(siteID = seq_len(n), cluster = rep(1:16, each = 10),
    weight = runif(n, 2, 4), weight1 = rep(seq(3, 6, length.out = 16), each = 10),
    xc = runif(n), yc = runif(n), x = rnorm(n), y = rnorm(n))
  d$x1 <- rep(runif(16), each = 10)
  d$y1 <- rep(runif(16), each = 10)
  d$y <- 2 + 3 * d$x + d$y
  d$region <- factor(ifelse(d$cluster <= 5, "A", "B"))
  d$category <- factor(ifelse(d$y > 2, "Good", "Poor"))
  d$stratum <- if (stratified) rep(c("H1", "H2"), each = 80) else "H1"
  d$wgt1 <- d$weight1
  d$wgt2 <- d$weight
  d
}

cluster_names <- list(clusterID = "cluster", xcoord = "xc", ycoord = "yc",
  xcoord1 = "x1", ycoord1 = "y1")

cluster_args <- function(d, type = "Local") {
  list(dframe = d, vars = "y", subpops = "region", statistics = c("Mean", "Total"),
    weight = "weight", weight1 = "weight1", clusterID = "cluster",
    xcoord = "xc", ycoord = "yc", xcoord1 = "x1", ycoord1 = "y1", vartype = type)
}

# Independent application of the documented formula using only exported local
# weight/covariance functions, not the new stage-combination helper.
cluster_reference <- function(d, z) {
  z <- as.matrix(z)
  ans <- matrix(0, ncol(z), ncol(z))
  for (h in unique(d$stratum)) {
    idx <- which(d$stratum == h)
    groups <- split(idx, d$cluster[idx])
    u <- matrix(unlist(lapply(groups, function(i) colSums(z[i, , drop = FALSE]))),
      nrow = length(groups), byrow = TRUE)
    first <- vapply(groups, `[`, integer(1), 1L)
    nb <- localmean_weight(d$x1[first], d$y1[first], 1 / d$weight1[first])
    ans <- ans + localmean_cov(u, nb)
    for (i in groups) {
      nb <- localmean_weight(d$xc[i], d$yc[i], 1 / d$weight[i])
      ans <- ans + d$weight1[i[1]] * localmean_cov(z[i, , drop = FALSE] / d$weight1[i], nb)
    }
  }
  ans
}

test_that("the shared two-stage operator preserves off-diagonal covariance", {
  for (stratified in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    base <- survey::svydesign(~cluster + siteID, strata = ~stratum,
      weights = ~weight1 + weight, data = d, nest = TRUE)
    z <- cbind(d$y, d$x, d$y <= 2) * weights(base)
    actual <- localmean_cluster_design(base, z, cluster_names)
    expect_equal(actual$covariance, cluster_reference(d, z), tolerance = 1e-10)
    expect_true(abs(actual$covariance[1, 2]) > 0)
  }
})

test_that("two-stage full-domain direct and GREG linearizations match the formula", {
  for (stratified in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    a <- cluster_args(d)
    if (stratified) a$stratumID <- "stratum"
    a$subset_local <- FALSE
    base <- survey::svydesign(~cluster + siteID, strata = ~stratum,
      weights = ~weight1 + weight, data = d, nest = TRUE)
    for (greg in c(FALSE, TRUE)) {
      if (greg) {
        a$formula <- ~x
        a$popsize <- c("(Intercept)" = sum(weights(base)) * 1.05, x = 20)
      }
      cal <- if (greg) survey::calibrate(base, ~x, a$popsize) else base
      actual <- do.call(cont_analysis, a)
      member <- as.numeric(d$region == "A")
      for (stat in c("Mean", "Total")) {
        ref <- if (stat == "Mean") survey::svyratio(member * d$y, member, cal) else
          survey::svytotal(member * d$y, cal)
        estimate <- as.numeric(coef(ref))
        u <- member * if (stat == "Mean") d$y - estimate else d$y
        if (greg) u <- residuals(lm(u ~ x, d, weights = weights(base)))
        z <- weights(cal) * u
        if (stat == "Mean") z <- z / sum(weights(cal) * member)
        expect_equal(actual[[stat]]$Estimate[1], estimate)
        expect_equal(actual[[stat]]$StdError[1]^2, as.numeric(cluster_reference(d, z)), tolerance = 1e-8)
        if (greg) expect_true(any(abs(z[member == 0]) > 1e-8)) else expect_true(all(z[member == 0] == 0))
      }
      if (greg) {
        a$subset_local <- TRUE
        expect_identical(do.call(cont_analysis, a), actual)
      }
    }
  }
})

test_that("nonlocal two-stage calibration follows survey with and without FPC", {
  for (stratified in c(FALSE, TRUE)) for (fpc in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    d$Ncluster <- if (stratified) 40 else 80
    d$stage1size <- 100
    a <- cluster_args(d, "SRS")
    if (stratified) a$stratumID <- "stratum"
    if (fpc) {
      make_fpc <- function(ids) c(Ncluster = d$Ncluster[1],
        setNames(rep(100, length(ids)), as.character(ids)))
      a$fpc <- if (stratified) list(H1 = make_fpc(1:8), H2 = make_fpc(9:16)) else make_fpc(1:16)
    }
    base <- survey::svydesign(~cluster + siteID, strata = ~stratum,
      weights = ~weight1 + weight, data = d, nest = TRUE,
      fpc = if (fpc) ~Ncluster + stage1size else NULL)
    a$formula <- ~x
    a$popsize <- c("(Intercept)" = sum(weights(base)), x = 20)
    cal <- survey::calibrate(base, ~x, a$popsize)
    actual <- do.call(cont_analysis, a)
    for (i in 1:2) {
      sub <- cal[d$region == levels(d$region)[i], ]
      for (stat in c("Mean", "Total")) {
        ref <- if (stat == "Mean") survey::svymean(~y, sub) else survey::svytotal(~y, sub)
        expect_equal(actual[[stat]]$Estimate[i], as.numeric(coef(ref)))
        expect_equal(actual[[stat]]$StdError[i], as.numeric(survey::SE(ref)))
        expect_equal(as.numeric(actual[[stat]][i, c("LCB95Pct", "UCB95Pct")]), as.numeric(confint(ref)))
      }
    }
  }
})

test_that("cluster GREG handles exact responses, domain totals, and negative weights", {
  d <- cluster_fixture()
  a <- cluster_args(d)
  a$formula <- ~x
  a$popsize <- c("(Intercept)" = sum(d$weight * d$weight1), x = 20)
  a$subpops <- NULL
  a$dframe$y <- 2 + 3 * d$x
  exact <- do.call(cont_analysis, a)
  expect_lt(exact$Total$StdError, 1e-9)
  expect_lt(exact$Mean$StdError, 1e-10)
  a$subpops <- "region"
  a$subpopsize <- list(region = list(A = c("(Intercept)" = 200, x = 5)))
  known <- do.call(cont_analysis, a)
  expect_equal(known$Total$Estimate[1], 415)
  a$dframe <- d
  a$subpopsize <- NULL
  a$popsize['x'] <- 10000
  expect_message(negative <- do.call(cont_analysis, a), "warning")
  expect_true(all(is.finite(negative$Total$StdError)))
  a$formula <- ~x + I(2 * x)
  a$popsize <- c("(Intercept)" = 2000, x = 20, "I(2 * x)" = 40)
  expect_error(do.call(cont_analysis, a), "rank-deficient")
})

test_that("cluster neighborhoods are reused only for matching construction inputs", {
  d <- cluster_fixture()
  base <- survey::svydesign(~cluster + siteID, weights = ~weight1 + weight, data = d)
  context <- localmean_context()
  nb <- localmean_weight
  calls <- 0L
  local_mocked_bindings(localmean_weight = function(...) { calls <<- calls + 1L; nb(...) })
  z <- d$y * weights(base)
  a <- localmean_cluster_design(base, z, cluster_names, local_context = context)
  expect_equal(calls, 17L)
  b <- localmean_cluster_design(base, 2 * z, cluster_names, local_context = context)
  expect_equal(calls, 17L)
  expect_equal(b$covariance, 4 * a$covariance)
  base$variables$xc[1] <- base$variables$xc[1] + 1
  localmean_cluster_design(base, z, cluster_names, local_context = context)
  expect_equal(calls, 18L)
})

test_that("cluster GREG cross-occasion CDF tests remain explicitly unsupported", {
  expect_error(cont_cdftest(data.frame(), vars = "y", formula = ~x,
    clusterID = "cluster", surveyID = "year"), "surveyID = NULL")
})

test_that("all eight public functions support two-stage full-domain defaults", {
  d <- cluster_fixture()
  d$region <- factor(rep(c("A", "B", "C", "C"), 40))
  d$stressor <- factor(rep(c("Good", "Poor"), 80))
  a <- cluster_args(d)
  a$statistics <- a$vars <- NULL
  a$siteID <- "siteID"
  for (entry in c("cont_analysis", "cat_analysis", "cont_cdftest",
      "attrisk_analysis", "relrisk_analysis", "diffrisk_analysis",
      "change_analysis", "trend_analysis")) {
    b <- a
    if (entry %in% c("cont_analysis", "cat_analysis", "cont_cdftest")) {
      b$vars <- if (entry == "cat_analysis") "category" else "y"
      if (entry == "cont_analysis") b$statistics <- c("Mean", "Total")
    } else if (grepl("risk", entry)) {
      b$vars_response <- "category"
      b$vars_stressor <- "stressor"
    } else {
      b$vars_cont <- "y"
      b$vars_cat <- "category"
      b$dframe <- do.call(rbind, lapply(1:3, function(year) {
        out <- d
        out$year <- year
        out$y <- d$y + year / 10 + sin(d$siteID * year)
        out$category <- factor(ifelse(out$y > 2, "Good", "Poor"))
        out$siteID <- d$siteID + 1000 * year
        out
      }))
      if (entry == "change_analysis") {
        b$dframe <- subset(b$dframe, year < 3)
        b$surveyID <- "year"
        b$test <- c("mean", "total", "median")
      } else b$model_cont <- "SLR"
    }
    default <- suppressMessages(do.call(entry, b))
    b$subset_local <- FALSE
    expect_identical(suppressMessages(do.call(entry, b)), default, info = entry)
    b$subset_local <- NULL
    expect_identical(suppressMessages(do.call(entry, b)), default, info = entry)
    b$subset_local <- TRUE
    if (entry != "cont_cdftest") {
      legacy <- suppressMessages(do.call(entry, b))
      expect_identical(class(default), class(legacy))
    }
  }
})

test_that("cluster GREG derived statistics propagate joint covariance", {
  d <- cluster_fixture()
  d$response <- factor(rep(c("Poor", "Poor", "Good", "Good"), 40))
  d$stressor <- factor(rep(c("Poor", "Good"), 80))
  a <- cluster_args(d)
  a$subpops <- NULL
  a$statistics <- a$vars <- NULL
  a$formula <- ~x
  base <- survey::svydesign(~cluster + siteID, weights = ~weight1 + weight, data = d)
  a$popsize <- colSums(model.matrix(~x, d) * weights(base))
  cal <- survey::calibrate(base, ~x, a$popsize)
  q <- cbind(t11 = d$response == "Poor" & d$stressor == "Poor",
    t12 = d$response == "Poor" & d$stressor == "Good",
    t21 = d$response == "Good" & d$stressor == "Poor",
    t22 = d$response == "Good" & d$stressor == "Good") * 1
  expr <- list(relrisk = quote(log(t11 / (t11 + t21)) - log(t12 / (t12 + t22))),
    diffrisk = quote(t11 / (t11 + t21) - t12 / (t12 + t22)),
    attrisk = quote(log((t11 + t12 + t21 + t22) * t12 / ((t11 + t12) * (t12 + t22)))))
  for (type in c("SRS", "Local")) {
    a$vartype <- type
    stat <- survey::svytotal(q, cal)
    if (type == "Local") attr(stat, "var") <- cluster_reference(d,
      residuals(lm(q ~ x, d, weights = weights(base))) * weights(cal))
    for (kind in names(expr)) {
      b <- c(a, list(vars_response = "response", vars_stressor = "stressor"))
      actual <- do.call(paste0(kind, "_analysis"), b)
      ref <- survey::svycontrast(stat, expr[[kind]])
      column <- if (kind == "diffrisk") "StdError" else "StdError_log"
      expect_equal(actual[[column]], as.numeric(survey::SE(ref)))
    }
    b <- c(a, list(vars = "y", statistics = "Pct", pctval = 50))
    actual <- do.call(cont_analysis, b)$Pct
    if (type == "SRS") {
      ref <- survey::svyquantile(~y, cal, quantiles = .5, interval.type = "mean", qrule = "math", df = Inf)
      expect_equal(actual$StdError, as.numeric(survey::SE(ref)))
    } else {
      indicator <- as.numeric(d$y <= actual$Estimate)
      mu <- weighted.mean(indicator, weights(cal))
      z <- weights(cal) * residuals(lm(I(indicator - mu) ~ x, d, weights = weights(base))) / sum(weights(cal))
      se <- sqrt(as.numeric(cluster_reference(d, z)))
      ordered <- order(d$y)
      cumulative <- cumsum(weights(cal)[ordered]) / sum(weights(cal))
      limits <- vapply(mu + c(-1, 1) * qnorm(.975) * se,
        function(p) d$y[ordered[which(cumulative >= p)[1]]], numeric(1))
      expect_equal(c(actual$LCB95Pct, actual$UCB95Pct), limits)
    }
  }
})

test_that("a nonresponding PSU is distinguished from a zero-domain PSU", {
  d <- cluster_fixture()
  base <- survey::svydesign(~cluster + siteID, weights = ~weight1 + weight, data = d)
  member <- d$region == "A"
  responded <- d$cluster != 16
  result <- localmean_cluster_domain(base, d$y, member, responded, TRUE,
    cluster_names, NULL, c("region", "A", "y"))
  expect_match(result$warn_df$warning, "no usable responses")
  ref <- survey::svyratio(member * d$y, member, base)
  expect_equal(result$covariance, diag(vcov(ref)))
})

test_that("two-stage GREG CDF comparisons agree with survey transformations", {
  d <- cluster_fixture()
  a <- cluster_args(d, "SRS")
  a$statistics <- NULL
  a$formula <- ~x
  base <- survey::svydesign(~cluster + siteID, weights = ~weight1 + weight, data = d)
  a$popsize <- colSums(model.matrix(~x, d) * weights(base))
  cal <- survey::calibrate(base, ~x, a$popsize)
  bounds <- sort(d$y)[floor(seq(nrow(d) / 3, nrow(d), length.out = 3))]
  cal$variables$bin <- cut(d$y, c(-Inf, bounds))
  tests <- c(Wald = "Wald", adjWald = "adjWald", RaoScott_First = "Chisq", RaoScott_Second = "F")
  for (name in names(tests)) {
    a$testname <- name
    actual <- do.call(cont_cdftest, a)
    ref <- survey::svychisq(~region + bin, cal, statistic = tests[[name]])
    expect_equal(actual$p_Value, as.numeric(ref$p.value))
    a$vartype <- "Local"
    expect_true(all(is.finite(do.call(cont_cdftest, a)$p_Value)))
    a$vartype <- "SRS"
  }
})

test_that("the exact two-stage target satisfies total-variance decomposition", {
  # Enumerate SRSWOR: two of three PSUs, two of three elements in each PSU.
  population <- list(c(1, 2, 7), c(2, 8, 10), c(4, 5, 12))
  first <- combn(3, 2, simplify = FALSE)
  second <- combn(3, 2, simplify = FALSE)
  draws <- unlist(lapply(first, function(psu) unlist(lapply(second, function(a)
    vapply(second, function(b) 1.5 * (1.5 * sum(population[[psu[1]]][a]) +
      1.5 * sum(population[[psu[2]]][b])), numeric(1))))))
  truth <- sum(unlist(population))
  between <- 3^2 * (1 - 2/3) * var(vapply(population, sum, numeric(1))) / 2
  within <- sum(vapply(population, function(y) 1.5 * 3^2 * (1 - 2/3) * var(y) / 2, numeric(1)))
  expect_equal(mean(draws), truth)
  expect_equal(mean((draws - truth)^2), between + within)
})

test_that("apiclus2 calibrated estimates and variances follow survey", {
  data("api", package = "survey", envir = environment())
  d <- apiclus2
  d$weight1 <- d$fpc1 / length(unique(d$dnum))
  d$weight <- d$fpc2 / ave(d$snum, d$dnum, FUN = length)
  a <- list(dframe = d, vars = "api00", clusterID = "dnum", siteID = "snum",
    weight1 = "weight1", weight = "weight", formula = ~api99,
    popsize = c("(Intercept)" = nrow(apipop), api99 = sum(apipop$api99)),
    vartype = "SRS", statistics = c("Mean", "Total"))
  base <- survey::svydesign(~dnum + snum, weights = ~weight1 + weight, data = d)
  cal <- survey::calibrate(base, ~api99, a$popsize)
  actual <- do.call(cont_analysis, a)
  expect_equal(actual$Mean$Estimate, as.numeric(coef(survey::svymean(~api00, cal))))
  expect_equal(actual$Total$StdError, as.numeric(survey::SE(survey::svytotal(~api00, cal))))
})

test_that("unsupported multistage PPS approximations retain survey's boundary", {
  d <- cluster_fixture()
  for (type in c("HT", "YG")) {
    expect_error(survey::svydesign(~cluster + siteID,
      probs = ~I(1 / weight1) + I(1 / weight), pps = "overton",
      variance = type, data = d), "Multistage PPS")
    a <- cluster_args(d, type)
    a$formula <- ~x
    a$popsize <- c("(Intercept)" = 2000, x = 20)
    expect_error(suppressMessages(do.call(cont_analysis, a)), "preceding")
    expect_true(any(grepl("two-stage", get("error_vec", envir = .GlobalEnv))))
  }
})

test_that("supported two-stage HT Brewer GREG inference follows survey", {
  d <- cluster_fixture()
  a <- cluster_args(d, "HT")
  a$jointprob <- "brewer"
  a$formula <- ~x
  base <- survey::svydesign(~cluster + siteID,
    probs = ~I(1 / weight1) + I(1 / weight), pps = "brewer", variance = "HT", data = d)
  a$popsize <- c("(Intercept)" = sum(weights(base)), x = 20)
  cal <- survey::calibrate(base, ~x, a$popsize)
  actual <- do.call(cont_analysis, a)
  for (i in 1:2) {
    sub <- cal[d$region == levels(d$region)[i], ]
    for (stat in c("Mean", "Total")) {
      ref <- if (stat == "Mean") survey::svymean(~y, sub) else survey::svytotal(~y, sub)
      expect_equal(actual[[stat]]$Estimate[i], as.numeric(coef(ref)))
      expect_equal(actual[[stat]]$StdError[i], as.numeric(survey::SE(ref)))
    }
  }
})

test_that("cluster GREG covariance combines different domain calibration contexts", {
  d <- cluster_fixture(TRUE)
  base <- survey::svydesign(~cluster + siteID, strata = ~stratum,
    weights = ~weight1 + weight, data = d, nest = TRUE)
  x <- model.matrix(~x, d)
  mask <- d$region == "A"
  known <- greg_calibrate(base, x * mask, colSums(x * mask * weights(base)), "known")
  unknown <- greg_calibrate(base, x, colSums(x * weights(base)), "population")
  q1 <- cbind(first = d$y * mask, second = 0)
  q2 <- cbind(first = 0, second = d$y * !mask)
  actual <- greg_joint_total(list(known, unknown), list(q1, q2), "Local", cluster_names)
  z <- lm.wfit(x * mask, q1, weights(base))$residuals * weights(known$design) +
    lm.wfit(x, q2, weights(base))$residuals * weights(unknown$design)
  expect_equal(unname(vcov(actual$stat)), cluster_reference(d, z), ignore_attr = TRUE)
  expect_true(abs(vcov(actual$stat)[1, 2]) > 1e-8)
})

test_that("All Sites and direct domains share compatible stage neighborhoods", {
  d <- cluster_fixture()
  a <- cluster_args(d)
  a$All_Sites <- TRUE
  original <- localmean_weight
  calls <- 0L
  local_mocked_bindings(localmean_weight = function(...) { calls <<- calls + 1L; original(...) })
  do.call(cont_analysis, a)
  expect_equal(calls, 17L)
})

test_that("singleton and small-PSU conventions and cache invalidation are explicit", {
  d <- cluster_fixture()
  keep <- unlist(lapply(split(seq_len(nrow(d)), d$cluster), function(i)
    head(i, c(1, 2, 3, 8)[(d$cluster[i[1]] - 1) %% 4 + 1])))
  d <- d[keep, ]
  q <- cbind(d$y, d$x) * d$weight
  context <- localmean_context()
  result <- localmean_cluster_covariance(q, d$cluster, d$weight1, d$weight,
    d$xc, d$yc, d$x1, d$y1, local_context = context)
  expect_length(result$notes, 8)
  expect_true(all(is.finite(result$covariance)))
  original <- localmean_weight
  count <- 0L
  local_mocked_bindings(localmean_weight = function(...) { count <<- count + 1L; original(...) })
  d$weight[tail(which(d$cluster == 4), 1)] <- 10
  changed <- localmean_cluster_covariance(q, d$cluster, d$weight1, d$weight,
    d$xc, d$yc, d$x1, d$y1, local_context = context)
  expect_equal(count, 1L)
  fresh <- localmean_cluster_covariance(q, d$cluster, d$weight1, d$weight,
    d$xc, d$yc, d$x1, d$y1)
  expect_identical(changed$covariance, fresh$covariance)
  i <- rev(seq_len(nrow(d)))
  reordered <- localmean_cluster_covariance(q[i, ], d$cluster[i], d$weight1[i], d$weight[i],
    d$xc[i], d$yc[i], d$x1[i], d$y1[i], local_context = context)
  ref <- localmean_cluster_covariance(q[i, ], d$cluster[i], d$weight1[i], d$weight[i],
    d$xc[i], d$yc[i], d$x1[i], d$y1[i])
  expect_identical(reordered$covariance, ref$covariance)
})

test_that("CDF domain fallbacks identify both groups and the response", {
  d <- cluster_fixture()
  d <- d[unlist(lapply(split(seq_len(nrow(d)), d$cluster), head, 3)), ]
  d$rowvar <- factor(ifelse(d$cluster <= 5, "A", ifelse(d$cluster <= 10, "B", NA)),
    levels = c("A", "B"))
  d$colvar <- factor(rep(c("low", "high"), length.out = nrow(d)))
  base <- survey::svydesign(~cluster + siteID, weights = ~weight1 + weight, data = d)
  actual <- localmean_cluster_cdftest(base, cluster_names, TRUE, NULL,
    c("region", "A", "B", "y"), 15, NULL)
  expect_equal(actual$warn_df$indicator, "y")
  expect_equal(actual$warn_df$subpop, "A / B")
  expect_true(all(is.finite(actual$varest)))
})

test_that("the CDF adapter agrees with working exhaustive legacy tables", {
  set.seed(5)
  d <- expand.grid(rowvar = factor(c("A", "B")), colvar = factor(c("L", "H")),
    repeat_id = 1:2, cluster = 1:8)
  d$siteID <- seq_len(nrow(d))
  d$weight <- runif(nrow(d), 2, 4)
  d$weight1 <- rep(seq(2, 4, length.out = 8), each = 8)
  d$xc <- runif(nrow(d)); d$yc <- runif(nrow(d))
  d$x1 <- rep(runif(8), each = 8); d$y1 <- rep(runif(8), each = 8)
  base <- survey_design(d, "siteID", "weight", FALSE, NULL, TRUE, "cluster",
    "weight1", FALSE, NULL, NULL, FALSE, NULL, NULL, NULL, "Local", NULL)
  names <- c(cluster_names, list(weight = "weight", weight1 = "weight1", stratumID = NULL))
  for (fun in list(cdftest_localmean_prop, cdftest_localmean_total)) {
    old <- fun(base, names, FALSE, NULL, c("group", "A", "B", "response"), TRUE)
    new <- fun(base, names, FALSE, NULL, c("group", "A", "B", "response"), FALSE)
    expect_equal(new$varest, old$varest, tolerance = 1e-10)
  }
})

test_that("public missing-PSU handling distinguishes nested labels and GREG", {
  d <- cluster_fixture(TRUE)
  d$cluster <- (d$cluster - 1) %% 8 + 1
  d$y[d$stratum == "H1" & d$cluster == 1] <- NA_real_
  a <- cluster_args(d)
  a$stratumID <- "stratum"
  a$statistics <- "Mean"
  suppressMessages(do.call(cont_analysis, a))
  expect_true(any(grepl("no usable responses", get("warn_df", envir = .GlobalEnv)$warning)))

  # Existing GREG semantics: the target is coded by domain AND response.
  # All sampled rows stay in calibration and in residual neighborhoods.
  # There is no additional missing-PSU fallback on this path.
  a$formula <- ~x
  a$popsize <- colSums(model.matrix(~x, d) * d$weight * d$weight1)
  actual <- suppressMessages(do.call(cont_analysis, a))$Mean
  base <- survey::svydesign(~cluster + siteID, strata = ~stratum,
    weights = ~weight1 + weight, data = d, nest = TRUE)
  cal <- survey::calibrate(base, ~x, a$popsize)
  member <- as.numeric(d$region == "A" & !is.na(d$y))
  q <- d$y; q[member == 0] <- 0
  ref <- survey::svyratio(q, member, cal)
  u <- q - member * as.numeric(coef(ref))
  z <- residuals(lm(u ~ x, d, weights = weights(base))) * weights(cal) /
    sum(weights(cal) * member)
  expect_equal(actual$Estimate[1], as.numeric(coef(ref)))
  expect_equal(actual$StdError[1]^2, as.numeric(cluster_reference(d, z)), tolerance = 1e-10)
  expect_true(any(abs(z[is.na(d$y)]) > 1e-8))
})

cluster_occasion_args <- function(d, stratified = FALSE, formula = ~x) {
  list(dframe = d, subpops = "region", siteID = "siteID", weight = "weight",
    xcoord = "xc", ycoord = "yc", stratumID = if (stratified) "stratum" else NULL,
    clusterID = "cluster", weight1 = "weight1", xcoord1 = "x1", ycoord1 = "y1",
    sizeweight = FALSE, sweight = NULL, sweight1 = NULL, fpc = NULL,
    formula = formula, popsize = colSums(model.matrix(formula, d) * d$weight * d$weight1),
    subpopsize = NULL, vartype = "Local", jointprob = "overton", conf = 95,
    All_Sites = FALSE, revisitwgt = TRUE, local_nbh = 15)
}

test_that("GREG occasions retain two-stage FPC and size weights", {
  for (stratified in c(FALSE, TRUE)) for (fpc in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    d$sw1 <- rep(seq(1, 2, length.out = 16), each = 10)
    d$sw2 <- seq(1, 1.2, length.out = nrow(d))
    a <- cluster_occasion_args(d, stratified)
    a$sizeweight <- TRUE; a$sweight <- "sw2"; a$sweight1 <- "sw1"
    a$vartype <- "SRS"
    d$Ncluster <- 100; d$stage1size <- 100
    make_fpc <- function(ids) c(Ncluster = 100, setNames(rep(100, length(ids)), ids))
    if (fpc) a$fpc <- if (stratified) list(H1 = make_fpc(1:8), H2 = make_fpc(9:16)) else make_fpc(1:16)
    one <- greg_build_occasion(a, "category", "y")
    ref <- survey::svydesign(~cluster + siteID, strata = if (stratified) ~stratum else NULL,
      weights = ~I(weight1 * sw1) + I(weight * sw2), nest = TRUE,
      fpc = if (fpc) ~Ncluster + stage1size else NULL, data = d)
    expect_equal(ncol(one$base$cluster), 2L)
    expect_equal(as.numeric(weights(one$base)), as.numeric(weights(ref)))
    expect_equal(one$base$fpc, ref$fpc)
    ref <- survey::calibrate(ref, ~x, a$popsize)
    ans <- greg_time_estimate(list(one), list(d$y), "region", "A", TRUE, 95)
    expected <- survey::svymean(~y, ref[d$region == "A", ])
    expect_equal(as.numeric(ans$stat), as.numeric(coef(expected)))
    expect_equal(as.numeric(vcov(ans$stat)), as.numeric(vcov(expected)))
  }
})

test_that("two-stage repeat residual covariance matches an independent local reference", {
  for (stratified in c(FALSE, TRUE)) for (equal in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    a <- cluster_occasion_args(d, stratified)
    a$revisitwgt <- equal
    one <- greg_build_occasion(a, NULL, "y")
    a$dframe <- d[d$siteID %% 4 != 0, ]
    a$dframe$y <- a$dframe$y + .3 * a$dframe$x
    if (!equal) a$dframe$weight <- a$dframe$weight * 1.2
    a$popsize <- colSums(model.matrix(~x, a$dframe) * a$dframe$weight * a$dframe$weight1)
    two <- greg_build_occasion(a, NULL, "y")
    z <- lapply(list(one, two), function(o) {
      q <- o$data$y * (o$data$region == "A")
      totals <- colSums(model.matrix(~x, o$data) * o$data$weight1 * o$data$weight)
      cal <- survey::calibrate(o$base, ~x, totals)
      residuals(lm(q ~ x, o$data, weights = weights(o$base))) * weights(cal)
    })
    ans <- greg_revisit_covariance(one, two, z[[1]], z[[2]], one$greg$contexts$region$A, NULL)
    i <- match(two$ids, one$ids)
    first <- one$data[i, ]; second <- two$data
    v <- cbind(z[[1]][i] / (first$weight1 * first$weight),
      z[[2]] / (second$weight1 * second$weight))
    expected <- 0
    for (h in unique(first$stratum)) {
      rows <- which(first$stratum == h)
      p <- first[rows, ]; q <- second[rows, ]
      values <- v[rows, , drop = FALSE]
      values[, 1] <- values[, 1] - weighted.mean(values[, 1], p$weight1 * p$weight)
      values[, 2] <- values[, 2] - weighted.mean(values[, 2], q$weight1 * q$weight)
      if (equal) {
        expected <- expected + cluster_reference(p, values * p$weight1 * p$weight)[1, 2]
      } else {
        unit <- p; unit$weight1 <- unit$weight <- 1
        cov <- cluster_reference(unit, values)
        rho <- cov[1, 2] / sqrt(prod(diag(cov)))
        expected <- expected + rho * sqrt(
          cluster_reference(p, values[, 1] * p$weight1 * p$weight)[1, 1] *
          cluster_reference(q, values[, 2] * q$weight1 * q$weight)[1, 1])
      }
    }
    expect_equal(ans$covariance, expected, tolerance = 1e-10)
    panel <- rbind(transform(one$data, occasion = "one"), transform(two$data, occasion = "two"))
    args <- cluster_occasion_args(d, stratified)
    args$dframe <- panel; args$surveyID <- "occasion"; args$vars_cont <- "y"
    args$test <- "total"; args$revisitwgt <- equal
    args$popsize <- list(one = args$popsize, two = a$popsize)
    actual <- do.call(change_analysis, args)$contsum_total
    variance <- cluster_reference(one$data, z[[1]])[1, 1] +
      cluster_reference(two$data, z[[2]])[1, 1] - 2 * expected
    expect_gt(variance, 0)
    expect_equal(actual$StdError[actual$Subpopulation == "A"]^2, variance, tolerance = 1e-10)
  }
})

test_that("the same-design shortcut checks the entire two-stage design", {
  d <- cluster_fixture(TRUE)
  d$sw1 <- 1
  a <- cluster_occasion_args(d, TRUE)
  a$sweight1 <- "sw1"
  one <- greg_build_occasion(a, NULL, "y")
  z <- greg_residual_contributions(one$greg$contexts$region$A, d$y)
  # A recognizable return makes dispatch observable without testing internal
  # arithmetic twice. A genuine same design must bypass this mock.
  local_mocked_bindings(greg_cluster_revisit = function(...) list(covariance = -123, warn_df = NULL))
  check <- function(second) greg_revisit_covariance(one, second, z, z,
    one$greg$contexts$region$A, NULL)$covariance
  expect_false(identical(check(one), -123))
  second <- one
  second$base$variables$wgt1 <- second$base$variables$wgt1 * 2
  second$base$variables$wgt2 <- second$base$variables$wgt2 / 2
  expect_identical(check(second), -123)
  second <- one; second$base$cluster[[1]] <- rev(second$base$cluster[[1]])
  expect_identical(check(second), -123)
  second <- one; second$base$strata[[1]] <- rev(second$base$strata[[1]])
  expect_identical(check(second), -123)
  for (field in c("x1", "y1", "sw1")) {
    second <- one; second$data[[field]] <- second$data[[field]] + 1
    expect_identical(check(second), -123)
  }
  second <- one; second$base$fpc$popsize <- second$base$fpc$popsize + 1
  # Infinite population sizes remain Inf, so use a finite replacement here.
  second$base$fpc$popsize[] <- 100
  expect_identical(check(second), -123)
})

test_that("unstratified GREG change matches direct ratios with matching centering", {
  d <- cluster_fixture()
  for (partial in c(FALSE, TRUE)) for (equal in c(FALSE, TRUE)) {
    second <- transform(d, y = .8 * y + sin(x))
    second$category <- factor(ifelse(second$y > 2, "Good", "Poor"))
    if (partial) second$siteID[second$siteID %% 4 == 0] <- second$siteID[second$siteID %% 4 == 0] + 1000
    if (!equal) second$weight <- second$weight * (1 + second$xc / 5)
    a <- cluster_occasion_args(d, formula = ~1)
    a$subpops <- NULL
    a$dframe <- rbind(transform(d, year = "one"), transform(second, year = "two"))
    a$revisitwgt <- equal
    a$popsize <- list(one = a$popsize, two = c("(Intercept)" = sum(second$weight * second$weight1)))
    a$surveyID <- "year"; a$vars_cont <- "y"; a$vars_cat <- "category"
    a$test <- c("mean", "total", "median")
    actual <- suppressMessages(do.call(change_analysis, a))
    a$formula <- a$popsize <- NULL
    direct <- suppressMessages(do.call(change_analysis, a))
    for (field in c("DiffEst", "StdError", "Estimate_1", "StdError_1", "Estimate_2", "StdError_2")) {
      expect_equal(actual$contsum_mean[[field]], direct$contsum_mean[[field]], tolerance = 1e-10)
    }
    for (field in c("DiffEst.P", "StdError.P", "Estimate.P_1", "StdError.P_1", "Estimate.P_2", "StdError.P_2")) {
      expect_equal(actual$catsum[[field]], direct$catsum[[field]], tolerance = 1e-10)
    }
    expect_true(all(is.finite(actual$contsum_total$StdError)))
    expect_true(all(is.finite(actual$contsum_median$StdError.P)))
  }
})

test_that("two-stage GREG trends match direct annual ratio centering", {
  for (stratified in c(FALSE, TRUE)) for (model in c("SLR", "WLR")) {
    d <- cluster_fixture(stratified)
    d$stratum <- factor(d$stratum)
    formula <- if (stratified) ~0 + stratum else ~1
    a <- cluster_occasion_args(d, stratified, formula)
    a$subpops <- NULL
    a$dframe <- do.call(rbind, lapply(1:4, function(year)
      transform(d, year = year, y = y + .2 * year + sin(x * year) / 3)))
    a$popsize <- setNames(rep(list(a$popsize), 4), as.character(1:4))
    a$revisitwgt <- NULL
    a$yearID <- "year"; a$vars_cont <- "y"; a$model_cont <- model
    actual <- do.call(trend_analysis, a)
    a$formula <- a$popsize <- NULL
    direct <- do.call(trend_analysis, a)
    rownames(actual$contsum) <- rownames(direct$contsum) <- NULL
    expect_equal(actual$contsum, direct$contsum, tolerance = 1e-10)
  }
})

test_that("two-stage GREG categorical SRS inference follows survey", {
  for (stratified in c(FALSE, TRUE)) {
    d <- cluster_fixture(stratified)
    a <- cluster_args(d, "SRS")
    a$statistics <- NULL; a$vars <- "category"
    a$formula <- ~x
    if (stratified) a$stratumID <- "stratum"
    a$popsize <- colSums(model.matrix(~x, d) * d$weight1 * d$weight)
    actual <- do.call(cat_analysis, a)
    base <- survey::svydesign(~cluster + siteID, strata = if (stratified) ~stratum else NULL,
      weights = ~weight1 + weight, data = d, nest = TRUE)
    cal <- survey::calibrate(base, ~x, a$popsize)
    for (domain in levels(d$region)) for (category in levels(d$category)) {
      v <- as.numeric(d$region == domain)
      q <- v * (d$category == category)
      p <- survey::svyratio(q, v, cal); u <- survey::svytotal(q, cal)
      row <- actual[actual$Subpopulation == domain & actual$Category == category, ]
      expect_equal(row$Estimate.P / 100, as.numeric(coef(p)))
      expect_equal(row$StdError.P / 100, as.numeric(survey::SE(p)))
      expect_equal(row$Estimate.U, as.numeric(coef(u)))
      expect_equal(row$StdError.U, as.numeric(survey::SE(u)))
    }
  }
})

test_that("PSUs with fewer than four repeat sites use the SRS or zero fallback with a warning", {
  d <- cluster_fixture()
  a <- cluster_occasion_args(d)
  one <- greg_build_occasion(a, NULL, "y")
  # Revisit one, two or three sites in each PSU, preserving the full
  # occasion design by giving the other sites new IDs.
  for (n in 1:3) {
    a$dframe <- d
    new <- ave(d$siteID, d$cluster, FUN = seq_along) > n
    a$dframe$siteID[new] <- a$dframe$siteID[new] + 1000
    two <- greg_build_occasion(a, NULL, "y")
    z <- greg_residual_contributions(one$greg$contexts$region$A, d$y)
    ans <- greg_revisit_covariance(one, two, z, z, one$greg$contexts$region$A, NULL)
    # if one site then use zero for within-PSU covariance; if 2-3 sites then use SRS within-PSU covariance.
    expect_true(is.finite(ans$covariance))
    expect_true(any(grepl("less than four", ans$warn_df$warning)))
  }
})

test_that("two-stage GREG change handles NLA_IN responses and partial revisits", {
  data <- new.env()
  load(system.file("extdata", "NLA_IN.rda", package = "spsurvey"), envir = data)
  d <- subset(data$NLA_IN, YEAR %in% c(2007, 2012))
  totals <- lapply(split(d, d$YEAR, drop = TRUE), function(x)
    c("(Intercept)" = sum(x$WGT_TP * x$weight1)))
  actual <- change_analysis(d, vars_cont = "ContVar", subpops = "LAKE_ORGN",
    surveyID = "YEAR", siteID = "SITE_ID", weight = "WGT_TP",
    xcoord = "XCOORD", ycoord = "YCOORD", clusterID = "clusterID",
    weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
    formula = ~1, popsize = totals)
  expect_equal(nrow(actual$contsum_mean), 2L)
  expect_true(all(is.finite(actual$contsum_mean$StdError)))
})
