test_that("risk response rows follow singleton removal", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  set.seed(5)
  n <- 49L
  d <- data.frame(id = seq_len(n), weight = 2, x = runif(n), y = runif(n),
    stratum = factor(c("singleton", rep(c("A", "B"), each = 24))),
    response = factor(c("Rare", rep(c("Good", "Poor"), 24)),
      levels = c("Good", "Poor", "Rare")),
    stressor = factor(sample(rep(c("Good", "Poor"), length.out = n))),
    domain = factor(rep(c("East", "West"), length.out = n)))
  for (fun in list(relrisk_analysis, attrisk_analysis, diffrisk_analysis)) {
    for (vartype in c("Local", "SRS")) {
      args <- list(dframe = d, vars_response = "response", vars_stressor = "stressor",
        siteID = "id", weight = "weight", xcoord = "x", ycoord = "y",
        stratumID = "stratum", subpops = "domain", vartype = vartype)
      assign("warn_df", NULL, globalenv())
      actual <- suppressWarnings(suppressMessages(do.call(fun, args)))
      actual_warnings <- get("warn_df", globalenv())
      args$dframe <- d[-1, ]
      assign("warn_df", NULL, globalenv())
      expected <- suppressWarnings(suppressMessages(do.call(fun, args)))
      expected_warnings <- get("warn_df", globalenv())
      expect_equal(actual, expected)
      actual_warnings <- actual_warnings[
        !grepl("singleton", actual_warnings$warning, fixed = TRUE), , drop = FALSE]
      rownames(actual_warnings) <- NULL
      if (!nrow(actual_warnings)) actual_warnings <- NULL
      if (!is.null(expected_warnings)) rownames(expected_warnings) <- NULL
      expect_equal(actual_warnings, expected_warnings)
    }
  }
})

test_that("GREG risk returns a data frame when all domains are empty", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  d <- declared_category_fixture()
  d$domain[] <- NA
  for (fun in list(relrisk_analysis, attrisk_analysis, diffrisk_analysis)) {
    args <- list(dframe = d, vars_response = "response", vars_stressor = "stressor",
      subpops = "domain", vartype = "SRS", formula = ~1,
      popsize = c("(Intercept)" = sum(d$weight)))
    empty <- suppressMessages(do.call(fun, args))
    args$dframe$domain <- factor("joined")
    full <- suppressMessages(do.call(fun, args))
    expect_s3_class(empty, "data.frame")
    expect_equal(nrow(empty), 0L)
    expect_identical(names(empty), names(full))
  }
})

test_that("categorical trend keeps a removed stratum as a declared level", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  d <- data.frame(id = rep(1:12, 3), year = rep(1:3, each = 12), weight = 2,
    stratum = factor(rep(rep(c("A", "B"), each = 6), 3), levels = c("A", "B", "C")))
  d$stratum[1] <- "C"
  assign("warn_df", NULL, globalenv())
  result <- suppressWarnings(suppressMessages(trend_analysis(d, vars_cat = "stratum",
    stratumID = "stratum", yearID = "year", siteID = "id", vartype = "SRS")))
  expect_identical(result$catsum$Category, c("A", "B", "C"))
  expect_equal(result$catsum$Trend_Estimate[3], 0)
  expect_equal(result$catsum$Trend_Std_Error[3], 0)
  expect_true(any(grepl("removed", get("warn_df", globalenv())$warning)))
})

test_that("aliased slopes keep the estimable intercept", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  year <- 1:3
  fit <- lm(c(1, 2, 3) ~ year, weights = c(1e28, 1, 1))
  expect_true(is.na(coef(fit)[2]))
  fields <- trend_regression_fields(fit, 95)
  expect_true(all(is.na(fields[1:5])))
  expect_equal(unname(fields[6]), unname(coef(fit)[1]))
})

test_that("single-category overlap suppresses only synthetic warnings", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  d <- data.frame(id = rep(1:24, 2), year = rep(1:2, each = 24),
    weight = 2, response = factor("Good"))
  assign("warn_df", NULL, globalenv())
  result <- suppressWarnings(suppressMessages(change_analysis(d, vars_cat = "response",
    subpops = "response", surveyID = "year", siteID = "id", vartype = "SRS")))
  warnings <- get("warn_df", globalenv())
  expect_identical(result$catsum$Category, "Good")
  expect_false(any(grepl(".spsurvey_padding", warnings$warning, fixed = TRUE)))
  expect_true(any(grepl('category level "Good"', warnings$warning, fixed = TRUE)))
  expect_true(all(warnings$indicator == "response"))
})

test_that("overlapping categorical response uses its public name in warnings", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  set.seed(5)
  n <- 30L
  d <- data.frame(id = rep(seq_len(n), 3), year = rep(1:3, each = n),
    weight = 2, x = rep(runif(n), 3), y = rep(runif(n), 3),
    response = factor(rep(c("Good", "Poor"), length.out = n * 3),
      levels = c("Poor", "Good", "Absent")))
  d$response[d$year == 1] <- "Good"
  assign("warn_df", NULL, globalenv())
  suppressWarnings(suppressMessages(change_analysis(subset(d, year < 3),
    vars_cat = "response", subpops = "response", surveyID = "year",
    siteID = "id", vartype = "SRS")))
  warnings <- get("warn_df", globalenv())
  expect_true(any(grepl("for indicator response", warnings$warning, fixed = TRUE)))
  expect_false(any(grepl(".spsurvey_category", warnings$warning, fixed = TRUE)))
  d$domain <- factor(rep(c("East", "West"), length.out = n * 3))
  d$response[d$year == 1 & d$domain == "East"] <- NA
  assign("warn_df", NULL, globalenv())
  suppressWarnings(suppressMessages(trend_analysis(d, vars_cat = "response",
    subpops = "domain", yearID = "year", siteID = "id", vartype = "SRS",
    cat_rhs = "response + Wyear + (1 | id)")))
  warnings <- get("warn_df", globalenv())
  expect_true(any(grepl('"response" contains no data', warnings$warning, fixed = TRUE)))
  expect_false(any(grepl(".spsurvey_category", warnings$warning, fixed = TRUE)))
})

test_that("two-stage GREG WLR preserves other fits when a slope is aliased", {
  skip_if_not(identical(Sys.getenv("SPSURVEY_RUN_EXTRAS"), "true"))
  set.seed(782)
  n <- 72L
  i <- seq_len(n)
  d <- data.frame(id = paste0("s", i), w = 2, w1 = 3,
    x = i %% 12L + runif(n), y = i %/% 12L + runif(n),
    x1 = rep(1:12, each = 6L), y1 = rep((1:12)^2 %% 11, each = 6L),
    psu = factor(rep(paste0("P", 1:12), each = 6L),
      levels = c(paste0("P", 12:1), "EmptyPSU")),
    h = factor(rep(c("North", "South"), each = n / 2),
      levels = c("South", "Lost", "North")),
    area = factor(rep(c("Upper", "Lower", "Lower"), length.out = n),
      levels = c("Upper", "Unoccupied", "Lower")),
    status = factor(sample(rep(c("Gold", "Slate"), each = n / 2)),
      levels = c("Slate", "Gold")),
    risk = factor(sample(rep(c("Poor", "Good"), each = n / 2))),
    exposure = factor(sample(rep(c("Poor", "Good"), each = n / 2))),
    value = rnorm(n))
  panel <- do.call(rbind, lapply(2011:2014, function(year) {
    a <- d
    a$year <- year
    a$status <- factor(if (year == 2011) rep("Gold", n) else if (year == 2012)
      ifelse(i %% 3 == 1, "Slate", "Gold") else sample(as.character(d$status)),
      levels = c("Slate", "Rust", "Gold"))
    a$flag <- i <= (12L + 3L * (year - 2010L))
    a$label <- as.character(a$status)
    a$value <- d$value + runif(n, max = .45) + .1 * a$year
    a
  }))
  controls <- setNames(rep(list(c("(Intercept)" = (sum(d$w) + 3) * 3)), 4), 2011:2014)
  args <- list(dframe = panel, vars_cat = c("status", "flag", "label"),
    vars_cont = "value", subpops = "area", stratumID = "h", vartype = "SRS",
    formula = ~1, popsize = controls, yearID = "year", model_cat = "WLR",
    All_Sites = TRUE, siteID = "id", weight = "w", xcoord = "x", ycoord = "y",
    clusterID = "psu", weight1 = "w1", xcoord1 = "x1", ycoord1 = "y1")
  assign("warn_df", NULL, globalenv())
  both <- suppressWarnings(suppressMessages(do.call(trend_analysis, args)))
  warnings <- get("warn_df", globalenv())
  selected <- both$catsum$Indicator == "status" & both$catsum$Category == "Slate" &
    both$catsum$Subpopulation == "All Sites"
  expect_true(is.na(both$catsum$Trend_Estimate[selected]))
  expect_true(is.finite(both$catsum$Intercept_Estimate[selected]))
  expect_true(any(grepl("could not be estimated", warnings$warning, fixed = TRUE)))
  args$vars_cat <- "flag"
  only <- suppressWarnings(suppressMessages(do.call(trend_analysis, args)))
  flag <- subset(both$catsum, Indicator == "flag")
  rownames(flag) <- rownames(only$catsum) <- NULL
  expect_equal(flag, only$catsum)
  expect_equal(both$contsum, only$contsum, ignore_attr = TRUE)
  args$formula <- args$popsize <- args$vars_cont <- NULL
  args$vars_cat <- c("status", "flag", "label")
  args$dframe$w[args$dframe$year == 2012 & args$dframe$id == "s1"] <- 1e10
  assign("warn_df", NULL, globalenv())
  direct <- suppressWarnings(suppressMessages(do.call(trend_analysis, args)))
  direct_warnings <- get("warn_df", globalenv())
  expect_true(any(grepl("could not be estimated", direct_warnings$warning, fixed = TRUE)))
  expect_true(any(is.na(direct$catsum$Trend_Estimate) &
    is.finite(direct$catsum$Intercept_Estimate)))
})
