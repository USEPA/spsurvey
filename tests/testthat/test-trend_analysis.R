context("trend analysis")

mixed_trend_data <- function() {
  set.seed(5)
  d <- expand.grid(site = factor(seq_len(8)), year = seq_len(16))
  d$Wyear <- d$year - min(d$year)
  d$stratum <- factor(ifelse(as.integer(d$site) <= 4, "A", "B"))
  d$extra <- as.numeric(d$stratum == "B")
  d$aux <- rnorm(nrow(d))
  d$w <- 1
  d$y <- rnorm(16, sd = 1.8)[d$year] + rnorm(8, sd = 3)[d$site] +
    (0.3 + rnorm(8, sd = 0.18)[d$site]) * d$year + rnorm(nrow(d))
  eta <- -0.5 + 0.08 * d$Wyear + d$aux + rnorm(8, sd = 0.5)[d$site]
  d$y_cat <- factor(ifelse(runif(nrow(d)) < plogis(eta), "yes", "no"))
  d
}

run_mixed_trend <- function(d, ..., invprboot = FALSE, model_cont = "LMM") {
  suppressWarnings(suppressMessages(trend_analysis(d,
    siteID = "site", yearID = "year", weight = "w", vartype = "SRS",
    invprboot = invprboot, model_cont = model_cont, ...
  )))
}

expect_mixed_trend <- function(result, fit) {
  reference <- coef(summary(fit))["Wyear", c("Estimate", "Std. Error")]
  expect_equal(as.numeric(result$Trend_Estimate), unname(reference[1L]))
  expect_equal(as.numeric(result$Trend_Std_Error), unname(reference[2L]))
}

test_that("GLMM and LMM use named fixed and random effects", {
  expect_false("package:lme4" %in% search())
  d <- mixed_trend_data()
  for (stratum in list(NULL, "stratum")) {
    cat_rhs <- "0 + aux + Wyear + (1 | site)"
    cont_rhs <- "aux + Wyear + (1 + Wyear | site)"
    both <- run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM", cat_rhs = cat_rhs,
      vars_cont = "y", cont_rhs = cont_rhs, stratumID = stratum)
    cat_only <- run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM",
      cat_rhs = cat_rhs, stratumID = stratum)
    cont_only <- run_mixed_trend(d, vars_cont = "y", cont_rhs = cont_rhs, stratumID = stratum)
    expect_identical(both$catsum, cat_only$catsum)
    expect_identical(both$contsum, cont_only$contsum)
    cat_formula <- reformulate(c(cat_rhs, stratum), "y_cat")
    cont_formula <- reformulate(c(cont_rhs, stratum), "y")
    cat_fit <- suppressWarnings(lme4::glmer(cat_formula, d, family = binomial()))
    cont_fit <- suppressWarnings(lme4::lmer(cont_formula, d))
    expect_mixed_trend(both$catsum, cat_fit)
    expect_mixed_trend(both$contsum, cont_fit)
    expect_equal(as.numeric(both$catsum$aux_Estimate), unname(lme4::fixef(cat_fit)["aux"]))
    expect_equal(as.numeric(both$contsum$aux_Estimate), unname(lme4::fixef(cont_fit)["aux"]))
  }

  both <- run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM", vars_cont = "y")
  fits <- list(
    catsum = suppressWarnings(lme4::glmer(y_cat ~ Wyear + (1 + Wyear | site) + (1 | year),
      d, family = binomial())),
    contsum = suppressWarnings(lme4::lmer(y ~ Wyear + (1 + Wyear | site) + (1 | year), d))
  )
  for (table in names(fits)) {
    expect_mixed_trend(both[[table]], fits[[table]])
    components <- lme4::VarCorr(fits[[table]])
    expect_identical(names(components)[1L], "year")
    expected <- c(components$site["(Intercept)", "(Intercept)"],
      components$site["Wyear", "Wyear"],
      attr(components$site, "correlation")["(Intercept)", "Wyear"],
      components$year["(Intercept)", "(Intercept)"])
    fields <- c("Var_SiteID_Int", "Var_SiteID_Trend", "Corr_SiteID_Int_Trend", "Var_YearID")
    expect_equal(as.numeric(both[[table]][1L, fields]), expected)
  }
})

test_that("mixed trend rows align by name when a nuisance coefficient is dropped", {
  d <- mixed_trend_data()
  d$y_missing <- ifelse(d$extra == 1, NA, d$y)
  rhs <- "Wyear + extra + (1 | site)"
  result <- run_mixed_trend(d, vars_cont = c("y_missing", "y"), cont_rhs = rhs)$contsum
  expect_identical(result$Indicator, c("y_missing", "y"))
  nuisance <- grep("^extra_", names(result), value = TRUE)
  expect_length(nuisance, 5L)
  expect_true(all(is.na(result[1L, nuisance])))
  expect_true(all(is.finite(as.numeric(result[2L, nuisance]))))
  for (i in seq_len(nrow(result))) {
    fit <- suppressWarnings(lme4::lmer(reformulate(rhs, result$Indicator[i]), d))
    expect_mixed_trend(result[i, ], fit)
  }
})

test_that("mixed trend input errors distinguish unsupported options and missing slopes", {
  d <- mixed_trend_data()
  d$duplicate <- d$Wyear
  d$missing <- "whole"
  d$missing[1L] <- NA
  for (categorical in c(TRUE, FALSE)) {
    args <- if (categorical) list(vars_cat = "y_cat", model_cat = "GLMM") else list(vars_cont = "y")
    rhs_arg <- if (categorical) "cat_rhs" else "cont_rhs"
    run <- function(..., rhs = NULL) {
      call_args <- c(list(d = d), args, list(...))
      call_args[[rhs_arg]] <- rhs
      do.call(run_mixed_trend, call_args)
    }
    expect_error(run(invprboot = TRUE), "Set invprboot = FALSE")
    expect_error(run(stratumID = "stratum"), "require a custom formula")
    expect_error(run(subpops = "stratum"), "every reported domain")
    expect_error(run(subpops = "stratum", All_Sites = TRUE), "every reported domain")
    expect_error(run(subpops = "missing"), "every reported domain")
    for (rhs in c("aux + (1 | site)", "aux:Wyear + (1 | site)", "aux + (Wyear | site)")) {
      expect_error(run(rhs = rhs), "must include Wyear as a fixed effect")
    }
    expect_error(run(rhs = "duplicate + Wyear + (1 | site)"), "Wyear could not be estimated")
  }
  expect_error(suppressMessages(trend_analysis(d, vars_cont = "y", model_cont = "LMM",
    siteID = "site", yearID = "year", weight = "w")), "Set invprboot = FALSE")
})

test_that("the default continuous trend uses SLR", {
  d <- mixed_trend_data()
  d$x <- runif(nlevels(d$site))[d$site]
  d$yc <- runif(nlevels(d$site))[d$site]
  args <- list(dframe = d, vars_cont = "y", siteID = "site", yearID = "year",
    weight = "w", xcoord = "x", ycoord = "yc")
  default <- do.call(trend_analysis, args)
  explicit <- do.call(trend_analysis, c(args, list(model_cont = "SLR")))
  expect_s3_class(default$contsum, "data.frame")
  expect_identical(default, explicit)
})

test_that("the default continuous GREG trend uses SLR", {
  set.seed(3)
  d <- data.frame(siteID = 1:60, weight = 5, x = runif(60))
  panel <- do.call(rbind, lapply(1:4, function(year)
    transform(d, year = year, y = 2 + x + 0.3 * year + rnorm(60))))
  totals <- colSums(model.matrix(~x, d) * d$weight)
  args <- list(dframe = panel, vars_cont = "y", yearID = "year", formula = ~x,
    popsize = setNames(rep(list(totals), 4), 1:4), vartype = "SRS")
  default <- do.call(trend_analysis, args)
  expect_s3_class(default$contsum, "data.frame")
  expect_identical(default, do.call(trend_analysis, c(args, list(model_cont = "SLR"))))
  expect_error(do.call(trend_analysis, c(args, list(model_cont = "LMM"))), "SLR/WLR")
})

test_that("GLMM-only trends run without coordinates under the new default", {
  d <- mixed_trend_data()
  result <- suppressWarnings(trend_analysis(d, vars_cat = "y_cat", model_cat = "GLMM",
    invprboot = FALSE, siteID = "site", yearID = "year", weight = "w"))
  fit <- suppressWarnings(lme4::glmer(y_cat ~ Wyear + (1 + Wyear | site) + (1 | year),
    d, family = binomial()))
  expect_mixed_trend(result$catsum, fit)
})

test_that("mixed trend domain guards use the retained analysis rows", {
  d <- mixed_trend_data()
  d$whole <- "one"
  run <- function(data, ...) {
    run_mixed_trend(data, vars_cat = "y_cat", model_cat = "GLMM", vars_cont = "y", ...)
  }
  all_sites <- run(d)
  one_domain <- run(d, subpops = "whole")
  for (table in names(all_sites)) {
    expect_identical(one_domain[[table]][-(1:2)], all_sites[[table]][-(1:2)])
  }

  d$stratum <- as.character(d$stratum)
  d$stratum[1L] <- "singleton"
  d$whole[1L] <- NA
  rhs <- "Wyear + (1 | site)"
  result <- run(d, cat_rhs = rhs, cont_rhs = rhs, subpops = "whole", stratumID = "stratum")
  reference <- run(d[-1L, ], cat_rhs = rhs, cont_rhs = rhs, subpops = "whole", stratumID = "stratum")
  expect_identical(result, reference)
})

test_that("mixed trend guards apply only to the active mixed family", {
  d <- mixed_trend_data()
  result <- trend_analysis(d, vars_cat = "y_cat", model_cont = "LMM",
    siteID = "site", yearID = "year", weight = "w", vartype = "SRS")
  expect_s3_class(result$catsum, "data.frame")
  rhs <- "Wyear + (1 | site)"
  a <- run_mixed_trend(d, vars_cat = "y_cat", cat_rhs = "unused", vars_cont = "y",
    cont_rhs = rhs, stratumID = "stratum")
  b <- run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM", cat_rhs = rhs,
    vars_cont = "y", model_cont = "SLR", cont_rhs = "unused", stratumID = "stratum")
  for (result in list(a, b)) {
    expect_s3_class(result$catsum, "data.frame")
    expect_s3_class(result$contsum, "data.frame")
  }
})

#
# Section for data input
#

# Read the data frame containing survey design and analysis variables
load(system.file("extdata", "NLA_IN.rda", package = "spsurvey"))

# Create a population size data frame
popsize <- data.frame(
  LAKE_ORGN = c("MAN_MADE", "NATURAL"),
  Total = c(6000, 14000)
)

# Create finite population correction factor objects
fpc1 <- 20000
fpc2a <- list(
  Urban = 5000,
  "Non-Urban" = 15000
)
fpc2b <- list(
  MAN_MADE = 6000,
  NATURAL = 14000
)
fpc3 <- c(
  Ncluster = 200,
  clusterID_1 = 100,
  clusterID_2 = 100,
  clusterID_3 = 100,
  clusterID_4 = 100
)
fpc4a <- list(
  Urban = c(
    Ncluster = 75,
    clusterID_1 = 50,
    clusterID_2 = 50,
    clusterID_3 = 50,
    clusterID_4 = 50
  ),
  "Non-Urban" = c(
    Ncluster = 125,
    clusterID_1 = 50,
    clusterID_2 = 50,
    clusterID_3 = 50,
    clusterID_4 = 50
  )
)
fpc4b <- list(
  NATURAL = c(
    Ncluster = 130,
    clusterID_1 = 50,
    clusterID_2 = 50,
    clusterID_3 = 50,
    clusterID_4 = 50
  ),
  MAN_MADE = c(
    Ncluster = 70,
    clusterID_1 = 50,
    clusterID_2 = 50,
    clusterID_3 = 50,
    clusterID_4 = 50
  )
)

#
# Section for the trend data analysis function
#

# Create a year variable for trend estimation
NLA_IN$year <- NLA_IN$YEAR - 2007

# Assign response variable names to the vars_cat and vars_cont vectors
vars_cat <- c("BENT_MMI_COND_2017")
vars_cont <- c("ContVar")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites", "LAKE_ORGN")

# Perform tests
Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD"
)

test_that("Trend: Unstratified single-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize
)

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  fpc = fpc1
)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "LAKE_ORGN"
)

test_that("Trend: Stratified single-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "LAKE_ORGN", fpc = fpc2b
)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Trend: Unstratified two-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS"
)

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS"
)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Trend: Stratified two-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS"
)

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(
  dframe = NLA_IN, vars_cat = vars_cat,
  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4b, vartype = "SRS"
)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

test_that("A warning (in message form) is produced", {
  expect_message(expect_error(trend_analysis(
    dframe = NLA_IN, vars_cat = vars_cat,
    vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
    siteID = "UNIQUE_ID", weight = "XYZ", xcoord = "XCOORD", ycoord = "YCOORD",
    stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
    xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4b, vartype = "SRS"
  )))
})

test_that("categorical trend rejects an all-missing response", {
  d <- declared_category_fixture()
  d$response[] <- NA
  expect_error(trend_analysis(d, vars_cat = "response", yearID = "year", siteID = "site",
    vartype = "SRS"), '"response" has no non-missing values.', fixed = TRUE)
})

test_that("GLMM reports declared categories and limits the drop-level hint", {
  d <- mixed_trend_data()
  levels(d$y_cat) <- c(levels(d$y_cat), "Unused")
  expect_error(run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM"),
    "exactly two declared levels.*Unused.*droplevels")
  d$y_cat[] <- "yes"
  err <- tryCatch(run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM"),
    error = conditionMessage)
  expect_match(err, "Unused")
  expect_false(grepl("droplevels", err, fixed = TRUE))
})

test_that("GLMM constant rows follow the fitted design and placement", {
  d <- mixed_trend_data()
  d$Trend <- d$aux
  d$extra <- as.numeric(d$year == max(d$year))
  d$second <- d$y_cat
  d$second[d$extra == 1] <- NA
  rhs <- "0 + Trend + Wyear + extra + (1 + Wyear | site) + (0 + Trend | site)"
  for (variables in list(c("second", "y_cat"), c("y_cat", "second"), "second", "y_cat")) {
    run <- function(data) run_mixed_trend(data, vars_cat = variables, model_cat = "GLMM",
      cat_rhs = rhs, stratumID = "stratum", conf = 90)$catsum
    reference <- run(d)
    for (constant_vars in list(variables[1L], variables[length(variables)], variables)) {
      modified <- d
      for (variable in constant_vars) {
        modified[[variable]][!is.na(modified[[variable]])] <- "no"
      }
      actual <- run(modified)
      expect_identical(names(actual), names(reference))
      expect_identical(actual$Indicator, variables)
      expect_true(all(is.na(actual[actual$Indicator %in% constant_vars, -(1:3)])))
      expect_match(paste(warn_df$warning, collapse = " "), "constant among the complete model rows")
    }
  }
})

test_that("GLMM uses complete fixed and random model rows to diagnose a constant", {
  d <- mixed_trend_data()
  d$fixed <- ifelse(d$y_cat == "yes", NA_real_, d$aux)
  d$group <- d$site
  d$group[d$y_cat == "no"] <- NA
  run <- function(rhs) run_mixed_trend(d, vars_cat = "y_cat", model_cat = "GLMM",
    cat_rhs = rhs)$catsum
  expect_true(all(is.na(run("Wyear + fixed + (1 | site)")[-(1:3)])))
  expect_true(all(is.na(run("Wyear + (1 | group)")[-(1:3)])))
  expect_error(run("Wyear + fixed + (1 | group)"), "Invalid grouping factor specification")
  reference <- run("Wyear + (1 + Wyear || site)")
  d$y_cat[] <- "yes"
  expect_identical(names(run("Wyear + (1 + Wyear || site)")), names(reference))
  expect_true(all(is.na(run("Wyear + (1 + Wyear || site)")[-(1:3)])))
})

test_that("categorical trends report absent and insufficient categories", {
  d <- declared_category_fixture()
  panel <- do.call(rbind, lapply(2000:2003, function(year) transform(d, year = year)))
  panel$response[panel$year == 2000] <- "Good"
  panel$response[panel$year > 2000 & panel$site <= panel$year - 1995] <- "Poor"
  for (greg in c(FALSE, TRUE)) {
    run <- function(model, frame = panel) suppressMessages(suppressWarnings(trend_analysis(
      frame, vars_cat = "response", siteID = "site", yearID = "year", vartype = "SRS",
      model_cat = model, formula = if (greg) ~1, popsize = if (greg)
        setNames(rep(list(c("(Intercept)" = sum(d$weight))), length(unique(frame$year))),
          unique(frame$year))))$catsum)
    wlr <- run("WLR")
    expect_true(all(is.finite(wlr$Trend_Estimate[wlr$Category != "Absent"])))
    expect_true(all(is.na(wlr[wlr$Category == "Absent", -(1:4)])))
    expect_true(any(grepl('category "Poor".*2000', warn_df$warning)))
    short <- run("WLR", subset(panel, year < 2003))
    expect_true(all(is.na(short[-(1:4)])))
    slr <- run("SLR")
    absent <- slr[slr$Category == "Absent", ]
    expect_equal(absent$Trend_Estimate, 0)
    expect_equal(absent$Trend_Std_Error, 0)
    expect_true(all(is.na(absent[c("Trend_p_Value", "R_Squared", "Adj_R_Squared")])))
    if (greg) {
      missing <- panel
      missing$response[] <- NA
      expect_null(run("SLR", missing))
      expect_null(run("WLR", missing))
    }
  }
})

test_that("GLMM success follows the second declared level", {
  d <- mixed_trend_data()
  run <- function(data) run_mixed_trend(data, vars_cat = "y_cat", model_cat = "GLMM",
    cat_rhs = "Wyear + (1 | site)")$catsum
  forward <- run(d)
  d$y_cat <- factor(d$y_cat, levels = rev(levels(d$y_cat)))
  reversed <- run(d)
  expect_equal(as.numeric(reversed$Trend_Estimate),
    -as.numeric(forward$Trend_Estimate), tolerance = 1e-5)
})

test_that("categorical response preserves the continuous mixed-model predictor", {
  d <- mixed_trend_data()
  d$predictor <- factor(d$y_cat, levels = c("yes", "unused", "no"))
  args <- list(dframe = d, vars_cont = "y", siteID = "site", yearID = "year",
    weight = "w", vartype = "SRS", model_cont = "LMM", invprboot = FALSE,
    cont_rhs = "Wyear + predictor + (1 | site)")
  alone <- suppressMessages(suppressWarnings(do.call(trend_analysis, args)))
  args$vars_cat <- "predictor"
  combined <- suppressMessages(suppressWarnings(do.call(trend_analysis, args)))
  expect_identical(combined$contsum, alone$contsum)
  expect_identical(unique(as.character(combined$catsum$Category)), levels(d$predictor))
})
