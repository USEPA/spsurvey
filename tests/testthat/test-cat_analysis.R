context("categorical variable analysis")

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
# Section for the categorical data analysis function
#

# Assign response variable names to the vars vector
vars <- c("BENT_MMI_COND_2017")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites", "LAKE_ORGN")

# Perform tests

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD"
)

test_that("Categorical: Unstratified single-stage analysis, local mean variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize
)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", fpc = fpc1
)

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17"
)

test_that("Categorical: Stratified single-stage analysis, local mean variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popsize = popsize
)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", fpc = fpc2a
)

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Categorical: Unstratified two-stage analysis, SRS variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  vartype = "SRS"
)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS"
)

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS"
)

test_that("Categorical: Stratified two-stage analysis, SRS variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, vartype = "SRS"
)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  fpc = fpc4a, vartype = "SRS"
)

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT"
)

test_that("Categorical: Unstratified single-stage analysis, HT-Overton variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT",
  jointprob = "hr"
)

test_that("Categorical: Unstratified single-stage analysis, HT-HR variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT",
  jointprob = "brewer"
)

test_that("Categorical: Unstratified single-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  stratumID = "URBN_NLA17", vartype = "HT", jointprob = "brewer"
)

test_that("Categorical: Stratified single-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  clusterID = "clusterID", weight1 = "weight1", vartype = "HT",
  jointprob = "brewer"
)

test_that("Categorical: Unstratified two-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  vartype = "HT", jointprob = "brewer"
)

test_that("Categorical: Stratified two-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "YG"
)

test_that("Categorical: Unstratified single-stage analysis, YG-Overton variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(
  dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "YG",
  jointprob = "hr"
)

test_that("Categorical: Unstratified single-stage analysis, YG-HR variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

test_that("A warning (in message form) is produced", {
  expect_message(expect_error(cat_analysis(
    dframe = NLA_IN, vars = vars,
    subpops = subpops, siteID = "SITE_ID", weight = "XYZ", vartype = "YG",
    jointprob = "hr"
  )))
})

test_that("categorical status reports declared levels across variance estimators", {
  d <- declared_category_fixture()
  pop <- tapply(d$weight, d$domain, sum)
  for (vt in c("Local", "SRS", "HT", "YG")) {
    for (size in list(NULL, data.frame(domain = names(pop), Freq = as.numeric(pop)),
        list(domain = pop))) {
      if (vt %in% c("HT", "YG") && is.data.frame(size)) next
      ans <- cat_analysis(d, "response", subpops = "domain", All_Sites = TRUE,
        siteID = "site", xcoord = "x", ycoord = "y", vartype = vt, popsize = size)
      expect_identical(as.character(ans$Category), rep(c(levels(d$response), "Total"), 3L))
      zero <- ans[ans$Category == "Absent", ]
      expect_true(all(zero[c("nResp", "Estimate.P", "Estimate.U", "StdError.P", "StdError.U")] == 0))
    }
  }
  a <- cat_analysis(d, "response", vartype = "SRS")
  b <- cat_analysis(d, "response", vartype = "SRS", formula = ~1,
    popsize = c("(Intercept)" = sum(d$weight)))
  expect_identical(as.character(a$Category), as.character(b$Category))
  d$response[] <- NA
  for (vt in c("Local", "SRS")) {
    expect_error(cat_analysis(d, "response", xcoord = "x", ycoord = "y", vartype = vt),
      '"response" has no non-missing values.', fixed = TRUE)
  }
})

test_that("status keeps case-sensitive reserved category labels", {
  d <- declared_category_fixture()
  for (observed in c(FALSE, TRUE)) {
    data <- d
    levels(data$response)[if (observed) 1L else 2L] <- "Total"
    for (greg in c(FALSE, TRUE)) {
      expect_error(cat_analysis(data, "response", vartype = "SRS",
        formula = if (greg) ~1, popsize = if (greg) c("(Intercept)" = sum(d$weight))),
        'category named "Total"', fixed = TRUE)
    }
  }
  levels(d$response)[1L] <- "total"
  expect_identical(as.character(cat_analysis(d, "response", vartype = "SRS")$Category),
    c("total", "Absent", "Good", "Total"))
})

test_that("categorical status copies factors with overlapping roles", {
  d <- declared_category_fixture()
  d$copy <- droplevels(d$response)
  for (role in c("subpops", "stratumID", "poststratum", "greg")) {
    args <- list(dframe = d, vars = "response", vartype = "SRS")
    if (role == "greg") {
      args$dframe$response <- ordered(droplevels(d$response))
      args$dframe$copy <- args$dframe$response
      args$formula <- ~response
      args$popsize <- colSums(model.matrix(~response, args$dframe) * d$weight)
      control <- args
      control$formula <- ~copy
      names(control$popsize)[-1L] <- "copy.L"
    } else if (role == "poststratum") {
      args$popsize <- data.frame(response = c("Poor", "Good"), Freq = c(72, 48))
      control <- args
      names(control$popsize)[1L] <- "copy"
    } else {
      args[[role]] <- "response"
      control <- args
      control[[role]] <- "copy"
    }
    actual <- do.call(cat_analysis, args)
    expected <- do.call(cat_analysis, control)
    if (role == "subpops") expected$Type[] <- "response"
    expect_equal(actual, expected)
  }
})

test_that("single-category total bounds follow the domain order under SRS, HT and YG", {
  d <- sf::st_drop_geometry(NLA_PNW)
  d$one <- 1
  d$category <- factor("Good")
  d$probability <- 1 / d$WEIGHT
  for (vt in c("SRS", "HT", "YG")) {
    design <- if (vt == "SRS") survey::svydesign(~1, weights = ~WEIGHT, data = d) else
      survey::svydesign(~1, probs = ~probability, variance = vt, pps = "overton", data = d)
    by_domain <- survey::svyby(~one, ~STATE, design, survey::svytotal)
    bounds <- confint(by_domain)
    result <- cat_analysis(d, "category", subpops = "STATE", weight = "WEIGHT", vartype = vt)
    i <- match(result$Subpopulation, by_domain$STATE)
    expect_equal(result$LCB95Pct.U, pmax(0, unname(bounds[i, 1L])))
    expect_equal(result$UCB95Pct.U, unname(bounds[i, 2L]))
  }
})

test_that("status fallback warnings use the requested response name", {
  d <- declared_category_fixture()[1:11, ]
  d$response <- factor(c(rep("Poor", 3), rep("Good", 8)), levels = c("Poor", "Unused", "Good"))
  suppressMessages(cat_analysis(d, vars = "response", subpops = "response", siteID = "site",
    xcoord = "x", ycoord = "y", vartype = "Local", subset_local = TRUE))
  fallback <- warn_df[warn_df$func %in% c("catvar_prop", "catvar_total"), ]
  expect_gt(nrow(fallback), 0)
  expect_true(all(fallback$indicator == "response"))
})
