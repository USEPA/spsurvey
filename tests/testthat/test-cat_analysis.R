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
