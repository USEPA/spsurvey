context("relative risk analysis")

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
# Section for the relative risk data analysis function
#

# Assign response variable names and stressor variable names to the
# vars_response and  vars_stressor vectors, respectively
vars_response <- c("BENT_MMI_COND_2017")
vars_stressor <- c("PTL_COND", "NTL_COND")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites", "LAKE_ORGN")

# Perform tests
RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD"
)

test_that("Relative Risk: Unstratified single-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD",
  response_levels = list("BENT_MMI_COND_2017" = c("Poor", "Good")),
  stressor_levels = list("PTL_COND" = c("Poor", "Good"), "NTL_COND" = c("Poor", "Good"))
)

test_that("Relative Risk: Unstratified single-stage analysis (specify response/stressor levels)", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD",
  response_levels = list(c("Poor", "Good")),
  stressor_levels = list(c("Poor", "Good"), c("Poor", "Good"))
)

test_that("Relative Risk: Unstratified single-stage analysis (unnamed response/stressor levels)", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize
)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", fpc = fpc1
)

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17"
)

test_that("Relative Risk: Stratified single-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popsize = popsize
)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", fpc = fpc2a
)

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1"
)

test_that("Relative Risk: Unstratified two-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize
)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc3
)

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS"
)

test_that("Relative Risk: Stratified two-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, vartype = "SRS"
)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(
  dframe = NLA_IN,
  vars_response = vars_response, vars_stressor = vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  fpc = fpc4a, vartype = "SRS"
)

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})
