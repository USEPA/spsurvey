context("CDF testing")

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
# Section for the CDF inference function
#

# Assign response variable names to the vars vector
vars <- c("ContVar")

# Assign subpopulation variable names to the subpops vector
subpops <- c("LAKE_ORGN", "AG_ECO9")

# Perform tests
CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  testname = "adjWald"
)

test_that("CDF Inference: Unstratified single-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  testname = "RaoScott_First"
)

test_that("CDF Inference: Unstratified single-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize, testname = "adjWald"
)

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize, testname = "RaoScott_First"
)

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  fpc = fpc1, testname = "adjWald"
)

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  fpc = fpc1, testname = "RaoScott_First"
)

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", testname = "adjWald"
)

test_that("CDF Inference: Stratified single-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", testname = "RaoScott_First"
)

test_that("CDF Inference: Stratified single-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize, testname = "adjWald"
)

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize, testname = "RaoScott_First"
)

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", fpc = fpc2a, testname = "adjWald"
)

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", fpc = fpc2a,
  testname = "RaoScott_First"
)

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS", testname = "adjWald"
)

test_that("CDF Inference: Unstratified two-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS", testname = "RaoScott_First"
)

test_that("CDF Inference: Unstratified two-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS", testname = "adjWald"
)

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS",
  testname = "RaoScott_First"
)

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS", testname = "adjWald"
)

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS", testname = "RaoScott_First"
)

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS",
  testname = "adjWald"
)

test_that("CDF Inference: Stratified two-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS",
  testname = "RaoScott_First"
)

test_that("CDF Inference: Stratified two-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  vartype = "SRS", testname = "adjWald"
)

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  vartype = "SRS", testname = "RaoScott_First"
)

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4a, vartype = "SRS",
  testname = "adjWald"
)

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(
  dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4a, vartype = "SRS",
  testname = "RaoScott_First"
)

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Tests"))
  expect_equal(attributes(CDF_Tests)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})
