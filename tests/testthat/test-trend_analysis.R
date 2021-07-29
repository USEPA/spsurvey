context("trend analysis")

#
# Section for data input
#

# Read the data frame containing survey design and analysis variables
load(system.file("extdata", "NLA_IN.rda", package = "spsurvey"))

# Create a population size data frame
popsize <- data.frame(
  LAKE_ORGN = c("MAN_MADE", "NATURAL"),
  Total = c(6000, 14000))

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
Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD")

test_that("Trend: Unstratified single-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  popsize = popsize)

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  fpc = fpc1)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  stratumID = "LAKE_ORGN")

test_that("Trend: Stratified single-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  stratumID = "LAKE_ORGN", fpc = fpc2b)

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
                                  ycoord1 = "ycoord1", vartype = "SRS")

test_that("Trend: Unstratified two-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
                                  ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS")

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
                                  ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS")

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
                                  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS")

test_that("Trend: Stratified two-stage analysis", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
                                  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS")

test_that("Trend: with known population sizes", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})

Trend_Estimates <- trend_analysis(dframe = NLA_IN, vars_cat = vars_cat,
                                  vars_cont = vars_cont, model_cont = "SLR", subpops = subpops,
                                  siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
                                  stratumID = "LAKE_ORGN", clusterID = "clusterID", weight1 = "weight1",
                                  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4b, vartype = "SRS")

test_that("Trend: with finite population correction factor", {
  expect_true(exists("Trend_Estimates"))
  expect_equal(attributes(Trend_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Trend_Estimates$contsum)$class, "data.frame")
  expect_equal(nrow(Trend_Estimates$catsum), 6)
  expect_equal(nrow(Trend_Estimates$contsum), 3)
})
