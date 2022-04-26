context("change analysis")

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
# Section for the change data analysis function
#

# Subset the input data frame
dframe <- droplevels(subset(NLA_IN, YEAR != 2017))

# Assign response variable names to the vars_cat and vars_cont vectors
vars_cat <- c("BENT_MMI_COND_2017")
vars_cont <- c("ContVar")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites")

# Create a population size data frame
popsize <- data.frame(
  All_Sites = c("Indiana Lakes"),
  Total = c(20000)
)

# Perform tests
Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD"
)

test_that("Change: Unstratified single-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize
)

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", fpc = fpc1
)

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN"
)

test_that("Change: Stratified single-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", fpc = fpc2b
)

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Change: Unstratified two-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  vartype = "SRS"
)

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS"
)

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS"
)

test_that("Change: Stratified two-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, vartype = "SRS"
)

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(
  dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "total", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  fpc = fpc4b, vartype = "SRS"
)

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_total)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_total), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})
