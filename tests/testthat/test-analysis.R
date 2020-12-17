context("survey analysis")
#
# Section for data input
#

# Read the data frame containing survey design and analysis variables
load(system.file("extdata", "NLA_IN.rda", package = "svyanalysis"))

# Create a population size data frame
popsize <- data.frame(
   LAKE_ORGN = c("MAN_MADE", "NATURAL"),
   Total = c(16000, 4000))

#
# Section for the categorical data analysis function
#

# Assign response variable names to the vars vector
vars <- c("BENT_MMI_COND_2017")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites", "LAKE_ORGN")

# Perform tests

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD")

test_that("Categorical: Unstratified single-stage analysis, local mean variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17")

test_that("Categorical: Stratified single-stage analysis, local mean variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popsize = popsize)

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popcorrect = TRUE,
  fpcsize = "fpcsize")

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS")

test_that("Categorical: Unstratified two-stage analysis, SRS variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster", vartype = "SRS")

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size", vartype = "SRS")

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS")

test_that("Categorical: Stratified two-stage analysis, SRS variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, Ncluster = "Ncluster", vartype = "SRS")

test_that("Categorical: with known population sizes", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popcorrect = TRUE, Ncluster = "Ncluster", stage1size="stage1size",
  vartype = "SRS")

test_that("Categorical: with finite population correction factor", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT")

test_that("Categorical: Unstratified single-stage analysis, HT-Overton variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT",
  jointprob = "hr")

test_that("Categorical: Unstratified single-stage analysis, HT-HR variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", vartype = "HT",
  jointprob = "brewer")

test_that("Categorical: Unstratified single-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  stratumID = "URBN_NLA17", vartype = "HT", jointprob = "brewer")

test_that("Categorical: Stratified single-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  clusterID = "clusterID", weight1 = "weight1", vartype = "HT",
  jointprob = "brewer")

test_that("Categorical: Unstratified two-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  vartype = "HT", jointprob = "brewer")

test_that("Categorical: Stratified two-stage analysis, HT-Brewer variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",  vartype = "YG")

test_that("Categorical: Unstratified single-stage analysis, YG-Overton variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

Condition_Estimates <- cat_analysis(dframe = NLA_IN, vars = vars,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP",  vartype = "YG",
  jointprob = "hr")

test_that("Categorical: Unstratified single-stage analysis, YG-HR variance", {
  expect_true(exists("Condition_Estimates"))
  expect_equal(attributes(Condition_Estimates)$class, "data.frame")
  expect_equal(nrow(Condition_Estimates), 9)
})

#
# Section for the continuous data analysis function
#

# Subset the input data frame
dframe <- droplevels(subset(NLA_IN, YEAR == 2017))

# Assign response variable names to the vars vector
vars <- c("ContVar")

# Perform tests
CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD")

test_that("Continuous: Unstratified single-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17")

test_that("Continuous: Stratified single-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",  ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS")

test_that("Continuous: Unstratified two-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, Ncluster = "Ncluster", vartype = "SRS")

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popcorrect = TRUE, Ncluster = "Ncluster",
  stage1size="stage1size", vartype = "SRS")

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS")

test_that("Continuous: Stratified two-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster", vartype = "SRS")

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

CDF_Estimates <- cont_analysis(dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size", vartype = "SRS")

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 24)
})

#
# Section for the relative risk data analysis function
#

# Assign response variable names and stressor variable names to the
# vars_response and  vars_stressor vectors, respectively
vars_response <- c("BENT_MMI_COND_2017")
vars_stressor <- c("PTL_COND", "NTL_COND")

# Perform tests
RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD")

test_that("Relative Risk: Unstratified single-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17")

test_that("Relative Risk: Stratified single-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popsize = popsize)

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popcorrect = TRUE,
  fpcsize = "fpcsize")

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1")

test_that("Relative Risk: Unstratified two-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster")

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size")

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS")

test_that("Relative Risk: Stratified two-stage analysis", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, Ncluster = "Ncluster", vartype = "SRS")

test_that("Relative Risk: with known population sizes", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

RelRisk_Estimates <- relrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popcorrect = TRUE, Ncluster = "Ncluster", stage1size="stage1size",
  vartype = "SRS")

test_that("Relative Risk: with finite population correction factor", {
  expect_true(exists("RelRisk_Estimates"))
  expect_equal(attributes(RelRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(RelRisk_Estimates), 6)
})

#
# Section for the attributable risk data analysis function
#

# Perform tests
AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD")

test_that("Attributable Risk: Unstratified single-stage analysis", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize)

test_that("Attributable Risk: with known population sizes", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Attributable Risk: with finite population correction factor", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17")

test_that("Attributable Risk: Stratified single-stage analysis", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popsize = popsize)

test_that("Attributable Risk: with known population sizes", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", popcorrect = TRUE,
  fpcsize = "fpcsize")

test_that("Attributable Risk: with finite population correction factor", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1")

test_that("Attributable Risk: Unstratified two-stage analysis", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster")

test_that("Attributable Risk: with known population sizes", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size")

test_that("Attributable Risk: with finite population correction factor", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS")

test_that("Attributable Risk: Stratified two-stage analysis", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, Ncluster = "Ncluster", vartype = "SRS")

test_that("Attributable Risk: with known population sizes", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

AttRisk_Estimates <- attrisk_analysis(dframe = NLA_IN,
  vars_response = vars_response, vars_stressor= vars_stressor,
  subpops = subpops, siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "URBN_NLA17", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popcorrect = TRUE, Ncluster = "Ncluster", stage1size="stage1size",
  vartype = "SRS")

test_that("Attributable Risk: with finite population correction factor", {
  expect_true(exists("AttRisk_Estimates"))
  expect_equal(attributes(AttRisk_Estimates)$class, "data.frame")
  expect_equal(nrow(AttRisk_Estimates), 6)
})

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
   Total = c(20000))

# Perform tests
Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD")

test_that("Change: Unstratified single-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popsize = popsize)

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", popcorrect = TRUE, fpcsize = "fpcsize")

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN")

test_that("Change: Stratified single-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", popcorrect = TRUE,
  fpcsize = "fpcsize")

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS")

test_that("Change: Unstratified two-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster", vartype = "SRS")

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size", vartype = "SRS")

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  vartype = "SRS")

test_that("Change: Stratified two-stage analysis", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popsize = popsize, Ncluster = "Ncluster", vartype = "SRS")

test_that("Change: with known population sizes", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

Change_Estimates <- change_analysis(dframe = dframe, vars_cat = vars_cat,
  vars_cont = vars_cont, test = c("mean", "median"), subpops = subpops,
  surveyID = "YEAR", siteID = "UNIQUE_ID", weight = "WGT_TP", xcoord = "XCOORD",
  ycoord = "YCOORD", stratumID = "LAKE_ORGN", clusterID = "clusterID",
  weight1 = "weight1", xcoord1 = "xcoord1", ycoord1 = "ycoord1",
  popcorrect = TRUE, Ncluster = "Ncluster", stage1size="stage1size",
  vartype = "SRS")

test_that("Change: with finite population correction factor", {
  expect_true(exists("Change_Estimates"))
  expect_equal(attributes(Change_Estimates$catsum)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_mean)$class, "data.frame")
  expect_equal(attributes(Change_Estimates$contsum_median)$class, "data.frame")
  expect_equal(nrow(Change_Estimates$catsum), 2)
  expect_equal(nrow(Change_Estimates$contsum_mean), 1)
  expect_equal(nrow(Change_Estimates$contsum_median), 2)
})

#
# Section for the CDF inference function
#
library(svyanalysis)

# Assign response variable names to the vars vector
vars <- c("ContVar")

# Assign subpopulation variable names to the subpops vector
subpops <- c("LAKE_ORGN", "AG_ECO9")

# Perform tests
CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  testname = "adjWald")

test_that("CDF Inference: Unstratified single-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  testname = "RaoScott_First")

test_that("CDF Inference: Unstratified single-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize, testname = "adjWald")

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize, testname = "RaoScott_First")

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popcorrect = TRUE, fpcsize = "fpcsize", testname = "adjWald")

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popcorrect = TRUE, fpcsize = "fpcsize", testname = "RaoScott_First")

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", testname = "adjWald")

test_that("CDF Inference: Stratified single-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", testname = "RaoScott_First")

test_that("CDF Inference: Stratified single-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize, testname = "adjWald")

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize, testname = "RaoScott_First")

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popcorrect = TRUE, fpcsize = "fpcsize",
  testname = "adjWald")

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popcorrect = TRUE, fpcsize = "fpcsize",
  testname = "RaoScott_First")

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS", testname = "adjWald")

test_that("CDF Inference: Unstratified two-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS", testname = "RaoScott_First")

test_that("CDF Inference: Unstratified two-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, Ncluster = "Ncluster",
  vartype = "SRS", testname = "adjWald")

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, Ncluster = "Ncluster",
  vartype = "SRS", testname = "RaoScott_First")

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popcorrect = TRUE, Ncluster = "Ncluster",
  stage1size="stage1size", vartype = "SRS", testname = "adjWald")

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popcorrect = TRUE, Ncluster = "Ncluster",
  stage1size="stage1size", vartype = "SRS", testname = "RaoScott_First")

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS",
  testname = "adjWald")

test_that("CDF Inference: Stratified two-stage analysis - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS",
  testname = "RaoScott_First")

test_that("CDF Inference: Stratified two-stage analysis - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster", vartype = "SRS", testname = "adjWald")

test_that("CDF Inference: with known population sizes - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  Ncluster = "Ncluster", vartype = "SRS", testname = "RaoScott_First")

test_that("CDF Inference: with known population sizes - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size", vartype = "SRS",
  testname = "adjWald")

test_that("CDF Inference: with finite population correction factor - adjusted Wald", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})

CDF_Tests <- cont_cdftest(dframe = NLA_IN, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popcorrect = TRUE,
  Ncluster = "Ncluster", stage1size="stage1size", vartype = "SRS",
  testname = "RaoScott_First")

test_that("CDF Inference: with finite population correction factor - RaoScott_First", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(nrow(CDF_Tests), 4)
})
