context("continuous variable analysis")

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
# Section for the continuous data analysis function
#

# Subset the input data frame
dframe <- droplevels(subset(NLA_IN, YEAR == 2017))

# Assign response variable names to the vars vector
vars <- c("ContVar")

# Assign subpopulation variable names to the subpops vector
subpops <- c("All_Sites", "LAKE_ORGN")

# Perform tests
CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD"
)

test_that("Continuous: Unstratified single-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  popsize = popsize
)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  fpc = fpc1
)

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17"
)

test_that("Continuous: Stratified single-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", popsize = popsize
)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", fpc = fpc2a
)

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Continuous: Unstratified two-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", popsize = popsize, vartype = "SRS"
)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  clusterID = "clusterID", weight1 = "weight1", xcoord1 = "xcoord1",
  ycoord1 = "ycoord1", fpc = fpc3, vartype = "SRS"
)

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", vartype = "SRS"
)

test_that("Continuous: Stratified two-stage analysis", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", popsize = popsize,
  vartype = "SRS"
)

test_that("Continuous: with known population sizes", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})

CDF_Estimates <- cont_analysis(
  dframe = dframe, vars = vars, subpops = subpops,
  siteID = "SITE_ID", weight = "WGT_TP", xcoord = "XCOORD", ycoord = "YCOORD",
  stratumID = "URBN_NLA17", clusterID = "clusterID", weight1 = "weight1",
  xcoord1 = "xcoord1", ycoord1 = "ycoord1", fpc = fpc4a, vartype = "SRS"
)

test_that("Continuous: with finite population correction factor", {
  expect_true(exists("CDF_Estimates"))
  expect_equal(attributes(CDF_Estimates$CDF)$class, c("sp_CDF", "data.frame"))
  expect_equal(attributes(CDF_Estimates$Pct)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Mean)$class, "data.frame")
  expect_equal(attributes(CDF_Estimates$Total)$class, "data.frame")
  expect_equal(nrow(CDF_Estimates$CDF), 66)
  expect_equal(nrow(CDF_Estimates$Pct), 21)
  expect_equal(nrow(CDF_Estimates$Mean), 3)
  expect_equal(nrow(CDF_Estimates$Total), 3)
})
