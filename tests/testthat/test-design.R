context("grts")

set.seed(52468110)
# Create finite frame shapefile object
load(system.file("extdata", "reg1_lakes.rda", package = "spsurvey"))
reg1_lakes$mdcaty <- runif(nrow(reg1_lakes))


# Finite: reg1_lakes point sample frame with equal random selection:
testsample <- grts(
  sframe = reg1_lakes, n_base = 10,
  n_over = 0, seltype = "equal"
)

test_that("test finite design equal random selection", {
  expect_true(exists("testsample"))
  expect_equal(names(testsample)[4], "dsgn")
  expect_equal(nrow(testsample$sites_base), 10)
})

# Finite: reg1_lakes point sample frame with unequal random selection:
caty_n <- c("LAKE/POND" = 15, "RESERVOIR" = 5)
testsample <- grts(
  sframe = reg1_lakes, n_base = 20,
  n_over = 0, seltype = "unequal",
  caty_var = "FTYPE", caty_n = caty_n
)


test_that("test unequal random selection", {
  expect_true(exists("testsample"))
  expect_equal(names(testsample)[4], "dsgn")
  expect_equal(nrow(testsample$sites_base), 20)
})

# Finite: NHDPoint PointZ shapefile:
load(system.file("extdata", "NHDPoint.rda", package = "spsurvey"))

testsample <- grts(
  sframe = NHDPoint, n_base = 10,
  n_over = 0, seltype = "equal"
)

test_that("test equal random selection using NHDPoint", {
  expect_true(exists("testsample"))
  expect_equal(names(testsample)[4], "dsgn")
  expect_equal(nrow(testsample$sites_base), 10)
})

# Linear: Butte Creek linear sample frame:
load(system.file("extdata", "ButteCreek.rda", package = "spsurvey"))

testsample <- grts(
  sframe = ButteCreek, n_base = 10,
  n_over = 0, seltype = "equal"
)

test_that("test equal random selection using linear sample frame", {
  expect_true(exists("testsample"))
  expect_equal(names(testsample)[4], "dsgn")
  expect_equal(nrow(testsample$sites_base), 10)
})

# Linear: eco_l3 area sample frame:
load(system.file("extdata", "eco_l3_ut.rda", package = "spsurvey"))

testsample <- grts(
  sframe = eco_l3_ut, n_base = 10,
  n_over = 0, seltype = "equal"
)

test_that("test equal random selection using area sample frame", {
  expect_true(exists("testsample"))
  expect_equal(names(testsample)[4], "dsgn")
  expect_equal(nrow(testsample$sites_base), 10)
})
