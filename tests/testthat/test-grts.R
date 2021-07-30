context("grts")

# set reproducible seed (as there are random components here)
set.seed(5)

#################################################
########### NE_LAKES DATA TESTS
#################################################

#--------------------------------------
#-------- Regular
#--------------------------------------

# number of grts columns added
col_grts_add <- 9

# number of NE_Lakes columns
col_data <- NCOL(NE_Lakes)

# number of grts columns plus NE_Lakes columns
col_out <- col_grts_add + col_data

# unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal")
  # see if function ran without error
  expect_true(exists("grts_output"))
  # no legacy sites
  expect_equal(NROW(grts_output$sites_legacy), 0)
  # base sample size of 50
  expect_equal(NROW(grts_output$sites_base), n_base)
  # no rho replacement sites
  expect_equal(NROW(grts_output$sites_over), 0)
  # no nn replacement sites
  expect_equal(NROW(grts_output$sites_near), 0)
  # no legacy sites
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  # base sample size columns should equal extra columns plus original columns
  expect_equal(NCOL(grts_output$sites_base), col_out)
  # no rho replacement sites
  expect_equal(NCOL(grts_output$sites_over), 1)
  # no nn replacement sites
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", stratum_var = "ELEV_CAT")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# unstratified, unequal probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "unequal", caty_var = "AREA_CAT", caty_n = caty_n)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, unequal probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  caty_n <- list(low = c(small = 10, large = 10), high = c(small = 10, large = 20))
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal", stratum_var = "ELEV_CAT",
    caty_var = "AREA_CAT", caty_n = caty_n
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, unequal probability (with repeated caty_n)
test_that("algorithm executes", {
  n_base <- c(low = 25, high = 25)
  caty_n <- c(small = 12.5, large = 12.5)
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal", stratum_var = "ELEV_CAT",
    caty_var = "AREA_CAT", caty_n = caty_n
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, unequal probability (with different caty_n)
test_that("algorithm executes", {
  n_base <- c(low = 25, high = 25)
  caty_n <- list(low = c(small = 10, large = 15), high = c(small = 12, large = 13))
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal", stratum_var = "ELEV_CAT",
    caty_var = "AREA_CAT", caty_n = caty_n
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# unstratified, proportional (to size) probability
test_that("algorithm executes", {
  n_base <- 50
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "proportional", aux_var = "AREA")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out + 1)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, proportional probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  grts_output <- grts(NE_Lakes, n_base = n_base, stratum_var = "ELEV_CAT", aux_var = "AREA")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out + 1)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

#--------------------------------------
#-------- Legacy
#--------------------------------------

# legacy sites, unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", legacy_var = "LEGACY")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  expect_equal(NROW(grts_output$sites_base), n_base - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# legacy sites, stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "equal",
    stratum_var = "ELEV_CAT", legacy_var = "LEGACY"
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  n_legacy_low <- sum(grts_output$sites_legacy$stratum == "low")
  n_legacy_high <- sum(grts_output$sites_legacy$stratum == "high")
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]] - n_legacy_low
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]] - n_legacy_high
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base) - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# legacy sites, unequal probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal",
    caty_var = "AREA_CAT", caty_n = caty_n, legacy_var = "LEGACY"
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  expect_equal(NROW(grts_output$sites_base), n_base - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# legacy sites, proportional probability
test_that("algorithm executes", {
  n_base <- 50
  n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "proportional",
    aux_var = "AREA", legacy_var = "LEGACY"
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  expect_equal(NROW(grts_output$sites_base), n_base - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out + 1)
  expect_equal(NCOL(grts_output$sites_base), col_out + 1)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

#--------------------------------------
#-------- Minimum Distance
#--------------------------------------

# minimum distance, unstratified, equal probability
test_that("algorithm executes", {
  library(sf)
  n_base <- 50
  mindis <- 1600
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", mindis = mindis)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
  dist_mx <- as.vector(st_distance(grts_output$sites_base))
  expect_true(min(dist_mx[dist_mx > 0]) > mindis)
})

#--------------------------------------
#-------- RHO replacement
#--------------------------------------

# rho replacement sites, unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  n_over <- 5
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", n_over = n_over)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), n_over)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), col_out)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# rho replacement sites, stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  n_over <- list(low = 2, high = 3)
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "equal",
    stratum_var = "ELEV_CAT", n_over = n_over
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(
    NROW(grts_output$sites_over[grts_output$sites_over$stratum == "low", , drop = FALSE]),
    n_over[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_over[grts_output$sites_over$stratum == "high", , drop = FALSE]),
    n_over[["high"]]
  )
  expect_equal(NROW(grts_output$sites_over), sum(unlist(n_over)))
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), col_out)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# rho replacement sites, unstratified, unequal probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  n_over <- 10
  caty_n_over <- c(small = 5, large = 5)
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal",
    caty_var = "AREA_CAT", caty_n = caty_n, n_over = caty_n_over
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), n_over)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), col_out)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# rho replacement sites, unstratified, proportional probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  n_over <- 10
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "proportional",
    aux_var = "AREA", n_over = n_over
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), n_over)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out + 1)
  expect_equal(NCOL(grts_output$sites_over), col_out + 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

#--------------------------------------
#-------- NN replacement
#--------------------------------------

# nn replacement sites, unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  n_near <- 2
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", n_near = n_near)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), n_base * n_near)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), col_out)
})

# nn replacement sites, stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(low = 20, high = 30)
  n_near <- 2
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "equal",
    stratum_var = "ELEV_CAT", n_near = n_near
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "low", , drop = FALSE]),
    n_base[["low"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "high", , drop = FALSE]),
    n_base[["high"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), n_near * sum(n_base))
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), col_out)
})

# nn replacement sites, unstratified, unequal probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  n_near <- 2
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "unequal",
    caty_var = "AREA_CAT", caty_n = caty_n, n_near = n_near
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), n_base * n_near)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), col_out)
})

# nn replacement sites, unstratified, proportional probability
test_that("algorithm executes", {
  n_base <- 50
  caty_n <- c(small = 24, large = 26)
  n_near <- 2
  grts_output <- grts(NE_Lakes,
    n_base = n_base, seltype = "proportional",
    aux_var = "AREA", n_near = n_near
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), n_base * n_near)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out + 1)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), col_out + 1)
})

#--------------------------------------
#-------- NN replacement
#--------------------------------------

# both replacement sites, unstratified
test_that("algorithm executes", {
  n_base <- 50
  n_over <- 5
  n_near <- 2
  grts_output <- grts(NE_Lakes, n_base = n_base, seltype = "equal", n_over = n_over, n_near = n_near)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), n_over)
  expect_equal(NROW(grts_output$sites_near), (n_base + n_over) * n_near)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), col_out)
  expect_equal(NCOL(grts_output$sites_near), col_out)
})

#################################################
########### Illinois_River DATA TESTS
#################################################

# number of grts columns added
col_grts_add <- 9

# number of Illinois_River columns
col_data <- NCOL(Illinois_River)

# number of grts columns plus Illinois_River columns
col_out <- col_grts_add + col_data

#--------------------------------------
#-------- Regular
#--------------------------------------

# unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  grts_output <- grts(Illinois_River, n_base = n_base, seltype = "equal")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(Oklahoma = 20, Arkansas = 30)
  grts_output <- grts(Illinois_River, n_base = n_base, seltype = "equal", stratum_var = "STATE_NAME")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "Oklahoma", , drop = FALSE]),
    n_base[["Oklahoma"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "Arkansas", , drop = FALSE]),
    n_base[["Arkansas"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

#--------------------------------------
#-------- Legacy
#--------------------------------------

# legacy sites, unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  n_legacy <- nrow(Illinois_River_Legacy)
  grts_output <- grts(Illinois_River, n_base = n_base, seltype = "equal", legacy_sites = Illinois_River_Legacy)
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  expect_equal(NROW(grts_output$sites_base), n_base - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# legacy sites, stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(Oklahoma = 20, Arkansas = 30)
  n_legacy <- nrow(Illinois_River_Legacy)
  grts_output <- grts(Illinois_River,
    n_base = n_base, seltype = "equal",
    stratum_var = "STATE_NAME", legacy_sites = Illinois_River_Legacy,
    legacy_stratum_var = "STATE_NAME"
  )
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), n_legacy)
  n_legacy_Oklahoma <- sum(grts_output$sites_legacy$stratum == "Oklahoma")
  n_legacy_Arkansas <- sum(grts_output$sites_legacy$stratum == "Arkansas")
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "Oklahoma", , drop = FALSE]),
    n_base[["Oklahoma"]] - n_legacy_Oklahoma
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "Arkansas", , drop = FALSE]),
    n_base[["Arkansas"]] - n_legacy_Arkansas
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base) - n_legacy)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), col_out)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

#################################################
########### Great_Lakes DATA TESTS
#################################################

# number of grts columns added
col_grts_add <- 9

# number of Great_Lakes columns
col_data <- NCOL(Great_Lakes)

# number of grts columns plus Great_Lakes columns
col_out <- col_grts_add + col_data

#--------------------------------------
#-------- Regular
#--------------------------------------

# unstratified, equal probability
test_that("algorithm executes", {
  n_base <- 50
  grts_output <- grts(Great_Lakes, n_base = n_base, seltype = "equal")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(NROW(grts_output$sites_base), n_base)
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})

# stratified, equal probability
test_that("algorithm executes", {
  n_base <- c(CAN = 20, USA = 30)
  grts_output <- grts(Great_Lakes, n_base = n_base, seltype = "equal", stratum_var = "COUNTRY")
  expect_true(exists("grts_output"))
  expect_equal(NROW(grts_output$sites_legacy), 0)
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "CAN", , drop = FALSE]),
    n_base[["CAN"]]
  )
  expect_equal(
    NROW(grts_output$sites_base[grts_output$sites_base$stratum == "USA", , drop = FALSE]),
    n_base[["USA"]]
  )
  expect_equal(NROW(grts_output$sites_base), sum(n_base))
  expect_equal(NROW(grts_output$sites_over), 0)
  expect_equal(NROW(grts_output$sites_near), 0)
  expect_equal(NCOL(grts_output$sites_legacy), 1)
  expect_equal(NCOL(grts_output$sites_base), col_out)
  expect_equal(NCOL(grts_output$sites_over), 1)
  expect_equal(NCOL(grts_output$sites_near), 1)
})
