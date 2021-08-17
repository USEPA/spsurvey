context("test behavior of some helper functions")

# set reproducible seed (as there are random components here)
set.seed(5)

#################################################
########### sframe
#################################################

test_that("sframe gives appropriate class", {
  NE_Lakes <- sframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("sframe", "sf", "data.frame"))
  NE_Lakes <- sframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("sframe", "sf", "data.frame"))
  class(NE_Lakes) <- class(NE_Lakes)[c(2, 3, 1)]
  NE_Lakes <- sframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("sframe", "sf", "data.frame"))
})

#################################################
########### dframe
#################################################

test_that("dframe gives appropriate class", {
  NE_Lakes <- dframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("dframe", "sf", "data.frame"))
  NE_Lakes <- dframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("dframe", "sf", "data.frame"))
  class(NE_Lakes) <- class(NE_Lakes)[c(2, 3, 1)]
  NE_Lakes <- dframe(NE_Lakes)
  expect_equal(class(NE_Lakes), c("dframe", "sf", "data.frame"))
})

#################################################
########### sframe_to_sf
#################################################

test_that("sframe to sf works", {
  expect_error(sframe_to_sf(NE_Lakes))
  NE_Lakes <- sframe(NE_Lakes)
  NE_Lakes <- sframe_to_sf(NE_Lakes)
  expect_equal(class(NE_Lakes), c("sf", "data.frame"))
})

#################################################
########### dframe_to_sf
#################################################

test_that("dframe to sf works", {
  expect_error(dframe_to_sf(NE_Lakes))
  NE_Lakes <- dframe(NE_Lakes)
  NE_Lakes <- dframe_to_sf(NE_Lakes)
  expect_equal(class(NE_Lakes), c("sf", "data.frame"))
})

#################################################
########### dframe_to_df
#################################################

test_that("dframe to df works", {
  expect_error(dframe_to_df(NE_Lakes))
  NE_Lakes <- dframe(NE_Lakes)
  NE_Lakes <- dframe_to_df(NE_Lakes)
  expect_equal(class(NE_Lakes), c("data.frame"))
})

#################################################
########### sprbind
#################################################

test_that("sprbind works base", {
  n_base <- 50
  eqprob <- grts(NE_Lakes, n_base)
  eqprob_spr <- sprbind(eqprob)
  expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
  expect_equal(NROW(eqprob_spr), n_base)
})

test_that("sprbind works legacy", {
  n_base <- 50
  n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
  eqprob <- grts(NE_Lakes, n_base = n_base, legacy_var = "LEGACY")
  eqprob_spr <- sprbind(eqprob)
  expect_equal(sum(eqprob_spr$siteuse == "Legacy"), n_legacy)
  expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base - n_legacy)
  expect_equal(NROW(eqprob_spr), n_base)
})

test_that("sprbind works over", {
  n_base <- 50
  n_over <- 5
  eqprob <- grts(NE_Lakes, n_base = n_base, n_over = n_over)
  eqprob_spr <- sprbind(eqprob)
  expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
  expect_equal(sum(eqprob_spr$siteuse == "Over"), n_over)
  expect_equal(NROW(eqprob_spr), n_base + n_over)
})

test_that("sprbind works near", {
  n_base <- 50
  n_near <- 5
  eqprob <- grts(NE_Lakes, n_base = n_base, n_near = n_near)
  eqprob_spr <- sprbind(eqprob)
  expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
  expect_equal(sum(substring(eqprob_spr$siteuse, 1, 4) == "Near"), n_base * n_near)
  expect_equal(NROW(eqprob_spr), n_base + n_base * n_near)
})

test_that("sprbind works with siteuse", {
  n_base <- 50
  n_near <- 5
  eqprob <- grts(NE_Lakes, n_base = n_base, n_near = n_near)
  eqprob_spr <- sprbind(eqprob, siteuse = "Base")
  expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
  expect_equal(NROW(eqprob_spr), n_base)
})


#################################################
########### spbalance
#################################################

test_that("spbalance works unstrat", {
  n_base <- 50
  eqprob <- grts(NE_Lakes, n_base = n_base)
  spb_eqprob <- spbalance(eqprob$sites_base, NE_Lakes)
  expect_equal(NROW(spb_eqprob), 1)
  expect_equal(NCOL(spb_eqprob), 3)
  spb_eqprob <- spbalance(eqprob$sites_base, NE_Lakes,
    metrics = c("pielou", "simpsons", "rmse", "mse", "mae", "medae", "chisq")
  )
  expect_equal(NROW(spb_eqprob), 7)
  expect_equal(NCOL(spb_eqprob), 3)
})

test_that("spbalance works unstrat custom ip", {
  n_base <- 50
  NE_Lakes$ip <- n_base / nrow(NE_Lakes) + rnorm(NROW(NE_Lakes), sd = 0.01)
  NE_Lakes$ip <- pmin(NE_Lakes$ip, 1)
  NE_Lakes$ip <- pmax(NE_Lakes$ip, 0)
  eqprob <- grts(NE_Lakes, n_base = n_base)
  spb_eqprob <- spbalance(eqprob$sites_base, NE_Lakes, ip = "ip")
  expect_equal(NROW(spb_eqprob), 1)
  expect_equal(NCOL(spb_eqprob), 3)
  spb_eqprob <- spbalance(eqprob$sites_base, NE_Lakes,
    metrics = c("pielou", "simpsons", "rmse", "mse", "mae", "medae", "chisq"),
    ip = "ip"
  )
  expect_equal(NROW(spb_eqprob), 7)
  expect_equal(NCOL(spb_eqprob), 3)
})
