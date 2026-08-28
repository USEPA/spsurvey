context("sp_balance")

# find system info
on_solaris <- Sys.info()[["sysname"]] == "SunOS"
if (on_solaris) {
  test_that("on solaris", {
    expect_true(on_solaris)
  })
} else {

  # set reproducible seed (as there are random components here)
  set.seed(5)

  test_that("sp_balance works unstrat", {
    n_base <- 50
    eqprob <- grts(NE_Lakes, n_base = n_base)
    spb_eqprob <- sp_balance(eqprob$sites_base, NE_Lakes)
    expect_equal(NROW(spb_eqprob), 1)
    expect_equal(NCOL(spb_eqprob), 3)
    spb_eqprob <- sp_balance(eqprob$sites_base, NE_Lakes,
      metrics = c("pielou", "simpsons", "rmse", "mse", "mae", "medae", "chisq")
    )
    expect_equal(NROW(spb_eqprob), 7)
    expect_equal(NCOL(spb_eqprob), 3)
  })

  test_that("sp_balance works unstrat custom ip", {
    n_base <- 50
    NE_Lakes$ip <- n_base / nrow(NE_Lakes) + rnorm(NROW(NE_Lakes), sd = 0.01)
    NE_Lakes$ip <- pmin(NE_Lakes$ip, 1)
    NE_Lakes$ip <- pmax(NE_Lakes$ip, 0)
    eqprob <- grts(NE_Lakes, n_base = n_base)
    spb_eqprob <- sp_balance(eqprob$sites_base, NE_Lakes, ip = "ip")
    expect_equal(NROW(spb_eqprob), 1)
    expect_equal(NCOL(spb_eqprob), 3)
    spb_eqprob <- sp_balance(eqprob$sites_base, NE_Lakes,
      metrics = c("pielou", "simpsons", "rmse", "mse", "mae", "medae", "chisq"),
      ip = "ip"
    )
    expect_equal(NROW(spb_eqprob), 7)
    expect_equal(NCOL(spb_eqprob), 3)
  })

  all7 <- c("pielou", "simpsons", "mse", "rmse", "mae", "medae", "chisq")

  test_that("sp_balance() is generic and grts()/irs() compute it eagerly", {
    set.seed(5)
    d <- grts(NE_Lakes, n_base = 50)
    expect_s3_class(d, "sp_design")
    expect_type(d$sp_balance, "list")
    expect_named(d$sp_balance, c("metrics", "extents"))

    out <- sp_balance(d)
    expect_s3_class(out, "data.frame")
    expect_named(out, c("stratum", "metric", "value"))
    expect_equal(NROW(out), 1)
    expect_equal(out$metric, "pielou")

    set.seed(5)
    d_irs <- irs(NE_Lakes, n_base = 50)
    expect_type(d_irs$sp_balance, "list")
    expect_s3_class(sp_balance(d_irs), "data.frame")
  })

  test_that("sp_balance.sp_design() metrics argument subsets the stored table", {
    set.seed(5)
    d <- grts(NE_Lakes, n_base = 50)

    out_all <- sp_balance(d, metrics = all7)
    expect_equal(NROW(out_all), 7)
    expect_setequal(out_all$metric, all7)

    out_two <- sp_balance(d, metrics = c("pielou", "rmse"))
    expect_equal(NROW(out_two), 2)
    expect_setequal(out_two$metric, c("pielou", "rmse"))

    expect_error(sp_balance(d, metrics = "not_a_metric"), "invalid metric")
  })

  test_that("sp_balance.sp_design() extents argument matches sp_balance.default()'s structure", {
    set.seed(5)
    d <- grts(NE_Lakes, n_base = 50)

    out <- sp_balance(d, extents = TRUE)
    expect_named(out, c("metrics", "extents"))
    expect_s3_class(out$extents, "sf")
    expect_named(sf::st_drop_geometry(out$extents), c("stratum", "extent"))
    expect_equal(NROW(out$extents), 50)
  })

  test_that("sp_balance = FALSE skips the computation and should error", {
    set.seed(5)
    d <- grts(NE_Lakes, n_base = 50, sp_balance = FALSE)
    expect_null(d$sp_balance)
    expect_error(sp_balance(d), "sp_balance")
  })

  test_that("sp_balance.default() errors informatively for a design object to sp_balance.default", {
    set.seed(5)
    d <- grts(NE_Lakes, n_base = 50)
    expect_error(sp_balance.default(d, NE_Lakes), "sp_balance\\(\\) directly")
  })

  test_that("spatial balance handles legacy sites and oversamples", {
    set.seed(8)
    leg <- NE_Lakes[1:6, ]
    leg_geom <- sf::st_geometry(leg) + c(50, 50)
    sf::st_geometry(leg) <- leg_geom
    sf::st_crs(leg) <- sf::st_crs(NE_Lakes)
    d_leg <- grts(NE_Lakes, n_base = 30, legacy_sites = leg)
    expect_type(d_leg$sp_balance, "list")
    expect_equal(NROW(sp_balance(d_leg, extents = TRUE)$extents), nrow(d_leg$sites_legacy) + nrow(d_leg$sites_base))

    set.seed(9)
    d_over <- grts(NE_Lakes, n_base = 30, n_over = 10)
    expect_type(d_over$sp_balance, "list")
    # the site set for balance is legacy + base only, not base + over
    expect_equal(NROW(sp_balance(d_over, extents = TRUE)$extents), nrow(d_over$sites_base))
  })

  test_that("spatial balance works for linear and areal frames", {
    set.seed(10)
    d_line <- grts(Illinois_River, n_base = 20)
    expect_type(d_line$sp_balance, "list")
    expect_s3_class(sp_balance(d_line), "data.frame")

    set.seed(11)
    d_area <- grts(Lake_Ontario, n_base = 30)
    expect_type(d_area$sp_balance, "list")
    expect_s3_class(sp_balance(d_area), "data.frame")

    set.seed(12)
    d_area_unequal <- grts(Lake_Ontario,
      n_base = 30, caty_var = "RSRC_CLASS",
      caty_n = round(table(Lake_Ontario$RSRC_CLASS) / sum(table(Lake_Ontario$RSRC_CLASS)) * 30)
    )
    expect_type(d_area_unequal$sp_balance, "list")

    set.seed(13)
    d_area_prop <- grts(Lake_Ontario, n_base = 20, aux_var = "AREA_SQKM")
    expect_type(d_area_prop$sp_balance, "list")
  })
}
