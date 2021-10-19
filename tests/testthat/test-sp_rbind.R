context("sp_rbind")

# find system info
on_solaris <- Sys.info()[["sysname"]] == "SunOS"
if (on_solaris) {
  test_that("on solaris", {
    expect_true(on_solaris)
  })
} else {

  # set reproducible seed (as there are random components here)
  set.seed(5)

  test_that("sp_rbind works base", {
    n_base <- 50
    eqprob <- grts(NE_Lakes, n_base)
    eqprob_spr <- sp_rbind(eqprob)
    expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
    expect_equal(NROW(eqprob_spr), n_base)
  })

  test_that("sp_rbind works legacy", {
    n_base <- 50
    n_legacy <- sum(!is.na(NE_Lakes$LEGACY))
    eqprob <- grts(NE_Lakes, n_base = n_base, legacy_var = "LEGACY")
    eqprob_spr <- sp_rbind(eqprob)
    expect_equal(sum(eqprob_spr$siteuse == "Legacy"), n_legacy)
    expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base - n_legacy)
    expect_equal(NROW(eqprob_spr), n_base)
  })

  test_that("sp_rbind works over", {
    n_base <- 50
    n_over <- 5
    eqprob <- grts(NE_Lakes, n_base = n_base, n_over = n_over)
    eqprob_spr <- sp_rbind(eqprob)
    expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
    expect_equal(sum(eqprob_spr$siteuse == "Over"), n_over)
    expect_equal(NROW(eqprob_spr), n_base + n_over)
  })

  test_that("sp_rbind works near", {
    n_base <- 50
    n_near <- 5
    eqprob <- grts(NE_Lakes, n_base = n_base, n_near = n_near)
    eqprob_spr <- sp_rbind(eqprob)
    expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
    expect_equal(sum(substring(eqprob_spr$siteuse, 1, 4) == "Near"), n_base * n_near)
    expect_equal(NROW(eqprob_spr), n_base + n_base * n_near)
  })

  test_that("sp_rbind works with siteuse", {
    n_base <- 50
    n_near <- 5
    eqprob <- grts(NE_Lakes, n_base = n_base, n_near = n_near)
    eqprob_spr <- sp_rbind(eqprob, siteuse = "Base")
    expect_equal(sum(eqprob_spr$siteuse == "Base"), n_base)
    expect_equal(NROW(eqprob_spr), n_base)
  })
}
