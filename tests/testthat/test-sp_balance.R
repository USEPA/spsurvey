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
}
