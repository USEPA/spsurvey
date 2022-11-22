context("sp_summary")

# find system info
on_solaris <- Sys.info()[["sysname"]] == "SunOS"
if (on_solaris) {
  test_that("on solaris", {
    expect_true(on_solaris)
  })
} else {


  # set reproducible seed (as there are random components here)
  set.seed(5)

  #--------------------------------------
  #-------- one sided formula
  #--------------------------------------

  # intercept only formula
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~1)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 1)
    expect_equal(length(output[, 1]), 1)
  })

  # single categorical variable
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ELEV_CAT)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), length(unique(NE_Lakes$ELEV_CAT)))
  })

  # single categorical variable removing intercept
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ ELEV_CAT - 1)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 1)
    expect_equal(length(output[, 1]), length(unique(NE_Lakes$ELEV_CAT)))
  })

  # two categorical variables
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ ELEV_CAT + AREA_CAT)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 3)
    expect_equal(length(output[, 2]), length(unique(NE_Lakes$ELEV_CAT)))
    expect_equal(length(output[, 3]), length(unique(NE_Lakes$AREA_CAT)))
  })

  # interaction between two categorical variables
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), length(unique(NE_Lakes$ELEV_CAT)) * length(unique(NE_Lakes$AREA_CAT)))
  })

  # onlyshow for interaction between two categorical variables
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT, onlyshow = "low:small")
    expect_true(exists("output"))
    expect_equal(NCOL(output), 1)
    expect_equal(length(output[, 1]), 1)
  })

  # single categorical variable
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ELEV)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 6) # 5 number summary plus mean
  })

  # * interaction operator works
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~ ELEV_CAT * AREA_CAT)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 4)
  })

  # . interaction operator works
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~.)
    expect_true(exists("output"))
    expect_equal(NCOL(output), NCOL(NE_Lakes) + 1) # our summary adds an intercept
  })

  # maxsum works operator works
  test_that("one sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = ~AREA_CAT, maxsum = 1)
    expect_true(exists("output"))
    expect_equal(length(output[, 2]), 1)

    output <- sp_summary(NE_Lakes, formula = ~AREA_CAT)
    expect_true(exists("output"))
    expect_equal(length(output[, 2]), 2)
  })

  #--------------------------------------
  #-------- two sided formula
  #--------------------------------------

  # numeric left hand side variable
  test_that("two sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = AREA ~ ELEV_CAT)
    expect_true(exists("output"))
    expect_equal(NROW(output), 2)
    expect_equal(NCOL(output[[1]]), 6)
    expect_equal(NROW(output[[1]]), 1)
    expect_equal(NCOL(output[[2]]), 6)
    expect_equal(NROW(output[[2]]), length(unique(NE_Lakes$ELEV_CAT)))
  })

  # numeric right hand side variable
  test_that("two sided formulas work", {
    output <- sp_summary(NE_Lakes, formula = AREA_CAT ~ ELEV_CAT)
    expect_true(exists("output"))
    expect_equal(NROW(output), 2)
    expect_equal(NCOL(output[[1]]), length(unique(NE_Lakes$AREA_CAT)))
    expect_equal(NROW(output[[1]]), 1)
    expect_equal(NCOL(output[[2]]), length(unique(NE_Lakes$AREA_CAT)))
    expect_equal(NROW(output[[2]]), length(unique(NE_Lakes$ELEV_CAT)))
  })

  #################################################
  ########### sp_design
  #################################################

  n_base <- 50
  eqprob <- grts(NE_Lakes, n_base = n_base)
  eqprob_legacy <- grts(NE_Lakes, n_base = n_base, legacy_sites = NE_Lakes_Legacy)
  n_over <- 5
  eqprob_rho <- grts(NE_Lakes, n_base = n_base, n_over = n_over)
  n_near <- 1
  eqprob_nn <- grts(NE_Lakes, n_base = n_base, n_near = n_near)
  eqprob_both <- grts(NE_Lakes, n_base = n_base, n_over = n_over, n_near = n_near)
  n_base_strat <- c(low = 25, high = 25)
  eqprob_strat <- grts(NE_Lakes, n_base = n_base_strat, stratum_var = "ELEV_CAT")

  #--------------------------------------
  #-------- one sided formula
  #--------------------------------------

  # one sided formula
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  # one sided formula with additional variable
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob, formula = ~ siteuse + ELEV_CAT)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 3)
    expect_equal(sum(!is.na(output[, 2])), 1)
  })

  # use with legacy variable
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_legacy, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_legacy, formula = ~siteuse, siteuse = "Base")
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  # use with rho replacement
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_rho, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_rho, formula = ~siteuse, siteuse = "Base")
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  # use with nn replacement
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_nn, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_nn, formula = ~siteuse, siteuse = "Base")
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  # use with both replacement
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_both, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 3)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_both, formula = ~siteuse, siteuse = "Base")
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_both, formula = ~siteuse, siteuse = c("Base", "Over"))
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_both, formula = ~siteuse, siteuse = c("Base", "Near"))
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # siteuse variable being set
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_both, formula = ~siteuse, siteuse = c("Over", "Near"))
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 2)
  })

  # with a stratified design
  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_strat, formula = ~siteuse)
    expect_true(exists("output"))
    expect_equal(NCOL(output), 2)
    expect_equal(length(output[, 2]), 1)
  })

  #--------------------------------------
  #-------- two sided formula
  #--------------------------------------

  test_that("one sided formulas work", {
    output <- sp_summary(eqprob_strat, formula = siteuse ~ stratum)
    expect_true(exists("output"))
    expect_equal(NROW(output), 2)
    expect_equal(NCOL(output[[1]]), 1)
    expect_equal(NROW(output[[1]]), 1)
    expect_equal(NCOL(output[[2]]), 1)
    expect_equal(NROW(output[[2]]), length(unique(eqprob_strat$sites_base$stratum)))
  })
}
