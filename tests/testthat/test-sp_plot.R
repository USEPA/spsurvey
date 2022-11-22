context("sp_plot")

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

  # one sided formulas
  test_that("one sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ~1), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, fix_bbox = FALSE), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV), NA)
  })

  # changing graphical parameters
  test_that("one sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, pch = 19), NA)
    var_args <- list(ELEV_CAT = list(main = "maintest"))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, var_args = var_args), NA)
    varlevel_args <- list(ELEV_CAT = list(levels = c("low", "high"), cex = c(1, NA)))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, varlevel_args = varlevel_args), NA)
    varlevel_args <- list(ELEV_CAT = list(levels = c("low", "high"), cex = c(1, 2)))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, varlevel_args = varlevel_args), NA)
    expect_error(sp_plot(NE_Lakes,
      formula = ~ELEV_CAT,
      varlevel_args = varlevel_args, var_args = var_args, pch = 19
    ), NA)
  })


  #--------------------------------------
  #-------- two sided formula
  #--------------------------------------

  # two sided formulas
  test_that("two sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ELEV ~ 1), NA)
    expect_error(sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT, onlyshow = "small"), NA)
  })

  # changing graphical parameters
  test_that("two sided formulas work", {
    var_args <- list(AREA_CAT = list(ELEV_CAT = list(levels = c("low", "high"), pch = c(1, 19))))
    expect_error(sp_plot(NE_Lakes, formula = ELEV_CAT ~ AREA_CAT, var_args = var_args, onlyshow = "large"), NA)
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
  #-------- without sframe
  #--------------------------------------

  # test sp_plot works
  test_that("sp_plot works", {
    expect_error(sp_plot(eqprob, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_legacy, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_rho, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_nn, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_nn, formula = ~siteuse, siteuse = c("Base")), NA)
    expect_error(sp_plot(eqprob_strat, formula = siteuse ~ stratum, onlyshow = "low"), NA)
  })

  # graphical paraemters
  test_that("sp_plot works", {
    var_args <- list(siteuse = list(main = "maintest"))
    varlevel_args <- list(siteuse = list(levels = c("Base", "Over"), pch = c(1, 19)))
    expect_error(sp_plot(eqprob_rho,
      formula = ~siteuse,
      varlevel_args = varlevel_args, var_args = var_args, cex = 0.5
    ), NA)
    var_args <- list(ELEV_CAT = list(siteuse = list(levels = c("Base", "Over"), pch = c(1, 19))))
    expect_error(sp_plot(eqprob_rho,
      formula = siteuse ~ ELEV_CAT,
      var_args = var_args, onlyshow = "low", cex = 0.5
    ), NA)
  })

  #--------------------------------------
  #-------- with sframe
  #--------------------------------------

  # test sp_plot works
  test_that("sp_plot works", {
    expect_error(sp_plot(eqprob, NE_Lakes, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_legacy, NE_Lakes, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_rho, NE_Lakes, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_nn, NE_Lakes, formula = ~siteuse), NA)
    expect_error(sp_plot(eqprob_nn, NE_Lakes, formula = ~siteuse, siteuse = c("Base")), NA)
    expect_error(sp_plot(eqprob_strat, NE_Lakes, formula = siteuse ~ ELEV_CAT, onlyshow = "low"), NA)
  })

  # graphical parameters
  test_that("sp_plot works", {
    var_args <- list(siteuse = list(main = "maintest"))
    varlevel_args <- list(siteuse = list(levels = c("Base", "Over"), pch = c(1, 19)))
    expect_error(sp_plot(eqprob_rho, NE_Lakes,
      formula = ~siteuse,
      varlevel_args = varlevel_args, var_args = var_args, cex = 0.5
    ), NA)
    var_args <- list(ELEV_CAT = list(siteuse = list(levels = c("Base"), pch = c(19))))
    expect_error(sp_plot(eqprob_rho, NE_Lakes,
      formula = siteuse ~ ELEV_CAT,
      var_args = var_args, onlyshow = "low", siteuse = c("Base"), cex = 1
    ), NA)
  })

  #--------------------------------------
  #-------- one sided formula
  #--------------------------------------

  # one sided formulas
  test_that("one sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ~1), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, fix_bbox = FALSE), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ ELEV_CAT:AREA_CAT), NA)
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV), NA)
  })

  # changing graphical parameters
  test_that("one sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, pch = 19), NA)
    var_args <- list(ELEV_CAT = list(main = "maintest"))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, var_args = var_args), NA)
    varlevel_args <- list(ELEV_CAT = list(levels = c("low", "high"), cex = c(1, NA)))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, varlevel_args = varlevel_args), NA)
    varlevel_args <- list(ELEV_CAT = list(levels = c("low", "high"), cex = c(1, 2)))
    expect_error(sp_plot(NE_Lakes, formula = ~ELEV_CAT, varlevel_args = varlevel_args), NA)
    expect_error(sp_plot(NE_Lakes,
      formula = ~ELEV_CAT,
      varlevel_args = varlevel_args, var_args = var_args, pch = 19
    ), NA)
  })


  #--------------------------------------
  #-------- two sided formula
  #--------------------------------------

  # two sided formulas
  test_that("two sided formulas work", {
    expect_error(sp_plot(NE_Lakes, formula = ELEV ~ 1), NA)
    expect_error(sp_plot(NE_Lakes, formula = ELEV ~ AREA_CAT, onlyshow = "small"), NA)
  })

  # changing graphical parameters
  test_that("two sided formulas work", {
    var_args <- list(AREA_CAT = list(ELEV_CAT = list(levels = c("low", "high"), pch = c(1, 19))))
    expect_error(sp_plot(NE_Lakes, formula = ELEV_CAT ~ AREA_CAT, var_args = var_args, onlyshow = "large"), NA)
  })
}
