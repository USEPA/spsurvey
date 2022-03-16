context("cdf_plot")

# find system info
on_solaris <- Sys.info()[["sysname"]] == "SunOS"
if (on_solaris) {
  test_that("on solaris", {
    expect_true(on_solaris)
  })
} else {

  #################################################
  ########### cdf_plot
  #################################################
  
  test_that("cdf_plot works", {
    bmmi <- cont_analysis(
      NLA_PNW,
      vars = "BMMI",
      weight = "WEIGHT",
      siteID = "SITE_ID"
    )
    expect_error(cdf_plot(bmmi$CDF), NA)
  })
  
  test_that("cdf_plot works subpop", {
    bmmi <- cont_analysis(
      NLA_PNW,
      vars = "BMMI",
      subpop = "PHOS_COND",
      weight = "WEIGHT",
      siteID = "SITE_ID"
    )
    expect_error(cdf_plot(bmmi$CDF, subpop = "PHOS_COND", subpop_level = "Good"), NA)
  })
  
  test_that("cdf_plot works variables", {
    bmmi <- cont_analysis(
      NLA_PNW,
      vars = c("BMMI", "WEIGHT"),
      weight = "WEIGHT",
      siteID = "SITE_ID"
    )
    expect_error(cdf_plot(bmmi$CDF, var = "BMMI"), NA)
  })
  
  test_that("cdf_plot works variables subpop", {
    bmmi <- cont_analysis(
      NLA_PNW,
      vars = c("BMMI", "WEIGHT"),
      subpop = "PHOS_COND",
      weight = "WEIGHT",
      siteID = "SITE_ID"
    )
    expect_error(cdf_plot(bmmi$CDF, var = "BMMI", subpop = "PHOS_COND", subpop_level = "Good"), NA)
  })
  
}