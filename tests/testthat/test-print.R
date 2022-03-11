context("print")

# find system info
on_solaris <- Sys.info()[["sysname"]] == "SunOS"
if (on_solaris) {
  test_that("on solaris", {
    expect_true(on_solaris)
  })
} else {
  # sample frame
  expect_output(print(sp_summary(NE_Lakes, ~ ELEV_CAT)))
  expect_output(print(sp_summary(NE_Lakes, AREA_CAT ~ ELEV_CAT)))
  expect_output(print(summary(sp_frame(NE_Lakes), ~ ELEV_CAT)))
  expect_output(print(summary(sp_frame(NE_Lakes), AREA_CAT ~ ELEV_CAT)))
  
  # design frame
  expect_output(print(sp_summary(NLA_PNW, ~ BMMI_COND)))
  expect_output(print(sp_summary(NLA_PNW, PHOS_COND ~ BMMI_COND)))
  expect_output(print(summary(sp_frame(NLA_PNW), ~ BMMI_COND)))
  expect_output(print(summary(sp_frame(NLA_PNW), PHOS_COND ~ BMMI_COND)))
  
  # grts output
  grts_output <- grts(NE_Lakes, 50)
  expect_output(print(sp_summary(grts_output, ~ ELEV_CAT)))
  expect_output(print(sp_summary(grts_output, AREA_CAT ~ ELEV_CAT)))
  expect_output(print(summary(grts_output, ~ ELEV_CAT)))
  expect_output(print(summary(grts_output, AREA_CAT ~ ELEV_CAT)))  
  expect_output(print(grts_output))
  
  # irs output
  irs_output <- irs(NE_Lakes, 50)
  expect_output(print(sp_summary(irs_output, ~ ELEV_CAT)))
  expect_output(print(sp_summary(irs_output, AREA_CAT ~ ELEV_CAT)))
  expect_output(print(summary(irs_output, ~ ELEV_CAT)))
  expect_output(print(summary(irs_output, AREA_CAT ~ ELEV_CAT)))  
  expect_output(print(irs_output))
}