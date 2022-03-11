test_that("sp_frame works", {
  NE_Lakes <- sp_frame(NE_Lakes)
  expect_s3_class(NE_Lakes, "sp_frame")
  NE_Lakes <- sp_unframe(NE_Lakes)
  expect_true(!inherits(NE_Lakes, "sp_frame"))
})
