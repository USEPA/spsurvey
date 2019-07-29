context("create_frame_objects")

data("NE_lakes","Luck_Ash_streams", "UT_ecoregions")

test_that("finite test frame exists as sf object with correct geometry type",{
  expect_is(NE_lakes,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(NE_lakes))$class, c("sfc_POINT", "sfc" ))
})

test_that("linear test frame exists as sf object with correct geometry type",{
  expect_is(Luck_Ash_streams,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(Luck_Ash_streams))$class, c("sfc_LINESTRING", "sfc" ))
})

test_that("area test frame exists as sf object with correct geometry type",{
  expect_is(UT_ecoregions,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(UT_ecoregions))$class, c("sfc_MULTIPOLYGON", "sfc" ))
})