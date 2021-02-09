context("create_frame_objects")

data("NE_lakes","Great_Lakes", "Illinois_River", "NCCA_Gulf","NLA_West","NRSA_EPA7")

test_that("finite test frame exists as sf object with correct geometry type",{
  expect_is(NE_Lakes,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(NE_Lakes))$class, c("sfc_POINT", "sfc" ))
})

test_that("linear test frame exists as sf object with correct geometry type",{
  expect_is(Illinois_River,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(Illinois_River))$class, c("sfc_MULTILINESTRING", "sfc" ))
})

test_that("area test frame exists as sf object with correct geometry type",{
  expect_is(Great_Lakes,c("sf","data.frame"))
  expect_equal(attributes(st_geometry(Great_Lakes))$class, c("sfc_MULTIPOLYGON", "sfc" ))
})

test_that("data frame exists",{
  expect_is(NCCA_Gulf,c("data.frame"))
})

