context("Test that package data has the appropriate structure")

data("NE_Lakes")
test_that("NE_Lakes (point design frame) is an sf object with the correct geometry type", {
  expect_s3_class(NE_Lakes, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NE_Lakes$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

data("Illinois_River")
test_that("Illinois_River (linear design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Illinois_River, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Illinois_River$geometry, c("sfc_MULTILINESTRING", "sfc"), exact = TRUE)
  expect_equal(st_crs(Illinois_River)$input, "EPSG:5070")
})

data("Illinois_River_Legacy")
test_that("Illinois_River_Legacy (linear legacy design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Illinois_River_Legacy, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Illinois_River_Legacy$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(Illinois_River_Legacy)$input, "EPSG:5070")
})

data("Lake_Ontario")
test_that("Lake Ontario (point design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Lake_Ontario, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Lake_Ontario$geometry, c("sfc_MULTIPOLYGON", "sfc"), exact = TRUE)
  expect_equal(st_crs(Lake_Ontario)$input, "EPSG:5070")
})

data("NLA_PNW")
test_that("NLA_PNW (point analysis frame) is an sf object with the correct geometry type", {
  expect_s3_class(NLA_PNW, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NLA_PNW$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NLA_PNW)$input, "EPSG:5070")
})

data("NRSA_EPA7")
test_that("NRSA_EPA7 (linear design frame) is an sf object with the correct geometry type", {
  expect_s3_class(NRSA_EPA7, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NRSA_EPA7$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NRSA_EPA7)$input, "EPSG:5070")
})
