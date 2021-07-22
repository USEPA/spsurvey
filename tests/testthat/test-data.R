context("Test that package data has the appropriate structure")

data(
  "NE_Lakes",
  "Illinois_River",
  "Illinois_River_Legacy",
  "Great_Lakes",
  "NLA_PNW",
  "NRSA_EPA7",
  "NCCA_Gulf"
)

test_that("NE_Lakes (point design frame) is an sf object with the correct geometry type", {
  expect_s3_class(NE_Lakes, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NE_Lakes$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

test_that("Illinois_River (linear design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Illinois_River, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Illinois_River$geometry, c("sfc_MULTILINESTRING", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

test_that("Illinois_River_Legacy (linear legacy design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Illinois_River_Legacy, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Illinois_River_Legacy$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

test_that("Great_Lakes (point design frame) is an sf object with the correct geometry type", {
  expect_s3_class(Great_Lakes, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(Great_Lakes$geometry, c("sfc_MULTIPOLYGON", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

test_that("NLA_PNW (point analysis frame) is an sf object with the correct geometry type", {
  expect_s3_class(NLA_PNW, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NLA_PNW$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NE_Lakes)$input, "EPSG:5070")
})

test_that("NRSA_EPA7 (linear design frame) is an sf object with the correct geometry type", {
  expect_s3_class(NRSA_EPA7, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NRSA_EPA7$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NRSA_EPA7)$input, "EPSG:5070")
})

test_that("NCCA_Gulf (areal design frame) is an sf object with the correct geometry type", {
  expect_s3_class(NCCA_Gulf, c("sf", "data.frame"), exact = TRUE)
  expect_s3_class(NCCA_Gulf$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
  expect_equal(st_crs(NCCA_Gulf)$input, "EPSG:5070")
})
