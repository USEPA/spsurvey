context("grts")

data("NE_lakes","Luck_Ash_streams", "UT_ecoregions")

set.seed(52468110)

testsample <- grts(design=list(None=list(panel=c(PanelOne=10), 
                                         over=0,seltype="Equal")),
                   src.frame="sf.object", type.frame="finite", sf.object=NE_lakes,shapefile=TRUE)

test_that("test finite grts design with output shapefile",{
  expect_true(file.exists('sample.shp'))
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
})