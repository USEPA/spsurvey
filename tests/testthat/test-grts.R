context("grts")

set.seed(52468110)
# Create sp objects
sp.finite <- read.shape(system.file("extdata", "reg1_lakes.shp", package="spsurvey"))
sp.finite@data$mdcaty <- runif(nrow(sp.finite))

testsample <- grts(design=list(None=list(panel=c(PanelOne=10), 
                                         over=0,seltype="Equal")),
                   type.frame="finite", in.shape=system.file("extdata", "reg1_lakes.shp", package="spsurvey"),
                   shapefile=TRUE)

test_that("test finite design equal random selection using shapefile and output shapefile",{
  expect_true(file.exists('sample.shp'))
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), 
                   type.frame="finite", src.frame="att.frame",
                   att.frame=sp.finite, xcoord="xcoord", ycoord="ycoord", shapefile=FALSE)

test_that("test equal random selection using an attribute frame and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})
