context("grts")

set.seed(52468110)
# Create sp objects
sp.finite <- read.shape(system.file("extdata", "reg1_lakes.shp", package="spsurvey"))
sp.finite$mdcaty <- runif(nrow(sp.finite))

# Finite: reg1_lakes point shapefile with equal random selection using a shapefile frame:
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

# Finite: reg1_lakes point shapefile with equal random selection using an attribute frame and no output shapefile:
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), 
                   type.frame="finite", src.frame="att.frame",
                   att.frame=sp.finite, xcoord="X_COORD", ycoord="Y_COORD", shapefile=FALSE)

test_that("test equal random selection using an attribute frame and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Finite: reg1_lakes point shapefile with equal random selection using sp object frame and no output shapefile:
sp.finite <- as(sp.finite, 'Spatial')
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), 
                   type.frame="finite", src.frame="sp.object",
                   sp.object=sp.finite, shapefile=FALSE)

test_that("test equal random selection using an attribute frame and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Finite: NE_lakes with equal random selection using sf object frame and no output shapefile:
data("NE_lakes")
sf.finite <- NE_lakes
sf.finite$mdcaty <- runif(nrow(sf.finite))
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), 
                   type.frame="finite", src.frame="sf.object",
                   sf.object=sf.finite, shapefile=FALSE)

test_that("test equal random selection using an sf frame and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})
