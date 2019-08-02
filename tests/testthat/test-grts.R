context("grts")

set.seed(52468110)
# Create sp object
load(system.file("extdata", "reg1_lakes.rda", package="spsurvey"))
st_write(reg1_lakes, "reg1_lakes.shp", quiet = TRUE, delete_dsn = TRUE)
sp.finite <- read.shape("./reg1_lakes.shp")
sp.finite$mdcaty <- runif(nrow(sp.finite))

# Finite: reg1_lakes point shapefile with equal random selection using a shapefile frame:
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), 
                                         over=0,seltype="Equal")),
                   type.frame="finite", in.shape="./reg1_lakes.shp",
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

# Finite: reg1_lakes point shapefile with unequal random selection using a shapefile frame:
testsample <- grts(design=list("LAKE/POND"=list(panel=c(PanelOne=10), over=6,
                                                caty.n=c("5"=5, "8"=5), seltype="Unequal"),
                               "RESERVOIR"=list(panel=c(PanelOne=10), over=0, caty.n=c("5"=5, "8"=5),
                                                seltype="Unequal")), type.frame="finite",
                   in.shape="./reg1_lakes.shp",
                   stratum="FTYPE", mdcaty="LEVEL1", shapefile=FALSE)

test_that("test unequal random selection using shapefile and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),26)
})

# Finite: continuous random selection using an sf object frame:
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Continuous")), type.frame="finite", src.frame="sf.object",
                   sf.object=sf.finite, mdcaty="mdcaty", shapefile=FALSE)

test_that("test unequal random selection using shapefile and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Finite: NHDPoint PointZ shapefile:
load(system.file("extdata", "NHDPoint.rda", package="spsurvey"))
NHDPoint <- st_zm(NHDPoint, drop=TRUE)
st_write(NHDPoint, "NHDPoint.shp", quiet = TRUE, delete_dsn = TRUE)

testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), type.frame="finite",
                   in.shape="./NHDPoint.shp",
                   shapefile=FALSE)
test_that("test equal random selection using NHDPointZ shapefile and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Linear: fp_len Polyline shapefile no output shapefile:
load(system.file("extdata", "fp_len.rda", package="spsurvey"))
st_write(fp_len, "fp_len.shp", quiet = TRUE, delete_dsn = TRUE)
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), type.frame="linear",
                   in.shape="./fp_len.shp",
                   shapefile=FALSE)
test_that("test equal random selection using fp_len linear shapefile and no output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Linear: ryan_len Polyline shapefile:
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), type.frame="linear",
                   in.shape="./fp_len.shp",
                   shapefile=TRUE)
test_that("test equal random selection using ryan_len linear shapefile and output shapefile",{
  expect_true(exists("testsample"))
  expect_equal(attributes(testsample)$class[1],"SpatialDesign")
  expect_equal(nrow(testsample@data),10)
})

# Linear: ryan_len Polyline shapefile with output shapefile:
sp.linear <- read.shape("./fp_len.shp")
sp.linear$stratum <- c("A", rep(c("A", "B"), 50))
sp.linear$mdcaty1 <- ifelse(sp.linear$FNODE_ < 26, "a", "b")
sp.linear$mdcaty2 <- runif(nrow(sp.linear))
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
                                         seltype="Equal")), type.frame="linear", src.frame="sp.object",
                   sp.object=sp.linear, shapefile=FALSE)
test_that("test equal random selection using ryan_len linear shapefile and output shapefile",{
expect_true(exists("testsample"))
expect_equal(attributes(testsample)$class[1],"SpatialDesign")
expect_equal(nrow(testsample@data),10)
})
