library(spsurvey)

# sample(100000000,1) 
set.seed(52468110)

ind.fp_len <- FALSE
ind.indiana_rte <- FALSE
ind.nc_per_alb83 <- FALSE
ind.hiest04_alb83 <- FALSE

sink("grts.test.out")

cat("Finite: reg1_lakes Point shapefile\n")
cat("\nEqual random selection using a shapefile frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="finite", in.shape="Coverages\\reg1_lakes",
   shapefile=TRUE)
print(testsample@data[,1:9])
cat("\nEqual random selection using an attributes frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="finite", src.frame="att.frame",
   att.frame=sp.finite, xcoord="X_COORD", ycoord="Y_COORD", shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nEqual random selection using an sp object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="finite", src.frame="sp.object",
   sp.object=sp.finite, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection:\n")
testsample <- grts(design=list("LAKE/POND"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("5"=5, "8"=5), seltype="Unequal"),
   "RESERVOIR"=list(panel=c(PanelOne=10), over=0, caty.n=c("5"=5, "8"=5),
   seltype="Unequal")), type.frame="finite", in.shape="Coverages\\reg1_lakes",
   stratum="FTYPE", mdcaty="LEVEL1", shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nContinuous random selection:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="finite", in.shape="Coverages\\reg1_lakes",
   att.frame=z@data, mdcaty="mdcaty", shapefile=FALSE)
print(testsample@data[,1:9])

cat("\n\nFinite: NHDPoint PointZ shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="finite", in.shape="Coverages\\NHDPoint",
   shapefile=FALSE)
print(testsample@data)


if(ind.fp_len) {
cat("\n\nLinear: fp_len Polyline shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\fp_len",
   shapefile=FALSE)
print(testsample@data)
}

cat("\n\nLinear: ryan_len Polyline shapefile\n")
z <- read.shape("Coverages\\ryan_len")
z@data$stratum <- c("A", rep(c("A", "B"), 25))
z@data$mdcaty1 <- ifelse(z@data$FNODE_ < 26, "a", "b")
z@data$mdcaty2 <- runif(nrow(z))
cat("\nEqual random selection using a shapefile frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\ryan_len",
   shapefile=FALSE)
print(testsample@data)
cat("\nEqual random selection using an sp object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", src.frame="sp.object", sp.object=z,
   att.frame=z@data, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection:\n")
testsample <- grts(design=list("A"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("a"=5, "b"=5), seltype="Unequal"),
   "B"=list(panel=c(PanelOne=10), over=0, caty.n=c("a"=5, "b"=5),
   seltype="Unequal")), type.frame="linear", in.shape="Coverages\\ryan_len",
   att.frame=z@data, stratum="stratum", mdcaty="mdcaty1", shapefile=FALSE)
print(testsample@data)
cat("\nContinuous random selection:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="linear", in.shape="Coverages\\ryan_len",
   att.frame=z@data, mdcaty="mdcaty2", shapefile=FALSE)
print(testsample@data)

if(ind.indiana_rte) {
cat("\n\nLinear: indiana_rte PolylineZ shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\indiana_rte",
   maxlev=3, shapefile=FALSE)
print(testsample@data)
}

cat("\n\nLinear: NHDTanana_main_ALB PolylineZ shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\NHDTanana_main_ALB",
   shapefile=FALSE)
print(testsample@data)

if(ind.nc_per_alb83) {
cat("\n\nLinear: nc_per_alb83 PolylineM shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\nc_per_alb83",
   shapefile=FALSE)
print(testsample@data[,1:9])
}


if(ind.hiest04_alb83) {
cat("Area: hiest04_alb83 Polygon shapefile\n")
testsample <- grts(design=list("< 100 SQ.Kilometers"=list(panel=c(PanelOne=16),
   over=0, caty.n=c(HAWAII=2, KAHOOLAWE=2, KAUAI=2, LANAI=2, MAUI=2, MOLOKAI=2,
   NIIHAU=2, OAHU=2), seltype="Unequal")), type.frame="area",
   in.shape="Coverages\\hiest04_alb83", stratum="CLASS",
   mdcaty="ISLAND_NAM", shapefile=FALSE, startlev=9)
print(testsample@data)
}

cat("\n\nArea: eco_l3_ut Polygon shapefile\n")
z <- read.shape("Coverages\\eco_l3_ut")
z@data$stratum <- rep(c("A", "B"), 5)
z@data$mdcaty1 <- ifelse(z@data$ECO %in% c(13, 14, 19), "a", "b")
z@data$mdcaty2 <- runif(nrow(z))
cat("\nEqual random selection using a shapefile frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", in.shape="Coverages\\eco_l3_ut",
   shapefile=FALSE)
print(testsample@data)
cat("\nEqual random selection using an sp object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", src.frame="sp.object", sp.object=z,
   att.frame=z@data, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection:\n")
testsample <- grts(design=list("A"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("a"=5, "b"=5), seltype="Unequal"),
   "B"=list(panel=c(PanelOne=10), over=0, caty.n=c("a"=5, "b"=5),
   seltype="Unequal")), type.frame="area", in.shape="Coverages\\eco_l3_ut",
   att.frame=z@data, stratum="stratum", mdcaty="mdcaty1", shapefile=FALSE)
print(testsample@data)
cat("\nContinuous random selection:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="area", in.shape="Coverages\\eco_l3_ut",
   att.frame=z@data, mdcaty="mdcaty2", shapefile=FALSE)
print(testsample@data)


cat("\n\nArea: NHDArea PolygonZ shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", in.shape="Coverages\\SBI_shoreline",
   shapefile=FALSE)
print(testsample@data)

rm(z)
sink()


if(FALSE) {

hiest04.plot3(x=c(-6133082.704688, -6115530.661719), y=c(1916786.070313, 1927317.296094))

inside.owin(x=c(-6133082.704688, -6115530.661719), y=c(1916786.070313, 1927317.296094), w=shp.to.owin.fcn(shp=hiest04$shp[62]))

z <- data.frame(polyid=1:376, polyarea=hiest04.alb83$dbf$dbf[,1], mdcaty=rep(paste("Caty", 1:4, sep=""), c(100,100,100,76)))
z$mdm <- mdmarea(z$polyarea, z$mdcaty, c(Caty1=4, Caty2=4, Caty3=4, Caty4=4))

temp.fcn <- function(x, y) {
npt <- length(x)
temp <- data.frame(rec=1:376, pt1=logical(376), pt2=logical(376))
for(rec in 1:376)
   temp[rec, 2:(npt+1)] <- inside.owin(x=x, y=y, w=shp.to.owin.fcn(shp=hiest04$shp[rec]))
temp[temp[,2] == TRUE | temp[,3] == TRUE,]
}
temp.fcn(x=c(-6133082.704688, -6115530.661719), y=c(1916786.070313, 1927317.296094)) 


library(spsurvey)
memory.limit(4000)
grid.xmin <- grid.ymin <- 0
grid.xmax <- grid.ymax <- 100
grid.extent <- 100
nlev <- 12
nlv2 <- 2^nlev
dx <- dy <- grid.extent/nlv2
xc <- seq(grid.xmin, grid.xmax, length=nlv2+1)
yc <- seq(grid.ymin, grid.ymax, length=nlv2+1)
xc <- rep(xc, nlv2+1)
yc <- rep(yc, rep(nlv2+1, nlv2+1))
nlev <- nlev + 1
hadr <- .Call("constructAddr", xc, yc, dx, dy, as.integer(nlev), PACKAGE="spsurvey")

library(spsurvey)
ranhadr <- .C("ranho", rep("1234123412341", ((2 ^ 12) + 1)^2), as.integer(13), PACKAGE="spsurvey")[[1]]

}
