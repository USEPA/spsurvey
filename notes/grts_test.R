library(spsurvey)

# sample(100000000,1) 
set.seed(52468110)

ind.fp_len <- FALSE
ind.indiana_rte <- FALSE
ind.nc_per_alb83 <- FALSE
ind.hiest04_alb83 <- FALSE

sink("grts_test.out")

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
cat("\nEqual random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="finite", src.frame="sf.object",
   sf.object=sf.finite, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection using a shapefile frame:\n")
testsample <- grts(design=list("LAKE/POND"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("5"=5, "8"=5), seltype="Unequal"),
   "RESERVOIR"=list(panel=c(PanelOne=10), over=0, caty.n=c("5"=5, "8"=5),
   seltype="Unequal")), type.frame="finite", in.shape="Coverages\\reg1_lakes",
   stratum="FTYPE", mdcaty="LEVEL1", shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nContinuous random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="finite", src.frame="sf.object",
   sf.object=sf.finite, mdcaty="mdcaty", shapefile=FALSE)
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
cat("\nEqual random selection using a shapefile frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\ryan_len",
   shapefile=TRUE)
print(testsample@data)
cat("\nEqual random selection using an sp object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", src.frame="sp.object",
   sp.object=sp.linear, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nEqual random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="linear", src.frame="sf.object",
   sf.object=sf.linear, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection using an sp object frame:\n")
testsample <- grts(design=list("A"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("a"=5, "b"=5), seltype="Unequal"), "B"=list(panel=c(PanelOne=10),
   over=0, caty.n=c("a"=5, "b"=5), seltype="Unequal")), type.frame="linear",
   src.frame="sp.object", sp.object=sp.linear, stratum="stratum",
   mdcaty="mdcaty1", shapefile=FALSE)
print(testsample@data)
cat("\nContinuous random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="linear", src.frame="sf.object",
   sf.object=sf.linear, mdcaty="mdcaty2", shapefile=FALSE)
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
   seltype="Equal")), type.frame="linear", in.shape="Coverages\\NHDTanana_main_ALB", shapefile=FALSE)
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
   in.shape="Coverages\\hiest04_alb83", stratum="CLASS", mdcaty="ISLAND_NAM",
   shapefile=FALSE, startlev=9)
print(testsample@data)
}

cat("\n\nArea: eco_l3_ut Polygon shapefile\n")
cat("\nEqual random selection using a shapefile frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", in.shape="Coverages\\eco_l3_ut",
   shapefile=TRUE)
print(testsample@data)
cat("\nEqual random selection using an sp object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", src.frame="sp.object",
   sp.object=sp.area, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nEqual random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", src.frame="sf.object",
   sf.object=sf.area, shapefile=FALSE)
print(testsample@data[,1:9])
cat("\nUnequal random selection using an sp object frame:\n")
testsample <- grts(design=list("A"=list(panel=c(PanelOne=10), over=6,
   caty.n=c("a"=5, "b"=5), seltype="Unequal"),
   "B"=list(panel=c(PanelOne=10), over=0, caty.n=c("a"=5, "b"=5),
   seltype="Unequal")), type.frame="area", src.frame="sp.object",
   sp.object=sp.area, stratum="stratum", mdcaty="mdcaty1", shapefile=FALSE)
print(testsample@data)
cat("\nContinuous random selection using an sf object frame:\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Continuous")), type.frame="area", src.frame="sf.object",
   sf.object=sf.area, mdcaty="mdcaty2", shapefile=FALSE)
print(testsample@data)


cat("\n\nArea: NHDArea PolygonZ shapefile\n")
testsample <- grts(design=list(None=list(panel=c(PanelOne=10), over=0,
   seltype="Equal")), type.frame="area", in.shape="Coverages\\NHDArea",
   shapefile=FALSE)
print(testsample@data)

sink()
