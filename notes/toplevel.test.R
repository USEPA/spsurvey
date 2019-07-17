# Attach the spsurvey library

library(spsurvey)

sink("toplevel.test.out")

cat("CATEGORICAL ANALYSIS FUNCTION\n")

mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
   Resource.Class=rep(c("Good","Poor"), c(55,45)))
mywgt <- runif(100, 10, 100)
mystratum <- factor(rep(c("Stratum1", "Stratum2", "Stratum3"), c(50, 49, 1)))
mydesign <- data.frame(siteID=mysiteID, wgt=mywgt)
mydata.cat <- data.frame(siteID=mysiteID, CatVar=rep(c("north", "south",
   "east", "west"), 25))
psrv.obj <- spsurvey.analysis(sites=mysites, subpop=mysubpop,
   design=mydesign, data.cat=mydata.cat, vartype="SRS")

cat("\nUsing Direct Input to the Function\n\n")
top.test <- cat.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, vartype="SRS")
print(top.test)

cat("\nUsing an spsurvey Object\n\n")
top.test <- cat.analysis(spsurvey.obj=psrv.obj)
print(top.test)

mysites <- data.frame(siteID=mysiteID, Active=(rep(c(TRUE, FALSE,TRUE,TRUE),
   25)))
mydesign <- data.frame(siteID=mysiteID, wgt=mywgt, xcoord=runif(100),
   ycoord=runif(100), stratum=rep(c("Stratum1", "Stratum2"), 50))
mypopsize <- list(All.Sites=c(Stratum1=3500, Stratum2=2000),
   Resource.Class=list(Good=c(Stratum1=2500, Stratum2=1500),
   Poor=c(Stratum1=1000, Stratum2=500)))

cat("\nUsing Direct Input to the Function and a More Complex Design\n\n")
top.test <- cat.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cat=mydata.cat, popsize=mypopsize)
print(top.test)


cat("\n\n\n\nCONTINUOUS ANALYSIS FUNCTION\n")

mysiteID <- paste("Site", 1:24, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 24))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 24),
   Resource.Class=rep(c("Good","Poor"), c(12,12)))
mywgt <- runif(24, 10, 24)
mydesign <- data.frame(siteID=mysiteID, wgt=mywgt)
ContVar <- rnorm(24, 10, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar)
psrv.obj <- spsurvey.analysis(sites=mysites, subpop=mysubpop,
   design=mydesign, data.cont=mydata.cont, vartype="SRS")

cat("\nUsing Direct Input to the Function\n\n")
top.test <- cont.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   data.cont=mydata.cont, total=TRUE, vartype="SRS")
print(top.test)

cat("\nUsing an spsurvey Object\n\n")
top.test <- cont.analysis(spsurvey.obj=psrv.obj)
print(top.test)

mysites <- data.frame(siteID=mysiteID, Active=(rep(c(TRUE, FALSE,TRUE,TRUE),
   6)))
mydesign <- data.frame(siteID=mysiteID, wgt=mywgt, xcoord=runif(24),
   ycoord=runif(24), stratum=rep(c("Stratum1", "Stratum2"), 12))
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar,
   ContVar.1=ContVar + rnorm(24, 0, sqrt(0.25)),
   ContVar.2=ContVar + rnorm(24, 0, sqrt(0.50)))
mypopsize <- list(All.Sites=c(Stratum1=720, Stratum2=600),
   Resource.Class=list(Good=c(Stratum1=420, Stratum2=350),
   Poor=c(Stratum1=300, Stratum2=250)))
mysigma <- c(NA, 0.25, 0.50)
names(mysigma) <- c("ContVar", "ContVar.1", "ContVar.2")

cat("\nUsing Direct Input to the Function and Deconvolution\n\n")
top.test <- cont.analysis(sites=mysites, subpop=mysubpop[,1:2], design=mydesign,
   data.cont=mydata.cont, sigma=mysigma, popsize=mypopsize[1])
print(top.test)


cat("\n\n\n\nCDF INFERENCE FUNCTION\n")

n <- 200
mysiteID <- paste("Site", 1:n, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, n))
mysubpop <- data.frame(siteID=mysiteID,  Resource_Class=sample(c("Agr",
  "Forest", "Urban"), n, replace=TRUE))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(n, 10, 100),
  xcoord=runif(n), ycoord=runif(n), stratum=rep(c("Stratum1",
  "Stratum2"), n/2))
mypopsize <- list(Resource_Class=list(Agr=c(Stratum1=2500, Stratum2=1500),
  Forest=c(Stratum1=1000, Stratum2=500), Urban=c(Stratum1=600, Stratum2=450)))
ContVar <- numeric(n)
tst <- mysubpop$Resource_Class == "Agr"
ContVar[tst] <- rnorm(sum(tst), 10, 1)
tst <- mysubpop$Resource_Class == "Forest"
ContVar[tst] <- rnorm(sum(tst), 10.1, 1)
tst <- mysubpop$Resource_Class == "Urban"
ContVar[tst] <- rnorm(sum(tst), 10.5, 1)
mydata.cont <- data.frame(siteID=mysiteID, ContVar=ContVar)
psrv.obj <- spsurvey.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
  data.cont=mydata.cont, popsize=mypopsize)

cat("\nUsing Direct Input to the Function\n\n")
top.test <- cont.cdftest(sites=mysites, subpop=mysubpop, design=mydesign,
  data.cont=mydata.cont, popsize=mypopsize, testname="Mean_Eigenvalue")
print(top.test)

cat("\nUsing an spsurvey Object\n\n")
top.test <- cont.cdftest(spsurvey.obj=psrv.obj, testname="Mean_Eigenvalue")
print(top.test)


cat("\n\n\n\nRELATIVE RISK ANALYSIS FUNCTION\n\n")
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
  Resource.Class=rep(c("Agr", "Forest"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10, 100),
  xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
  "Stratum2"), 50))
mydata.rr <- data.frame(siteID=mysiteID, RespVar1=sample(c("Poor", "Good"),
  100, replace=TRUE), RespVar2=sample(c("Poor", "Good"), 100, replace=TRUE),
  StressVar=sample(c("Poor", "Good"), 100, replace=TRUE), wgt=runif(100, 10,
  100))
top.test <- relrisk.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
  data.rr=mydata.rr, response.var=c("RespVar1", "RespVar2"),
  stressor.var=rep("StressVar", 2))
print(top.test)


cat("\n\n\n\nATTRUBUTABLE RISK ANALYSIS FUNCTION\n\n")
mysiteID <- paste("Site", 1:100, sep="")
mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, 100))
mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", 100),
  Resource.Class=rep(c("Agr", "Forest"), c(55,45)))
mydesign <- data.frame(siteID=mysiteID, wgt=runif(100, 10, 100),
  xcoord=runif(100), ycoord=runif(100), stratum=rep(c("Stratum1",
  "Stratum2"), 50))
mydata.ar <- data.frame(siteID=mysiteID, RespVar1=sample(c("Poor", "Good"),
  100, replace=TRUE), RespVar2=sample(c("Poor", "Good"), 100, replace=TRUE),
  StressVar=sample(c("Poor", "Good"), 100, replace=TRUE), wgt=runif(100, 10,
  100))
top.test <- attrisk.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
  data.ar=mydata.ar, response.var=c("RespVar1", "RespVar2"),
  stressor.var=rep("StressVar", 2))
print(top.test)

sink()
