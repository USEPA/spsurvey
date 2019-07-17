# Attach the spsurvey library

library(spsurvey)

# Set Indicator Values

data.input.ind <- TRUE
category.est.ind <- TRUE
total.est.ind <- TRUE
cdf.est.ind <- TRUE
cdf.decon.ind <- TRUE
cdf.test.ind <- TRUE
relrisk.est.ind <- TRUE
attrisk.est.ind <- TRUE


# Section for Data Input

if(data.input.ind) {

NY.dat <- read.table("NY.dat", header=TRUE, sep="\t")
z <- read.table("NY2.dat", header=TRUE)
NY.dat$resp <- z$resp
NY.dat$sizewt <- z$sizewt
nresp <- length(NY.dat$resp)
NY.rnorm <- rnorm(nresp, 1, 1)
NY.dat$stratum <- factor(rep(c('Stratum1', 'Stratum2', 'Stratum3'), c(22, 22, 25)))
zR <- c(1080,490,540)
names(zR) <- c('Stratum1', 'Stratum2', 'Stratum3')
zW <- c(450,180,220)
names(zW) <- c('Stratum1', 'Stratum2', 'Stratum3')
NY.dat$cluster <- factor(rep(c('A','B','C','D','E','F','G'), c(rep(11,6), 3)))
zNcluster <- c(30,30,40)
names(zNcluster) <- c('Stratum1', 'Stratum2', 'Stratum3')
NY.dat$wgt2 <- rep(c(3.5, 6.3, 2.2, 2.3, 1.8, 2.0, 4.0), c(rep(11,6), 3))
NY.dat$wgt1 <- rep(tapply(NY.dat$weight, NY.dat$cluster, mean)/c(3.5, 6.3, 2.2, 2.3, 1.8, 2.0, 4.0), c(rep(11,6), 3))
NY.dat$x1 <- rep(c(-75.171, -74.929, -74.797, -74.769, -74.934, -74.999, -75.092), c(rep(11,6), 3))
NY.dat$y1 <- rep(c(42.140, 42.024, 42.144, 42.216, 42.167, 42.159, 42.187), c(rep(11,6), 3))
zstage1size <- c(rep(20,6), 10)
names(zstage1size) <- c('A','B','C','D','E','F','G')
zstage1size.s <- c(rep(20,6), 10)
names(zstage1size.s) <- c('Stratum1&A', 'Stratum1&B', 'Stratum2&C', 'Stratum2&D', 'Stratum3&E', 'Stratum3&F', 'Stratum3&G')
zsupport <- rep(1,69)
NY.dat$swgt2 <- rep(c(0.044, 0.042, 0.049, 0.037, 0.050, 0.044, 0.027), c(rep(11,6), 3))
NY.dat$swgt1 <- rep(tapply(NY.dat$sizewt, NY.dat$cluster, mean)/c(0.044, 0.042, 0.049, 0.037, 0.050, 0.044, 0.027), c(rep(11,6), 3))
bounds <- sort(c(NY.dat$resp, NY.dat$resp+NY.rnorm))[floor(seq((2*nresp)/3, (2*nresp), length=3))]

}


sink("analysis.test.out")


# Section for Category Estimation Function

if(category.est.ind) {

cat("CATEGORY ESTIMATION FUNCTION\n")

cat("\nStatus Estimation: Single Stage - No Strata, Local Variance\n\n")
zny <- category.est(catvar=NY.dat$status, wgt=NY.dat$weight, x=NY.dat$long.dd, y=NY.dat$lat.dd, vartype="Local")
print(zny)

cat("\n\nStatus Estimation: Single Stage - No Strata, SRS variance\n\n")
zny <- category.est(catvar=NY.dat$status, wgt=NY.dat$weight, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, popsize=2110, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, Known Size, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zR, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nStatus Estimation: Two Stage - No Strata, Local Variance\n\n")
zny <- category.est(catvar=NY.dat$status, wgt=NY.dat$wgt2, x=NY.dat$long.dd, y=NY.dat$lat.dd, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, x1=NY.dat$x1, y1=NY.dat$y1, vartype="Local")
print(zny)

cat("\n\nStatus Estimation: Two Stage - No Strata, SRS variance\n\n")
zny <- category.est(catvar=NY.dat$status, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=2110, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, Known Size, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zR, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, popsize=850, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zW, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Single Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=850, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zW, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nCategory Estimation: Two Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- category.est(catvar=NY.dat$group, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

}


# Section for Total Estimation Function

if(total.est.ind) {

cat("\n\n\n\nTOTAL ESTIMATION FUNCTION\n")

cat("\nTotal Estimation: Single Stage - No Strata, Local Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, x=NY.dat$long.dd, y=NY.dat$lat.dd, vartype="Local")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, SRS variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, popsize=2110, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, Known Size, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zR, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Local Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, x=NY.dat$long.dd, y=NY.dat$lat.dd, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, x1=NY.dat$x1, y1=NY.dat$y1, vartype="Local")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, SRS variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=2110, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, Known Size, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zR, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, popsize=850, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zW, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Single Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=850, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zW, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

cat("\n\nTotal Estimation: Two Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- total.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS")
print(zny)

}


# Section for CDF Estimation Function

if(cdf.est.ind) {

cat("\n\n\n\nCDF ESTIMATION FUNCTION\n")

cat("\nCDF Estimation: Single Stage - No Strata, Local Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, x=NY.dat$long.dd, y=NY.dat$lat.dd, vartype="Local", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, SRS variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, popsize=2110, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zR, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Local Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, x=NY.dat$long.dd, y=NY.dat$lat.dd, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, x1=NY.dat$x1, y1=NY.dat$y1, vartype="Local", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, SRS variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=2110, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zR, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, popsize=850, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, popcorrect=TRUE, pcfsize=2110, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popsize=zW, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Single Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$weight, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=850, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zW, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Estimation: Two Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.est(z=NY.dat$resp, wgt=NY.dat$wgt2, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

}


# Section for CDF Deconvolution Function

if(cdf.decon.ind) {

cat("\n\n\n\nCDF DECONVOLUTION FUNCTION\n")

cat("\nCDF Deconvolution: Single Stage - No Strata, Local Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, x=NY.dat$long.dd, y=NY.dat$lat.dd, vartype="Local", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, SRS variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, popsize=2110, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, popcorrect=TRUE, pcfsize=2110, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, popsize=zR, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Local Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, x=NY.dat$long.dd, y=NY.dat$lat.dd, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, x1=NY.dat$x1, y1=NY.dat$y1, vartype="Local", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, SRS variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Known Size, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=2110, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zR, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, popsize=850, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, popcorrect=TRUE, pcfsize=2110, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, popsize=zW, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Single Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$weight, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, popcorrect=TRUE, pcfsize=zR, support=zsupport, sizeweight=TRUE, swgt=NY.dat$sizewt, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=850, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=100, stage1size=zstage1size, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popsize=zW, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

cat("\n\nCDF Deconvolution: Two Stage - Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.decon(z=NY.dat$resp, wgt=NY.dat$wgt2, sigma=0.25, var.sigma=0.1, stratum=NY.dat$stratum, cluster=NY.dat$cluster, wgt1=NY.dat$wgt1, popcorrect=TRUE, N.cluster=zNcluster, stage1size=zstage1size.s, support=zsupport, sizeweight=TRUE, swgt=NY.dat$swgt2, swgt1=NY.dat$swgt1, vartype="SRS", cdfval=seq(6.75, 12, length=10))
print(zny)

}


# Section for CDF Hypothesis Test Function

if(cdf.test.ind) {

cat("\n\n\nCDF HYPOTHESIS TEST FUNCTION\n")

cat("\nCDF Inference: Single Stage - No Strata, Local Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, x_1=NY.dat$long.dd, y_1=NY.dat$lat.dd, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, x_2=NY.dat$long.dd, y_2=NY.dat$lat.dd)
print(zny)

cat("\n\nCDF Inference: Single Stage - No Strata, SRS variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, popsize_1=2110, popsize_2=2110, support_1=zsupport, support_2=zsupport, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, popsize_1=zR, popsize_2=zR, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, popsize_1=zR, popsize_2=zR, support_1=zsupport, support_2=zsupport, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - No Strata, Local Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, x_1=NY.dat$long.dd, y_1=NY.dat$lat.dd, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, x_2=NY.dat$long.dd, y_2=NY.dat$lat.dd, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, x1_1=NY.dat$x1, y1_1=NY.dat$y1, wgt1_2=NY.dat$wgt1, x1_2=NY.dat$x1, y1_2=NY.dat$y1)
print(zny)

cat("\n\nCDF Inference: Two Stage - No Strata, SRS variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - No Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, N.cluster_1=100, N.cluster_2=100, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, stage1size_1=zstage1size, stage1size_2=zstage1size, support_1=zsupport, support_2=zsupport, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, Known Size, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, popsize_1=zR, popsize_2=zR, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, Pop. Correction, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, N.cluster_1=zNcluster, N.cluster_2=zNcluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, stage1size_1=zstage1size.s, stage1size_2=zstage1size.s, support_1=zsupport, support_2=zsupport, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, popsize_1=2110, popsize_2=2110, support_1=zsupport, support_2=zsupport, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, popsize_1=zW, popsize_2=zW, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Single Stage - Strata, Pop. Correction, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$weight, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$weight, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, popsize_1=zW, popsize_2=zW, support_1=zsupport, support_2=zsupport, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - No Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, swgt1_1=NY.dat$swgt1, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, swgt1_2=NY.dat$swgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - No Strata, Pop. Correction, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, N.cluster_1=100, N.cluster_2=100, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, stage1size_1=zstage1size, stage1size_2=zstage1size, support_1=zsupport, support_2=zsupport, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, swgt1_1=NY.dat$swgt1, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, swgt1_2=NY.dat$swgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, swgt1_1=NY.dat$swgt1, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, swgt1_2=NY.dat$swgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, swgt1_1=NY.dat$swgt1, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, swgt1_2=NY.dat$swgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

cat("\n\nCDF Inference: Two Stage - Strata, Pop. Correction, Known Sum of Size-Weights, SRS Variance\n\n")
zny <- cdf.test(bounds=bounds, z_1=NY.dat$resp, wgt_1=NY.dat$wgt2, z_2=NY.dat$resp+NY.rnorm, wgt_2=NY.dat$wgt2, stratum_1=NY.dat$stratum, stratum_2=NY.dat$stratum, cluster_1=NY.dat$cluster, cluster_2=NY.dat$cluster, wgt1_1=NY.dat$wgt1, wgt1_2=NY.dat$wgt1, popsize_1=zW, popsize_2=zW, N.cluster_1=zNcluster, stage1size_1=zstage1size.s, support_1=zsupport, N.cluster_2=zNcluster, stage1size_2=zstage1size.s, support_2=zsupport, sizeweight_1=TRUE, swgt_1=NY.dat$sizewt, swgt1_1=NY.dat$swgt1, sizeweight_2=TRUE, swgt_2=NY.dat$sizewt, swgt1_2=NY.dat$swgt1, vartype_1="SRS", vartype_2="SRS")
print(zny)

}


# Section for Relative Risk Estimation Function

if(relrisk.est.ind) {

cat("\n\n\n\nRELATIVE RISK ESTIMATION FUNCTION\n")

cat("\nRelative Risk Estimation: Single Stage - No Strata, SRS Variance\n\n")
response <- sample(c("Poor", "Good"), 100, replace=TRUE)
stressor <- sample(c("Poor", "Good"), 100, replace=TRUE)
wgt <- runif(100, 10, 100)
zny <- relrisk.est(response, stressor, wgt=wgt, vartype="SRS")
print(zny)

cat("\n\nRelative Risk Estimation: Single Stage - No Strata, Local Variance\n\n")
xcoord <- runif(100)
ycoord <- runif(100)
zny <- relrisk.est(response, stressor, wgt=wgt, xcoord=xcoord, ycoord=ycoord)
print(zny)

}


# Section for Attributable Risk Estimation Function

if(attrisk.est.ind) {

cat("\n\n\n\nATTRIBUTABLE RISK ESTIMATION FUNCTION\n")

cat("\nAttributable Risk Estimation: Single Stage - No Strata, SRS Variance\n\n")
response <- sample(c("Poor", "Good"), 100, replace=TRUE)
stressor <- sample(c("Poor", "Good"), 100, replace=TRUE)
wgt <- runif(100, 10, 100)
zny <- attrisk.est(response, stressor, wgt=wgt, vartype="SRS")
print(zny)

cat("\n\nAttributable Risk Estimation: Single Stage - No Strata, Local Variance\n\n")
xcoord <- runif(100)
ycoord <- runif(100)
zny <- attrisk.est(response, stressor, wgt=wgt, xcoord=xcoord, ycoord=ycoord)
print(zny)

}

sink()
