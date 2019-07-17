library(spsurvey)
# sample(100000000,1) 
set.seed(52468110)

# Create sp objects
sp.finite <- read.shape("Coverages\\reg1_lakes")
sp.finite@data$mdcaty <- runif(nrow(sp.finite))

sp.linear <- read.shape("Coverages\\ryan_len")
sp.linear@data$stratum <- c("A", rep(c("A", "B"), 25))
sp.linear@data$mdcaty1 <- ifelse(sp.linear@data$FNODE_ < 26, "a", "b")
sp.linear@data$mdcaty2 <- runif(nrow(sp.linear))

sp.area <- read.shape("Coverages\\eco_l3_ut")
sp.area@data$stratum <- rep(c("A", "B"), 5)
sp.area@data$mdcaty1 <- ifelse(sp.area@data$ECO %in% c(13, 14, 19), "a", "b")
sp.area@data$mdcaty2 <- runif(nrow(sp.area))

# Create sf objects
load("data/NE_lakes.rda")
sf.finite <- NE_lakes
sf.finite$mdcaty <- runif(nrow(sf.finite))

sf.linear <- st_read("Coverages\\ryan_len.shp")
sf.linear$stratum <- c("A", rep(c("A", "B"), 25))
sf.linear$mdcaty1 <- ifelse(sf.linear$FNODE_ < 26, "a", "b")
sf.linear$mdcaty2 <- runif(nrow(sf.linear))

sf.area <- st_read("Coverages\\eco_l3_ut.shp")
sf.area$stratum <- rep(c("A", "B"), 5)
sf.area$mdcaty1 <- ifelse(sf.area$ECO %in% c(13, 14, 19), "a", "b")
sf.area$mdcaty2 <- runif(nrow(sf.area))
