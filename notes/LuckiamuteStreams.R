# File: LuckiamuteStreams.R
# Purpose: Example linear GRTS survey designs for 
#          Lukiamute Watershed Council streams
# Programmers: Tony Olsen and Tom Kincaid
# Date: April 17, 2005
# Last Modified: June 20, 2019

# Load the spsurvey library
library(spsurvey)

# read the shapefile. In general don't need to do this.
# only done here so that can plot the stream network in R instead of ArcGIS
load("F:/Git Projects/spsurvey/data/Luck_Ash_streams.rda")
plot(Luck_Ash_streams$geometry)



###### Four example GRTS survey designs follow
# Equal probability GRTS survey design
# Create the design list
Equaldsgn <- list(None=list(panel=c(Base=50), seltype="Equal"))

# Summarize the sample frame
framesum(Luck_Ash_streams,Equaldsgn, type='linear')

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(458454)  # Don't change unless want a different set of sites
Equalsites <- grts(design=Equaldsgn,
                   DesignID="LuckEQ",
                   type.frame="linear",
                   src.frame="sf.object",
                   sf.object = Luck_Ash_streams,
                   shapefile = TRUE,
                   out.shape="Luck_EqualSites")

# print summary of sites selected
dsgnsum(Equalsites)
# Print the initial six lines of the survey design
sites <- Equalsites@data    # this is attribute part of shapefile created
head(sites)

# plot sites on stream network
plot(shape)
points(Equalsites, pch=16, col='red')



### Stratified GRTS survey design with an oversample
# Create the design list
Stratdsgn <- list(Perennial=list(panel=c(Base=50),
                                 seltype="Equal",
                                 over=50),
                  Intermittent=list(panel=c(Base=50),
                                    seltype="Equal",
                                    over=50))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(603619)  # Don't change unless want a different set of sites
Stratsites <- grts(design=Stratdsgn,
                   DesignID="LuckST",
                   type.frame="linear",
                   src.frame="shapefile",
                   in.shape="luck-ash",
                   att.frame=att,
                   stratum="stratum",
                   prjfilename="luck-ash",
                   out.shape="Luck_StratifySites")

# print summary of sites selected
dsgnsum(Stratsites)
# Print the initial six lines of the survey design
sites <- Stratsites@data    # this is attribute part of shapefile created
head(sites)

# plot sites on stream network
plot(shape)
points(Stratsites, pch=16, col='red')

### Unequal probability GRTS survey design with an oversample and stratification
# Create the design list
Unequaldsgn <- list(Perennial=list(panel=c(Base=75),
                                   seltype="Unequal",
                                   caty.n=c("1st"=25, "2nd"=25, "3rd+"=25),
                                   over=36),
                    Intermittent=list(panel=c(Base=25),
                                      seltype="Unequal",
                                      caty.n=c("1st"=17, "2nd"=5, "3rd+"=3),
                                      over=0))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(609715)  # Don't change unless want a different set of sites
Unequalsites <- grts(design=Unequaldsgn,
                   DesignID="LuckUN",
                   type.frame="linear",
                   src.frame="shapefile",
                   in.shape="luck-ash",
                   att.frame=att,
                   stratum="stratum",
                   mdcaty="mdcaty",
                   prjfilename="luck-ash",
                   out.shape="Luck_UnEqualSites"
                   )

# print summary of sites selected
dsgnsum(Unequalsites)
# Print the initial six lines of the survey design
sites <- Unequalsites@data    # this is attribute part of shapefile created
head(sites)

# plot sites on stream network
plot(shape)
points(Unequalsites, pch=16, col='red')


### Unequal probability GRTS survey design with an oversample and a panel
### structure for survey over time
# Create the design list
Paneldsgn <- list(Perennial=list(panel=c(Year1=17, Year2=17, YearAll=16),
                                 seltype="Unequal",
                                 caty.n=c("1st"=15, "2nd"=15, "3rd+"=20 ),
                                 over=50),
                  Intermittent=list(panel=c(YearOnce=25),
                                    seltype="Unequal",
                                    caty.n=c("1st"=17, "2nd"=5, "3rd+"=3 ),
                                    over=0))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(310913)  # Don't change unless want a different set of sites
Panelsites <- grts(design=Paneldsgn,
                   DesignID="LuckPan",
                   type.frame="linear",
                   src.frame="shapefile",
                   in.shape="luck-ash",
                   att.frame=att,
                   stratum="stratum",
                   mdcaty="mdcaty",
                   prjfilename="luck-ash",
                   out.shape="Luck_PanelSites")

# print summary of sites selected
dsgnsum(Panelsites)
# Print the initial six lines of the survey design
sites <- Panelsites@data    # this is attribute part of shapefile created
head(sites)

# plot sites on stream network
plot(shape)
points(Panelsites, pch=16, col='red')