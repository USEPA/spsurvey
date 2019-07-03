# File: UtahLevel3Ecoregions.R
# Purpose: Example area GRTS survey designs for 
#          Omernik level 3 ecoregions within Utah
# Programmers: Tony Olsen and Tom Kincaid
# Date: February 28, 2005
# Last Modified: June 20, 2019

# Load the spsurvey library
library(spsurvey)

# Read the attribute table from the shapefile
load("F:/Git Projects/spsurvey/data/UT_ecoregions.rda")
# print first 6 lines of attribute file
head(UT_ecoregions)


# Display names of the ecoregions attribute that will be used to define
# multidensity categories (mdcaty) and strata
levels(UT_ecoregions$Level3_Nam)

###### Four example GRTS survey designs follow
### Equal probability GRTS survey design
# Create the design list
Equaldsgn <- list(None=list(panel=c(Base=115), seltype="Equal")
								)

# Summarize the sample frame
framesum(UT_ecoregions,Equaldsgn, type.frame='area')

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(656633)  # Don't change unless want a different set of sites
Equalsites <- grts(design=Equaldsgn,
                   src.frame="shapefile",
                   in.shape="eco_l3_ut", 
                   type.frame="area",
                   DesignID="UTEco3EQ",
                   shapefile=TRUE,
                   out.shape="Eco3_EqualSites")

# print summary of sites selected
dsgnsum(Equalsites)
# Print the initial six lines of the survey design
sites <- Equalsites@data    # this is attribute part of shapefile created
head(sites)

### Unequal Probability GRTS survey design by Level 3 ecoregion
# create unequal probability variable
att$mdcaty <- att$LEVEL3_NAM
# Create the design list
Unequaldsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype="Unequal",
                              caty.n=c("Central Basin and Range"=25,
                                       "Colorado Plateaus"=25,
                                       "Mojave Basin and Range"=10,
                                       "Northern Basin and Range"=10,
                                       "Southern Rockies"=10,
                                       "Wasatch and Uinta Mountains"=25,
                                       "Wyoming Basin"=10)))

# Summarize the sample frame
framesum(att,Unequaldsgn, type='area', mdcaty='mdcaty')

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(275854)  # Don't change unless want a different set of sites
Unequalsites <- grts(design=Unequaldsgn,
                     src.frame="shapefile",
                     in.shape="eco_l3_ut",  
                     att.frame=att,
                     type.frame="area",
                     mdcaty="mdcaty",									
                     DesignID="UTEco3UN",
                     shapefile=TRUE,
                     prj="eco_l3_ut",
                     out.shape="Eco3_UnEqualSites")

# print summary of sites selected
dsgnsum(Unequalsites)
# Print the initial six lines of the survey design
sites <- Unequalsites@data    # this is attribute part of shapefile created
head(sites)

### Stratified GRTS survey design by Level 3 ecoregion
# create stratum variable
att$stratum <- att$LEVEL3_NAM
# Create the design list
Stratdsgn <- list("Central Basin and Range"=list(panel=c(Base=25),
                                                 seltype="Equal"),
                  "Colorado Plateaus"=list(panel=c(Base=25),
                                           seltype="Equal"),
                  "Mojave Basin and Range"=list(panel=c(Base=10),
                                                seltype="Equal"),
                  "Northern Basin and Range"=list(panel=c(Base=10),
                                                  seltype="Equal"),
                  "Southern Rockies"=list(panel=c(Base=10),
                                          seltype="Equal"),
                  "Wasatch and Uinta Mountains"=list(panel=c(Base=25),
                                                     seltype="Equal"),
                  "Wyoming Basin"=list(panel=c(Base=10),
                                       seltype="Equal"))

# Summarize the sample frame
framesum(att,Unequaldsgn, type='area', stratum='stratum')

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(174514)  # Don't change unless want a different set of sites
Stratsites <- grts(design=Stratdsgn,
                   DesignID="UTEco3ST",
                   src.frame="shapefile",
                   in.shape="eco_l3_ut",  
                   att.frame=att,
                   type.frame="area",
                   stratum="stratum",
                   shapefile=TRUE,
                   prj="eco_l3_ut",
                   out.shape="Eco3_StratifySites")

# print summary of sites selected
dsgnsum(Stratsites)
# Print the initial six lines of the survey design
sites <- Stratsites@data    # this is attribute part of shapefile created
head(sites)

### Unequal probability GRTS survey design with an oversample 
### and a panel structure for survey over time
# Create the design list
Paneldsgn <- list(None=list(panel=c(Year_1=50, Year_2=50, Year_3=50,
                                    Year_4=50, Year_5=50),
                            seltype="Unequal",
                            caty.n=c("Central Basin and Range"=64,
                                     "Colorado Plateaus"=63,
                                     "Mojave Basin and Range"=15,
                                     "Northern Basin and Range"=15,
                                     "Southern Rockies"=15,
                                     "Wasatch and Uinta Mountains"=63,
                                     "Wyoming Basin"=15),
                            over=100))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(488823)  # Don't change unless want a different set of sites
Panelsites <- grts(design=Paneldsgn,
                   src.frame="shapefile",
                   in.shape="eco_l3_ut",  
                   att.frame=att,
                   type.frame="area",
                   mdcaty="mdcaty",									
                   DesignID="UTEco3Pan",
                   shapefile=TRUE,
                   prj="eco_l3_ut",
                   out.shape="Eco3_PanelSites")

# print summary of sites selected
dsgnsum(Panelsites)
# Print the initial six lines of the survey design
sites <- Panelsites@data    # this is attribute part of shapefile created
head(sites)
