# File: NELakes.R
# Purpose: Example finite GRTS survey designs for Northeast lakes
# Programmers: Tony Olsen, Tom Kincaid, Marc Weber
# Date: April 17, 2005
# Last Modified: May 9, 2018



# load the sf data frame as a .rda file (save as package dataset soon)
load("data/NE_lakes.rda")
plot(NE_lakes$geometry, axes=T)

# print first six lines of file
head(NE_lakes)
# print last six lines of file
tail(NE_lakes)


#####  Four example survey designs follow

### Equal Probability GRTS survey design for all lakes and reservoirs
# Create the design list


Equaldsgn <- list(CT=list(panel=c(Base=100), seltype="Equal"),
                  MA=list(panel=c(Base=100), seltype="Equal"),
                  RI=list(panel=c(Base=100), seltype="Equal"))

# Summarize the sample frame
framesum(NE_lakes,Equaldsgn, type='finite')

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(764966)  # Don't change unless want a different set of sites
Equalsites <- grts(design=Equaldsgn,
                   DesignID="NELakesEQ",
                   type.frame="finite",
                   att.frame=NE_lakes,
                   stratum = "State",
                   out.shape="NELakes_EqualSites" )

# summary of sites selected
dsgnsum(Equalsites)
# Print the initial six lines of the survey design
head(Equalsites@data)




### Stratified GRTS survey design for all lakes and reservoirs
### Stratify by state
att$stratum <- att$ST
# Create the design list
Stratdsgn <- list(CT=list(panel=c(Base=50), seltype="Equal"),
                  MA=list(panel=c(Base=50), seltype="Equal"),
                  ME=list(panel=c(Base=50), seltype="Equal"),
                  NH=list(panel=c(Base=50), seltype="Equal"),
                  RI=list(panel=c(Base=50), seltype="Equal"),
                  VT=list(panel=c(Base=50), seltype="Equal"))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(97029)  # Don't change unless want a different set of sites
Stratsites <- grts(design=Stratdsgn,
                   DesignID="NELakesST",
                   type.frame="finite",
                   src.frame="shapefile",
                   in.shape="reg1_lakes",
                   att.frame=att,
                   stratum="stratum",
                   prjfilename="reg1_lakes",
                   out.shape="NELakes_StratifySites"
                   )

# summary of sites selected
dsgnsum(Stratsites)
# Print the initial six lines of the survey design
head(Stratsites@data)

### Unequal probability GRTS survey design based on lake area 
### with an oversample
# Create unequal selection category variable
att$mdcaty <- att$lake.area.cat
# Create the design list
Unequaldsgn <- list(None=list(panel=c(Base=300),
                              seltype="Unequal",
                              caty.n=c("[0,1]"=50, "(1,5]"=50, "(5,10]"=50,
                                       "(10,50]"=50, "(50,500]"=50,
                                       "(500,7e+04]"=50),
                              over=120))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(605784)  # Don't change unless want a different set of sites
Unequalsites <- grts(design=Unequaldsgn,
                   DesignID="NELakesUN",
                   type.frame="finite",
                   src.frame="shapefile",
                   in.shape="reg1_lakes",
                   att.frame=att,
                   mdcaty="mdcaty",
                   prjfilename="reg1_lakes",
                   out.shape="NELakes_UnEqualSites")

# summary of sites selected
dsgnsum(Unequalsites)
# Print the initial six lines of the survey design
head(Unequalsites@data)


### Unequal probability GRTS survey design with an oversample and a panel
# structure for survey over time
# Create the design list
Paneldsgn <- list(None=list(panel=c(Annual=50, Year1=50, Year2=50, Year3=50,
                               Year4=50, Year5=50),
                            seltype="Unequal",
                            caty.n=c("[0,1]"=50, "(1,5]"=50, "(5,10]"=50,
                                     "(10,50]"=50, "(50,500]"=50,
                                     "(500,7e+04]"=50),
                            over=120))

# Create the GRTS survey design
sample(1000000,1) # run once to get random seed and put result into set.seed
# Reason is so that can reproduce exactly same sites if rerun it.
set.seed(427911)  # Don't change unless want a different set of sites
Panelsites <- grts(design=Paneldsgn,
                   DesignID="NELakesPan",
                   type.frame="finite",
                   src.frame="shapefile",
                   in.shape="reg1_lakes",
                   att.frame=att,
                   mdcaty="lake.area.cat",
                   prjfilename="reg1_lakes",
                   out.shape="NELakes_PanelSites")

# summary of sites selected
dsgnsum(Panelsites)
# Print the initial six lines of the survey design
head(Panelsites@data)
