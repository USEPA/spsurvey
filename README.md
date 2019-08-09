<!-- badges: start -->
[![](https://cranlogs.r-pkg.org/badges/grand-total/spsurvey)](https://cran.rstudio.com/web/packages/spsurvey/index.html)[![Travis build status](https://travis-ci.org/mhweber/spsurvey.svg?branch=master)](https://travis-ci.org/mhweber/spsurvey)
<!-- badges: end -->


# spsurvey

`spsurvey` is comprised of functions which implement algorithms for design and analysis of probability surveys.  The functions are tailored for Generalized Random Tessellation Stratified survey designs.

## Installation

You can install the released version from CRAN using:

```r
# install from CRAN
install.packages('spsurvey')
library(spsurvey)
```

You can install `spsurvey` from github with:

```r
# requires devtools to install
install.packages('devtools')
library(devtools)

# install from repository
install_github('USEPA/spsurvey')
library(spsurvey)
```

## Example
An overview of the spsurvey package is provided in the users guide that is included with the package.  The documentation includes a number of vignettes for the design and analysis of survey designs for point (finite), linear and area (polygons) spatial features.  Vignettes are also available for typical survey analyses.

## Citation
```r
citation(package = 'spsurvey')
```

```
## 
## To cite the spsurvey package in publications use:
## 
##   Kincaid, T. M. and Olsen, A. R. (2016). spsurvey: Spatial Survey
##   Design and Analysis. R package version 3.3.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {spsurvey: Spatial Survey Design and Analysis},
##     author = {Thomas M. Kincaid and Anthony R. Olsen},
##     year = {2016},
##     note = {R package version 3.3},
##   }
```

## Package Contributions
We encourage users to submit issues and enhancement requests so we may
continue to improve our package.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

### License

This project is licensed under the GNU General Public License, [GPL-2](https://cran.r-project.org/web/licenses/GPL-2).  
