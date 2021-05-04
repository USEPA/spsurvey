<!-- badges: start -->
[![](https://cranlogs.r-pkg.org/badges/grand-total/spsurvey)](https://cran.r-project.org/package=spsurvey)[![Travis build status](https://travis-ci.org/mhweber/spsurvey.svg?branch=master)](https://travis-ci.org/mhweber/spsurvey)[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/spsurvey)](https://cran.r-project.org/package=spsurvey)
[![cran checks](https://cranchecks.info/badges/worst/spsurvey)](https://cran.r-project.org/web/checks/check_results_spsurvey.html)
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

You can install development version of`spsurvey` from GitHub with the following:

```r
# requires devtools to install
install.packages('devtools')
library(devtools)

# install from repository
install_github('USEPA/spsurvey')
library(spsurvey)
```

To install from GitHub with package vignettes:
```r
library(devtools)
install_github('USEPA/spsurvey', build_vignettes=TRUE)
library(spsurvey)
```

## Example
An overview of the spsurvey package is provided in the users guide that is included with the package.  The documentation includes a number of vignettes for the design and analysis of survey designs for point (finite), linear, and areal spatial features.

## Citation
```r
citation(package = 'spsurvey')
```

```
## 
## To cite the spsurvey package in publications use:
## 
##   Kincaid, T. M., Olsen, A. R., Weber, M. H., and Dumelle, Michael (2021).
##   spsurvey: Spatial Survey Design and Analysis. R package
##  version 5.0.0.
## 
## A BibTeX entry for LaTeX users is
## 
##  @Manual{,
##     title = {spsurvey: Spatial Survey Design and Analysis},
##     author = {Thomas M. Kincaid and Anthony R. Olsen and Marc H. Weber and Michael Dumelle},
##     year = {2021},
##     note = {R package version 5.0.0},
##   }
```

## Package Contributions
We encourage users to submit issues and enhancement requests so we may
continue to improve our package.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

### License

This project is licensed under the GNU General Public License, [GPL-2](https://cran.r-project.org/web/licenses/GPL-2).  
