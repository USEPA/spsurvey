<!-- badges: start -->
[![R-CMD-check](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml)
[![CRAN](http://www.r-pkg.org/badges/version/spsurvey)](https://cran.r-project.org/package=spsurvey)
[![cran checks](https://cranchecks.info/badges/worst/spsurvey)](https://cran.r-project.org/web/checks/check_results_spsurvey.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/spsurvey)](https://cran.r-project.org/package=spsurvey)
<!-- badges: end -->

# spsurvey

spsurvey is an R package for spatial survey design and analysis.  Spatially balanced sampled are selected using the Generalized Random Tessellation Stratified (algorithm) via the `grts()` function. Functions also exist to perform categorical variable analysis, continuous variable analysis, relative risk analysis, attributable risk analysis, risk difference analysis, change analysis, and trend analysis.

## Installation

You can install the released version from CRAN using:

```r
# install from CRAN
install.packages("spsurvey")
library(spsurvey)
```

You can install development version of`spsurvey` from GitHub with the following:

```r
# requires devtools to install
install.packages("devtools")

# install from repository
devtools::install_github("USEPA/spsurvey")
library(spsurvey)
```

To install from GitHub with package vignettes:
```r
devtools::install_github("USEPA/spsurvey", build_vignettes=TRUE)
library(spsurvey)
```

## Example
An overview of spsurvey is provided through the package's vignettes. These can be viewed by running `vignette(package = "spsurvey")` after installing spsurvey from CRAN or from GitHub (when `build_vignettes = TRUE`). Further detail is contained in the package's documentation manual.

## Citation
```r
citation(package = "spsurvey")
```

```
#> To cite the spsurvey package in publications use:
#> 
#>   Dumelle, Michael., Kincaid, T. M., Olsen, A. R., and Weber, M. H. (2021). spsurvey:
#>   Spatial Survey Design and Analysis. R package version 5.0.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {spsurvey: Spatial Survey Design and Analysis},
#>     author = {Michael Dumelle and Thomas M. Kincaid and Anthony R. Olsen and Marc H. Weber},
#>     year = {2021},
#>     note = {R package version 5.0.0},
#>   }
```

## Package Contributions
We encourage users to submit issues and enhancement requests so we may
continue to improve our package.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

### License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).  
