<!-- badges: start -->
[![R-CMD-check](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml)
[![CRAN](http://www.r-pkg.org/badges/version/spsurvey)](https://cran.r-project.org/package=spsurvey)
[![cran checks](https://cranchecks.info/badges/worst/spsurvey)](https://cran.r-project.org/web/checks/check_results_spsurvey.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/spsurvey)](https://cran.r-project.org/package=spsurvey)
<!-- badges: end -->

# spsurvey

spsurvey is an R package that implements a design-based approach to statistical inference,
with a focus on spatial data.
Spatially balanced samples are selected using the
Generalized Random Tessellation Stratified (GRTS) algorithm.
The GRTS algorithm can be applied to finite resources (point geometries) and
infinite resources (linear / linestring and areal / polygon geometries) and flexibly
accommodates a diverse set of sampling design features, including
stratification, unequal inclusion probabilities, proportional (to size)
inclusion probabilities, legacy (historical) sites, a minimum distance between
sites, and two options for replacement sites (reverse hierarchical order and
nearest neighbor). Data are analyzed using a wide
range of analysis functions that perform categorical variable analysis, continuous
variable analysis, attributable risk analysis, risk difference analysis, relative
risk analysis, change analysis, and trend analysis. spsurvey can also be used to
summarize objects, visualize objects, select samples that are not spatially balanced,
select panel samples, measure the amount of spatial balance in a sample,
adjust design weights, and more.

## Installation

You can install and load the most recent approved version from CRAN by running

```r
# install the most recent approved version from CRAN
install.packages("spsurvey")
# load the most recent approved version from CRAN
library(spsurvey)
```

You can install and load the most recent development version of`spsurvey` from GitHub by running:

```r
# Installing from GitHub requires you first install the remotes package
install.packages("remotes")

# install the most recent development version from GitHub
remotes::install_github("USEPA/spsurvey", ref = "main")
# load the most recent development version from GitHub
library(spsurvey)
```

You can install the most recent development version of `spsurvey` from GitHub with package vignettes by running:
```r
install the most recent development version from GitHub with package vignettes
devtools::install_github("USEPA/spsurvey", build_vignettes=TRUE)
```

To view the vignettes in RStudio, run
```r
vignette("start-here", "spsurvey") # start with this vignette for an spsurvey overview
vignette("EDA", "spsurvey") # for summaries and visualizations (exploratory data analysis)
vignette("sampling", "spsurvey") # for spatially balanced sampling
vignette("analysis", "spsurvey") # for analyzing data
```

To view the vignettes in a web format, visit [here](https://cran.r-project.org/package=spsurvey).

Further detail regarding spsurvey is contained in the package's documentation manual available for download [here](https://cran.r-project.org/package=spsurvey).


## Citation

If you used spsurvey in your work, please cite it. You can view the most recent citation by running
```r
citation(package = "spsurvey")
```

```
#> To cite the spsurvey package in publications use:
#> 
#>   Dumelle, Michael., Kincaid, T. M., Olsen, A. R., and Weber, M. H. (2022). spsurvey:
#>   Spatial Sampling Design and Analysis. R package version 5.3.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {spsurvey: Spatial Sampling Design and Analysis},
#>     author = {Michael Dumelle and Thomas M. Kincaid and Anthony R. Olsen and Marc H. Weber},
#>     year = {2022},
#>     note = {R package version 5.3.0},
#>   }
```

## Package Contributions

We encourage users to submit issues and enhancement requests so we may
continue to improve spsurvey.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

### License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).  
