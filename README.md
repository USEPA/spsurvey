# spsurvey <img src="man/figures/logo.png" align="right" height="130" alt="" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN](http://www.r-pkg.org/badges/version/spsurvey)](https://cran.r-project.org/package=spsurvey)
[![cran checks](https://badges.cranchecks.info/worst/spsurvey.svg)](https://cran.r-project.org/web/checks/check_results_spsurvey.html)
[![R-CMD-check](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/spsurvey)](https://cran.r-project.org/package=spsurvey)

<!-- badges: end -->

# Overview

`spsurvey` is an R package that implements a design-based approach to statistical inference,
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
risk analysis, change analysis, and trend analysis. `spsurvey` can also be used to
summarize objects, visualize objects, select samples that are not spatially balanced,
select panel samples, measure the amount of spatial balance in a sample,
adjust design weights, and more. For additional details, see Dumelle et al. (2023) <doi:10.18637/jss.v105.i03>.

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

Further detail regarding `spsurvey` is contained in the package's documentation manual available for download [here](https://cran.r-project.org/package=spsurvey).


## Citation

If you used `spsurvey` in your work, please cite it. You can view the most recent citation by running
```r
citation(package = "spsurvey")
```

```
#> To cite spsurvey in publications use:
#> 
#>   Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023).
#>   spsurvey: Spatial Sampling Design and Analysis in R. Journal of
#>   Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {{spsurvey}: Spatial Sampling Design and Analysis in {R}},
#>     author = {Michael Dumelle and Tom Kincaid and Anthony R. Olsen and Marc Weber},
#>     journal = {Journal of Statistical Software},
#>     year = {2023},
#>     volume = {105},
#>     number = {3},
#>     pages = {1--29},
#>     doi = {10.18637/jss.v105.i03},
#>   }
```

## Package Contributions

We encourage users to submit issues and enhancement requests so we may
continue to improve `spsurvey`.

## Acknowledgements

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.


A special thank you to Virginia Vichi-Miller for creating the `spsurvey` hex sticker (i.e., graphic identifier). The sticker represents the environmental sampling context for which `spsurvey` was created, with an emphasis on aquatic resources. The sticker shows a sunny day filled with a fish swimming in water, aquatic vegetation, and rolling hills and trees. The word `spsurvey` is set against the backdrop of the clear sky.

### License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).  
