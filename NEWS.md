# spsurvey 5.4.0 (2022-xx-xx)

## Minor Updates

* Added an `sp_frame()` function to create `sp_frame` objects for use with `plot()` and `summary()` for sampling frames and analysis data.
    * `sp_frame` objects have class `sp_frame`.
* Added an `sp_unframe()` function to transform `sp_frame` objects back into their original object type.
* Objects output from `grts()` and `irs()` are now called `sp_design` objects instead of `spdesign` objects
    * `sp_design` objects have class `sp_design`.
* `summary()` now works with `sp_frame` objects and `sp_design` objects.
    * `sp_summary()` yields equivalent summaries and is currently maintained for backwards compatibility (i.e., `sp_summary()` has not changed).
* `plot()` now works with `sp_frame` objects and `sp_design` objects.
    * `sp_plot()` yields equivalent plots and is currently maintained for backwards compatibility (i.e., `sp_plot()` has not changed).
 * `plot()` now works with `sp_CDF` objects that are output from the `CDF` element of `cont_analysis()`.
    * `cdf_plot()` yields equivalent plots and is currently maintained for backwards compatibility (i.e., `cdf_plot()` has not changed).   
* Updated print functions for summaries obtained via `summary()` or `sp_summary()` so that they are clearer and easier to read.
* Added a print function for `sp_design` objects. Now, a summary of site counts by site type (`Legacy`, `Base`, `Over` `Near`) crossed by strata or unequal probability levels or both is provided.
* Added `stratum_var`, `caty_var`, and `aux_var` elements to the design list in `sp_design` objects.
* Added `legacy_stratum_var`, `legacy_caty_var`, and `legacy_aux_var` elements to the design list in `sp_design` objects when legacy sites are used.
* Minor documentation updates.
* Minor vignette updates.

## Bug fixes

* Fixed a bug that prevented proper printing of the `Indicator` column when using `change_analysis()` with `test = median`.
* Fixed a bug that made `change_analysis` sensititve to the ordering of the levels of variables in `var_cat` if those variables were factors.
* Fixed a bug in `sp_summary()` that incorrectly ordered the `siteuse` variable.

# spsurvey 5.3.0 (2022-02-24)

## Minor updates

* Added a `projcrs_check` argument to `grts()` and `irs()`, which checks whether projected coordinates are required (`projcrs_check = TRUE`) or not (`projcrs_check = FALSE`).
* Nearest neighbor replacement sites (specified by `n_near`) in `grts()` or `irs()` are now also generated for each observation in `legacy_sites`.

## Bug fixes

* Fixed a bug in `attrisk_analysis()`, `diffrisk_analysis()`, and `relrisk_analysis()` that sometimes caused names in `response_levels` to not be found.
* Fixed a bug in `grts()` and `irs()` that returned an error when the name of the `geometry` column in `sframe` and `legacy_sites` differed. Now when this occurs, the `geometry` column in `legacy_sites` is renamed to have the same name as the geometry column in `sframe`.
* Fixed a bug in `grts()` and `irs()` that prevented `legacy_stratum_var`, `legacy_caty_var` and `legacy_aux_var` from performing properly.
* Fixed a bug in `grts()` and `irs()` that returned an error when at least one stratum in `legacy_sites` had zero observations.
* Fixed a bug in `grts()` and `irs()` that prevented `warnprnt()` from performing properly.
* Removed the warning in `grts()` and `irs()` that indicated when m or z values in `sframe` or `legacy_sites` were dropped. Now, the dropping of m or z values is explained in the documentation.
* Removed a warning in `grts()` and `irs()` that indicated when row names were set if `legacy_sites` was a tibble. Now, `legacy_sites` is coerced to a base R data frame (i.e., not a tibble) before setting row names.
* Added an error in `grts()` and `irs()` when `legacy_stratum_var` (and `legacy_caty_var` and `legacy_aux_var`) are `NULL` but the name of `stratum_var` in `sframe` is not contained in `legacy_sites`.
* Added an error in `grts()` and `irs()` that checks whether `sframe` and `legacy_sites` have the same crs.
* Updated documentation for `pt_density` in `grts()` and `irs()` to indicate that `pt_density` must be a positive integer.
* Fixed a bug in `sp_summary()` that returned an error when `formula = ~ .` and the geometry column was not named `"geometry"`. Now, `sp_summary()` works with `formula = ~ .` regardless of the name of the geometry column.
* Fixed a bug in `grts()` and `irs()` that affected minimum distance performance when legacy sites were used.

# spsurvey 5.2.0 (2021-01-23)

## Minor updates

* Made it easier in `grts()` and `irs()` to specify reverse hierarchically ordered replacement sites with unequal selection probabilities by requiring `n_over` be an integer and removing `caty_n_over`.
* Added an argument to `grts()` and `irs()` called `sep`, which acts as a separator between the character string created by `DesignID` and `SiteBegin` in the `sites` output.
* The `pt_density` argument in `grts()` and `irs()` now reflects the site multiplier (instead of the per-unit density of `sframe`) that generates the approximation to `sframe` when sampling from `LINESTRING`, `MULTILINESTRING`, `POLYGON`, or `MULTIPOLYGON` sf objects.

## Bug fixes

* Fixed a bug in `grts()` and `irs()` that prevented use when the geometry column of `sframe` was not named `"geometry"`.
* Fixed a bug in `grts()` and `irs()` that sometimes caused unequal selection probabilities to be misrepresented when reverse hierarchically ordered replacement sites were desired.
* Fixed a bug in `grts()` and `irs()` that sometimes prevented the dropping of an sf object's z or m dimension.
* Fixed a bug in `grts()` and `irs()` that sometimes incorrectly copied `stratum` values as `caty` values in the `sites` output.
* Fixed a bug in `grts()` and `irs()` that prevented sampling from `LINESTRING`, `MULTILINESTRING`, `POLYGON`, or `MULTIPOLYGON` sf objects when `pt_density` was excessively large.

# spsurvey 5.1.0 (2021-12-14)

## Minor updates

* Added a `Total` option to the `statistics` argument in `cont_analysis()`.
* Added `localmean_weight()`, `localmean_var()`, and `localmean_cov()` functions to compute the local neighborhood variance estimator outside of the `*_analysis()` functions.
* Added an option to provide a bounding box vector to the `fix_bbox` argument in `sp_plot()`.
* Added an error message to `grts() ` and `irs()` that stops the function when geographic coordinates are used.
* Added an error message to `grts()` and `irs()` that stops the function when too many expected samples are specified for at least one level of an unequal probability variable.
* Added a `caty_n_over` argument to `grts()` and `irs()` that makes it easier to specify reverse hierarchically ordered replacement sites for unequal probability sampling designs.
* Added vector argument support for the `n_over` and `n_near` arguments in `grts()` and `irs()` (list arguments were already supported).
* Added a default value for `siteID` in `attrisk_analysis()`, `cat_analysis()`, `cont_analysis()`, `diffrisk_analysis()`, and `relrisk_analysis()` that assumes each row in `dframe` represents a unique site.

## Bug fixes

* Fixed a bug in `irs()` that made the algorithm sensitive to the ordering of `sframe`.
* Fixed a bug in percentile estimation from `cont_analysis()` that incorrectly copied estimate values.
* Fixed a bug in `grts()` and `irs()` that prevented sampling from `sframe` when the geometry type was `LINESTRING`, `MULTILINESTRING`, `POLYGON`, or `MULTIPOLYGON` and the number of desired samples exceeded the number of rows.

# spsurvey 5.0.1 (2021-10-18)

## Bug fix

* Addressed Solaris performance problems.

# spsurvey 5.0.0 (2021-10-15)

## Major Updates

* Syntax for the `grts()` (Generalized Random Tessellation Stratified) and `irs()` (Independent Random Sampling) functions has been significantly simplified. These functions now require the sampling frame is an `sf` object.  

* The `grts()` and `irs()` functions now accommodate legacy (or historical) sites, a minimum distance between sites, and two approaches for selecting replacement sites (reverse hierarchical ordering and nearest neighbor).

* Syntax for the analysis functions (`*_analysis()`) has been significantly simplified. 

* Risk difference analysis is now available using the `diffrisk_analysis()` function.

* Trend analysis is now available using the `trend_analysis()` function.

* The `sp_summary()` function is now available to summarize sampling frames, design sites, and analysis data.

* The `sp_plot()` function is now available to visualize sampling frames, design sites, and analysis data.

## Minor Updates

* Several functions have undergone minor changes to syntax and scope.

## Breaking Changes

* Several functions have changed (both inputs and outputs) and functions in version 5.0.0 are not always backwards compatible with functions from previous versions. Though we recommend users upgrade existing code to work with the current version's simpler implementations, backwards compatibility can be achieved by downloading previous versions of spsurvey.

# spsurvey 4.1.4 (2020-9-15)

## Bug fix

* This is a bug-fix release to specifically address issue:  
* A new check in R-devel (part of --as-cran) looks for return without (): 
this is reported on the CRAN results pages for fedora-clang and fedora-gcc.

# spsurvey 4.1.3 (2020-6-15)

## Bug fix

* Fixed code so that sp design objects inherit CRS of input files and shapefiles written out also have inherited projection from input file. [(#2)](https://github.com/USEPA/spsurvey/issues/2)

* Variable sframe$len was used in grts function and never defined. Now uses length_mdm that is calculated earlier. [(#6)](https://github.com/USEPA/spsurvey/issues/6)

# spsurvey 4.1.2 (2020-03-31)

## Bug fix

* Update test data and Description file to handle changes in `sf` and changes to `crs` described [here](https://r-spatial.org/r/2020/03/17/wkt.html)

# spsurvey 4.1.1 (2019-12-12)

## Bug fix

* Fix to `localmean.weight` function to correct when class for matrix objects was causing an indicator variable to have length two rather than length one, which caused an error in a while statement.

# spsurvey 4.1.0 (2019-07-11)

## Breaking changes
* Functions that create survey designs were modified to replace use of shapefiles to provide the survey frame with use of simple features `sf` objects to provide the survey frame.  To accommodate this change, argument src.frame for functions `grts` and `irs` now will accept the choice `sf.object` to indicate that an `sf` object will be used as the source of the survey frame.  In addition, a new argument named `sf.object` was added to functions `grts` and `irs` to allow input of an `sf` object as the survey frame.  Also, note that arguments id, maxtry, and prjfilename for functions `grts` and `irs` have been depricated since they are no longer needed.  Finally, the three rda files in the data directory that contain survey frame objects (`NE_lakes`, `Luck_Ash_streams`, and `UT_ecoregions`) were revised to contain `sf` objects rather than objects belonging to classes that are defined in the `sp` package. 

* The package no longer contains any functions written in C.  The C functions were either no longer required due to the transition to use of `sf` objects to contain the survey frame or were replaced with functions written in R.  The new R functions are: `cellWeight`, `constructAddr`, `insideAreaGridCell`, `insideLinearGridCell`, `make_grid`, `numLevels`, `pickFiniteSamplePoints`, `pickGridCells`, `pickSamplePoints`, `ranho`, and `selectFeatureID`.

## Bug fixes
* The new R code function named `numLevels` that determines the number of hierarchical levels for a generalized random-tessellation stratified (GRTS) survey design now includes code to ensure that the maximum number of levels (which is currently 11) is not bypassed when creating a survey design.

## Modified function
* Modified function `input.check` to check for missing values among the x-coordinates and y-coordinates for location.

# spsurvey 4.0.0 (2019-04-04)

## New features

* Necessary changes were enacted so that the package can be built within RStudio, e.g., roxygen comments were added to R source files.

* Created functions that create panel designs and that compute and plot power for panel designs given a linear trend.  Function `revisit_dsgn` creates a panel revisit design.  Function `revisit_bibd` creates a balanced incomplete block panel revisit design.  Function `revisit_rand` creates a revisit design with random assignment to panels and time periods.  Function `panel_summary` summarizes characteristics of a revisit panel design.  Function `power.dsgn` calculates power for multiple panel designs.  Function `cov.panel.dsgn` creates the covariance matrix for a panel design.  Function `plot.powerpaneldesign` plots power curves for panel designs. 

## Bug fixes

* Modified C function `readDbfFile` to avoid PROTECT errors.

# spsurvey 3.4.0 (2018-06-12)

## Bug fixes

* Modified C functions `insideAreaGridCell`, `insideLinearGridCell`, `linSample`, `linSampleIRS`, `numLevels`, `pickAreaSamplePoints`, `pickLinearSamplePoints`, and `pointInPolygonFile` to avoid PROTECT errors.

* Modified function `change.analysis` to assign the support_2 variable from the design_2 data frame rather than from the design_1 data frame.

* Modified function `change.analysis` to use a revised procedure for ensuring that each repeated visit site is present in both surveys for subpopulation estimation.

* Modified C functions `writeShapeFilePoint` and `writeShapeFilePolygon` to correct memory access errors.

* Modified C function `readDbfFile` to ensure that missing values (NA) are handled correctly.

# spsurvey 3.3.0 (2016-08-19)

## New features

* Inserted a SystemRequirements field in the DESCRIPTION file and modified functions `grts`, `grtsarea`, `grtslin`, `grtspts`, `irs`, `irsarea`, `irslin`, `read.dbf`, and `read.shape` to prevent the functions from being executed on big-endian processors.

## Bug fixes

* Removed the C header file named `order.h` and replaced C functions `readBigEndian` and `readLittleEndian` with the version of those functions from `spsurvey` version 3.1.

# spsurvey 3.2.0 (2016-08-16)

## New features

* Created a vignette for change estimation.

* Modified functions `change.analysis` and `change.est` to include estimation for difference in median values for change estimation using continuous variables.

* Created a function named `examine` that examines variables in a data frame by printing either a table or the results of a call to the describe function in the `Hmisc` package.

## Bug fixes

* Modified function `sp2shape` to accommodate objects of class `SpatialDesign` created by either the `grts` or `irs` functions.

* Added a C header file named `order.h` that determines whether the CPU employs big-endian or little-endian byte order.  Also, modified C functions `readBigEndian` and `readLittleEndian` to use constants created by the header file to ensure that computer words are read in the correct byte order.

* Modified function `input.check` to ensure that use of numeric variables for arguments `stratum` and `cluster` are handled correctly when finite or continuous population correction factors are utilized.

* Modified function `cdf.test` to ensure upper bounds that define classes for the CDFs are unique. Also, modified functions `cont.cdftest` and `cdf.test` to reduce the minimum number of upper bounds that define classes for the CDFs from three to two.

* Modified functions `sp2shape` and `read.dbf` to ensure that missing values are handled without error for character vectors.

* Modified functions `grts` and `irs` to ensure that a `dataframe` object is assigned to argument `att.frame` when argument `src.frame` is assigned value `att.frame`.

* Modified function `change.analysis` to ensure that each repeated visit site is present in both surveys for subpopulation estimation.

* Modified function `read.sas` to use "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe" as the default value for argument `sascmd`.

# spsurvey 3.1.0 (2015-10-23)

## New features

* Modified vignettes to use data sets from the data directory for the package.

## Bug fixes

* Modified C functions to ensure that variables passed to function `malloc` are of type unsigned integer.

# spsurvey 3.0.0 (2015-05-21)

## New features

* Created a class named `SpatialDesign` that contains class `SpatialPointsDataFrame`, which is defined in package `sp`.  The class is used for the output objects created by functions `grts` and `irs`.  Objects of class `SpatialDesign` include the survey design list as a slot rather than as an attribute, which was used previously.

* Created S3 and S4 methods for generic functions `plot` and `summary` for objects of class `SpatialDesign`.

## Bug fixes

* Modified C functions to change the type declaration for variable `shpFileName` from `const char *` to `char * restrict`.

* Modified C functions `writeDbfFile`, `writeShapeFilePoint`, and `writeShapeFilePolygon` to change the return type from void to SEXP NULL, i.e., R_NilValue.

* Modified function `input.check` to remove missing values from final adjusted weights prior to checking that all values are positive.

* Modified functions `grtspts`, `grtslin`, and `grtsarea` to ensure that argument `nlev` (number of hierarchical levels) for  C function `constructAddr` was stored as type integer.

# spsurvey 2.7.0 (2014-06-26)

## New features

* Added a `cex.main` argument to the `cont.cdfplot` and `cdf.plot` functions.

## Bug fixes

* Modified function `warnprnt` to use correct variable names in the output data frame so that partial matching warnings for the names are not generated.

# spsurvey 2.6.0 (2013-09-20)

## New features

* Created a data directory that contain rda versions of the data files used by vignettes.

## Bug fixes

* Modified function `change.analysis` to allow analysis of surveys with no repeat visit sites. 

* Modified function `change.est` to ensure that levels for a categorical response variable are listed in the desired order rather than sorted order.

* Modified function `localmean.weight` to allow recovery from an error in the singular value decomposition function `La.svd` that is called by the generalized inverse function `ginv` in the `MASS` package. Also, created a support function named `localmean.weight2`.

# spsurvey 2.5.0 (2012-10-10)

## New features

* Modified function `grtspts` and C function `numLevels` to calculate the sampling grid random shift values only once rather than each time the number of hierarchical levels is incremented.

## Bug fixes

* Modified functions `attrisk.analysis`, `cat.analysis`, `change.analysis`, `cont.analysis`, and `relrisk.analysis` to replace NA values with FALSE in the indicator variables for subpopulations.

* Modified function `spbalance` to include all grid cells with nonzero extent for the frame when calculating the Pielou evenness measure for a rectangular grid.

* Modified function `localmean.weight` to ensure that the initial set of weights are inversely proportional to distance.

* Modified functions `cont.cdftest` and `cdf.test` to ensure that the number of classes into which the CDFs will be divided (binned) is at least three.

* Modified function `dframe.check` to ensure proper handling of repeated site ID values in the sites data frame.

# spsurvey 2.4.0 (2012-05-23)

## New features

* Created a vignette that presents deconvolution of a cumulative distribution function (CDF) generated by a GRTS survey design.

* Created a function named `spbalance` that calculates spatial balance metrics for a survey design.

* Created functions named `sbcframe` and `sbcsamp` that calculate spatial balance grid cell extent and proportions for a sample frame and for a survey design, respectively.

* Modified function `change.est` to include calculation of resource size change estimates for categorical variables.
 
* Created a function named `changevar.size` to calculate covariance or correlation estimates of the estimated change in class resource size estimates between two probability surveys.

## Bug fixes

* Modified function `change.est` to correct errors in the output values for categorical variables from survey designs that lack repeat visit sites.
 
* Modified the following functions to assign consecutive numbers to the row names for the output data frame(s): `attrisk.analysis`, `cat.analysis`, `change.analysis`, `cont.analysis`, `cont.cdftest`, and `relrisk.analysis`.

# spsurvey 2.3.0 (2012-02-03)

## New features

* Created a function named `change.analysis` that conducts change analysis for a collection of response variables (categorical and continuous) generated by two probability surveys.

* Created functions named `change.est`, `changevar.prop`, and `changevar.mean` to calculate estimates of change and its associated variance.

* Created a vignette that presents cumulative distribution function (CDF) analysis of a GRTS survey design.

* Added a function named `ash1.wgt` that computes the average shifted histogram (ASH) for weighted data.

## Bug fixes

* Modified function `grtspts` to correct an error that occurs when argument `src.frame` equals `att.frame` and the number of hierarchical levels equals eleven.

* Modified C functions `printAddrList`, `printColCharList`, `code{printDbf` to replace calls to the `printf` function with calls to the `Rprintf` function.

* Modified C function `pickAreaSamplePoints` to correct an error that occurs when selecting sample points for PolygonZ and PolygonM type shapefiles.

* Created an `.onAttach` function that prints a message when the `spsurvey` package is attached.  Removed the `.onLoad` function, which prior versions used to to print a startup message.

# spsurvey 2.2.0 (2011-05-16)

## New features

* In order to reduce package size, removed demonstration (demo) R scripts and the associated data directory.

* Created vignettes that present analyses of GRTS survey designs for finite, linear, and areal resources.

* Created a function named `attrisk.analysis` that conducts attributable risk analysis for a collection of response variables generated by a probability survey.

* Created functions named `attrisk.est` and `attrisk.var` to calculate estimates of attributable risk and its associated variance.

* Modified function `relrisk.est` to change the way the relative risk estimate is calculated for a stratified sample.

* Modified functions `cat.analysis`, `cont.analysis`, `cont.cdftest`, `relrisk.analysis`, `category.est`, `cdf.est`, `cdf.decon`, `total.est`, `cdf.test`, `relrisk.est`, `catvar.prop`, `catvar.size`, `cdfvar.prop`, `cdfvar.total`, `cdfvar.size.prop`, `cdfvar.size.total`, `dcdfvar.prop`, `dcdfvar.total`, `dcdfvar.size.prop`, `dcdfvar.size.total`, `total.var`, `cdfvar.test`, and `relrisk.var` to allow variance estimates to be calculated when a two stage sample has stage one sampling units that contain a single stage two sampling unit.  Variance for those stage one sampling units is calculated using the mean of the variance estimates for stage one sampling units that contain two or more stage two sampling units.

* Modified function `grtslin` to improve efficiency of sample point selection.  Created a C function named `insideLinearGridCell` that, for each grid cell selected to receive a sample point, returns the ID value for shapefile records contained in the cell and the clipped length of the polyline segments within the cell for each record.  Created a C function named `pickLinearSamplePoints` that selects sample points.

* Modified function `grtsarea` to improve efficiency of sample point selection.  Created a C function named `insideAreaGridCell` that, for each grid cell selected to receive a sample point, returns the ID value for shapefile records contained in the cell and the clipped area of the polygon within the cell for each record.  Created a C function named `pickAreaSamplePoints` that selects sample points.

## Bug fixes

* Modified function `input.check` to include determination of whether a valid value was provided for argument `vartype`.

* Modified functions `grts` and `irs` so that correct survey design weights are created for linear and areal designs when the type of random selection is "continuous".

* Modified functions `grtspts`, `grtslin`, and `grtsarea` to execute without error for a stratum sample size of one.

* Modified function `dframe.check` to terminate execution when
 missing values are encountered in the logical variable of the sites data frame.
 
# spsurvey 2.1.0 (2009-10-29)

## New features

* Created vignettes that present examples of GRTS survey designs for finite, linear, and areal resources.

* Created a function named `geodalbers` that projects latitude and longitude (spheroid) models of the globe to Albers projection in the plane.

* Modified function `dsgnsum` to produce summary tables for the actual set of design variables that are present rather than a standard set of design variables.

* Modified function `dframe.check` to terminate execution when missing site ID values or repeated site ID values are encountered in the `sites`, `design`, `subpop`, `data.cat`, `data.cont`, or `data.rr` data frames.  Note that `dframe.check` is called by functions `cat.analysis`, `cont.analysis`, `cont.cdftest`, and `relrisk.analysis`.

## Bug fixes

* Modified function `cont.cdftest` to correct an error caused by improper handling of an empty subpopulation.

* Modified function `relrisk.analysis` to correct an error resulting from inclusion of more than one response variable in the
`response.var` argument.

# spsurvey 2.0.0 (2008-06-16)

## New features

* Eliminated use of argument `unitsize` (known sum of size-weights) by package functions.  Restricted argument `popsize` to provide only the known size of the resource for use in ratio adjustment of estimators.  Created a new argument named `pcfsize` to provide resource size for calculation of finite and continuous population correction factors for single-stage samples.

* Modified functions `cat.analysis`, `cont.analysis`, `cont.cdftest`, and `relrisk.analysis` to add logical variables to their argument lists that specify use of finite or continuous population correction factors or use of size-weights in analysis routines.

* Modified functions `category.est`, `cdf.decon`, `cdf.est`, `cdf.test`, `relrisk.est` and `total.est` to add logical variables to their argument lists that specify use of finite or continuous population correction factors or use of size-weights.

## Bug fixes

* Modified function `irs` to eliminate checking for existence of x-coordinates and y-coordinates when the type of frame is "finite" and the frame is included in the `att.frame` data frame.

* Modified C functions `parseFields` and `readDbfFile` to ensure that blank values in the shapefile attributes (dbf) file are converted to R missing values (NA).

* Modified C function `writeDbfFile` to output blank values for R missing values (NA) when creating the shapefile attributes (dbf) file.

* Modified functions `grts`, `irs`, and `sp2shape` to ensure that an output shapefile attributes (dbf) file containing character variables with missing values can be read without error.

# spsurvey 1.7.0 (2007-11-09)

## New features

* Created a function named `cont.cdftest` that tests for differences between cumulative distribution functions (CDFs) for pairs of subpopulations within a population `Type` for a collection of response variables generated by a probability survey.  Also, modified function `cdf.test` so that it is consistent with functions that are called by high-level functions `cat.analysis` and `cont.analysis`.

* Modified function `grtspts` so that, when source of the frame equals "att.frame", the current number of grid levels for hierarchical randomization and the final number of grid levels is printed to the console while the function is executing.

* Created a function named `relrisk.analysis` that conducts relative risk analysis for a collection of response variables generated by a probability survey.  Also, renamed function `relrisk` to `relrisk.est` and modified the function so that it is consistent with functions that are called by high-level functions `cat.analysis` and `cont.analysis`.

* Modified function `grtspts` and C function `numLevels` to terminate the algorithm for determining the number of grid levels for hierarchical randomization when the maximum value of total inclusion probability for the grid cells stops changing as the number of levels increases.

* Created functions named `cdf.plot` and `cont.cdfplot` that create cumulative distribution function (CDF) plots using the data frame named "CDF" contained in the output object created by function `cont.analysis`.  Function `cdf.plot` creates a single CDF plot, and function `cont.cdfplot` creates a set of CDF plots.

* Created a function named `read.sas` that can read SAS datasets or a SAS XPORT (transport) file.

## Bug fixes

* Modified C functions `intersect`, `linSampleIRS`, `lintFcn`, and `linSample` to eliminate warning messages that were generated during package creation.

* Modified function `grts` so that argument `do.sample` provides a value for each stratum.

* Modified C functions `getRecordShapeSizes`, `readDbfFile`, `parsePoints`, `parsePointsZ`, `parsePointsM`, `parsePolygon`, `parsePolygonZ`, and `parsePolygonM` to generate error messages and terminate execution when a shapefile containing a Null record is encountered.

* Modified functions `irslin` and `irsarea` in addition to C function `getRecordIDs` to ensure that sample points are selected in random order for linear and areal IRS designs.

* Modified function `grts` to ensure that, when the type of random selection is "unequal", an oversample is apportioned correctly whenever the category sample sizes are proportional to the oversample size.

# spsurvey 1.6.0 (2007-01-18)

## New features

* For C functions that read shapefiles from the current directory, replaced calls to `_findfirst` and `_findnext` with code using calls to `readdir`.  These changes were implemented to facilitate portability of the package.

* Created a C function named `matchFiles` that determines whether file names in the current directory have a desired file extension.  This change was implemented to facilitate portability of the package.

## Bug fixes

* Modified functions `grts` and `irs` to accommodate use of a factor for the ID variable in the attributes data frame.

* Modified functions `grts` and `irs` to ensure that sample weights are correctly adjusted when an oversample is present and the type of random selection is "Continuous".

# spsurvey 1.5.0 (2006-12-06)

## Bug fixes

* Modified C functions `getRecordShapeSizes` and `lintFcn` to accommodate Polyline shapefiles that have multiple parts.

# spsurvey 1.4.0 (2006-10-10)

## New features

* Modified functions `dsgnsum` and `sp2shape` to accommodate the change in representation from `AttributeList` to `data.frame` for the data slot of sp package objects belonging to class `SpatialPointsDataFrame`.

* Modified functions `grts` and `irs` to print a warning message when the type of frame equals "finite" and a stratum name in the design list matches only a single value in the stratum column of the attributes data frame.  For this case, function `grtspts` or `irspts` is not called since the sample will be composed of a single point.

* Modified functions `grts`, `grtspts`, `grtslin`, and `grtsarea` to change the maximum value allowed for arguments startlev (the initial number of hierarchical levels to use for the GRTS grid) and maxlev (the maximum number of hierarchical levels to use for the GRTS grid) from 12 to 11.

* Added an example polylines dataset to the data directory.

* Modified functions `grts` and `irs` to allow use of an sp package object as the source of the frame.  An argument named `sp.object` was added to the argument list for `grts` and `irs`.

* Modified functions `grts`, `grtspts`, `grtslin`, `grtsarea`, `irs`, `irspts`, `irslin`, and `irsarea` to remove use of argument `xy.frame` as an option for source of the frame.  Then modified functions `grts`, `grtspts`, `irs`, and `irspts` to allow incorporation of frame coordinates into the attributes data frame when the type of frame equals "finite".  Also, removed argument `elmsize` from functions `grts` and `irs` since the argument no longer was required.

## Bug fixes

* Modified functions `grts` and `irs` to print a warning message when the type of frame equals "finite" and a stratum name in the design list matches only a single value in the stratum column of the attributes data frame.  For this case, function `grtspts` or `irspts` is not called since the sample will be composed of a single point.

* Modified functions `grts` and `irs` to ensure that the ID values for elements of the frame provided in att.frame are unique.

* Modified functions `grts` and `irs` to ensure that valid values are provided for the `panel` and `caty.n` arguments in the design list.

# spsurvey 1.3.0 (2006-08-01)

## New features

* Added an example polygons dataset to the data directory.

* Incorporated the CHANGES, README, and UserGuide files into the help page.

# spsurvey 1.2.0 (2006-06-27)

## New features

* Created a function named `sp2shape` and a C function named `writeShapeFilePolygon` that convert objects created by package `sp` to ESRI shapefiles.  Also, renamed the C function `writeShapeFile` to `writeShapeFilePoint`.

## Bug fixes

* Modified function `irsarea` and created a C function named `getRecordIDs` to ensure that an IRS sample is selected when argument `type.frame` is set to "area" in function `irs`.

# spsurvey 1.1.0 (2006-05-31)

## New features

* Modified the C functions so that the package can accommodate M-type shapefiles.

## Bug fixes

* Modified functions `grts` and `irs` to ensure that the levels of `mdcaty` (the variable in the attributes data frame that specifies the unequal probability category for each element in the frame) are equivalent to the names in `caty.n` (the vector of sample sizes for each category in `mdcaty`, which is required for each element of the design list for which the selection type is "Unequal").

* Modified functions `grts` and `irs` to ensure that the columns of `xy.frame` are named "x" and "y" when `xy.frame` is provided and `type.frame` equals "finite".

* Modified functions `grts` and `irs` so that the sample weights are correctly adjusted when an oversample is requested and the realized sample size is less than the desired sample size.

# spsurvey 1.0.0 (2006-05-05)

## New features

* This is the original version of the package.
