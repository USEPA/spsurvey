This is a major update introducing several updates to pre-existing functions
Some of these changes are breaking; we alert users to this fact in the `NEWS.md` file
and in a startup message upon loading the package. In addition, some 
new functions were added.

-------

## Test environments
* Tests run on October 05, 2021 (rhub version 1.1.1)

* Build reports can be viewed through the URL
https://builder.r-hub.io/status/BUILD-ID

* `rhub::check_for_cran()`
    * Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-8c25d5077fb146e4a1827f84dce01010
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-ce7f0efd0e274071b46d4922639c6330
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-b72631e93cca4d31a343c66ce57e8c3d
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * BUild ID: spsurvey_5.0.0.tar.gz-4ecfbae06897473f986d363642fd8a62
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * BUild ID: spsurvey_5.0.0.tar.gz-9b45d3ede2a84d67bd0fdee13d197f07
        
* `rhub::check_on_ubuntu()`
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * BUild ID: spsurvey_5.0.0.tar.gz-eedffb8a0361405faa9c86fd7eda5570
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-5ab4f28b53fb4a1298922583cc89e433
        
## R CMD check results

Here is the output from `devtools::check(manual = TRUE)` on R Version 4.1.0,
devtools version 2.4.2, and Windows 10 x64 operating system

0 errors | 1 warnings | 0 notes

The warning: "WARNING `qpdf` is needed for checks on size reduction of PDFs". I
believe some compression happens while building the manual that is not recognized
in `devtools::check()`, as the `NOTE`s from rhub suggest the PDF size is approximately 
5.2 MB, but upon building the manual with `build_manual()`, the actual file size is
only 0.39 MB, well below the 1MB suggested limit for manuals. In short, it is my understanding that this
warning in `devtools::check()` (and `NOTE` in rhub builds) does not accurately
reflect the size of the final PDF manual installed upon package build.

## Downstream dependencies

I have let the authors of packages that import and suggest spsurvey know that we 
are releasing a major update.
