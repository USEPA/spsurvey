This is a major update introducing several updates to pre-existing functions
Some of these changes are breaking; we alert users to this fact in the News.md file
and in a startup message upon loading the package. In addition, some 
new functions were added.

-------

## Test environments

* Tests run on August 12, 2021 (rhub version 1.1.1)

* Build reports can be viewed through the URL
https://builder.r-hub.io/status/BUILD-ID

* `rhub::check_for_cran()`
    * Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-2b4375e21fa14b99a6f4c47ff7b3439d
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: Error -- It appears this is a bug with rhub and
        is not related to the spsurvey package build. The error message
        has been submitted as a bug report to rhub (thread located at 
        https://github.com/r-hub/rhub/issues/471)
        * Build ID: spsurvey_5.0.0.tar.gz-0fe8685b635c4d4cb2aaeb90fb1d065d
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-287543586bfa4e44b7ea2ae0a610e789
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-5ab4f28b53fb4a1298922583cc89e433
        
* `rhub::check(platform = "debian-clang-devel")`
    * Platform: Debian Linux, R-devel, clang, ISO-8859-15 locale
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: spsurvey_5.0.0.tar.gz-1ca2c7a7840b4b2cb9138fcb31f3a470

## R CMD check results

Here is the output from `devtools::check(manual = TRUE)` on R Version 4.1.0,
devtools version 2.4.2, and Windows 10 x64 operating system

0 errors | 1 warnings | 0 notes

The warning: "WARNING `qpdf` is needed for checks on size reduction of PDFs". I
believe some compression happens while building the manual that is not recognized
in `devtools::check()`, as the NOTEs from rhub suggest the PDF size is approximately 
5.2 MB, but upon building the manual with `build_manual()`, the actual file size is
only 0.39 MB, well below the 1MB suggested limit for manuals. In short, it is my understanding that this
warning in `devtools::check()` (and NOTE in rhub builds) does not accurately
reflect the size of the final PDF manual installed upon package build.

## Downstream dependencies

I have let the authors of packages that import and suggest spsurvey know that we 
are releasing a major update.

