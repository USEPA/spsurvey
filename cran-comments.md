This is a minor update adding two minor features and addressing a few bugs from version 5.2.0.

-------

## Resubmission

This is a resubmission. 

## Test environments

* Tests run on February 24, 2022 (rhub version 1.1.1)

* Build reports can be viewed through their hyperlinked Build ID URLs

* `rhub::check_for_cran()`
    * Platform: Windows Server 2022, R-devel, 64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-f750211f35ad44d7b1e61302a2a5b39c
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-7199ad2bdc6c4dc6b05cb4596cbf3004
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-7c098ab242434600aead79e1868d9025
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-ed36c5c25bb2439ba20a9c956ad323c8
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * SUCCESS (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-7a33f803708640fbabb5b7d0d4d6b01f
        
* `rhub::check_on_solaris()`
    * Platform: Oracle Solaris 10, x86, 32 bit, R-release
        * SUCCESS
            * `One ERROR: checking package dependencies ... ERROR Packages required but not available: 'sf', 'lme4'`
            * I am not sure how to diagnose / solve this error if sf and lme4 are not available on Solaris
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-c5500b5566414d24a5d8c00eaf4a3d0c
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.3.0.tar.gz-86095e6fe1cb49889fb3fef9c3b2bea2
        
## R CMD check results

Here is the output from `devtools::check(manual = TRUE)` on R Version 4.1.0,
devtools version 2.4.2, and Windows 10 x64 operating system

0 errors | 1 warnings | 0 notes

The warning: "WARNING qpdf is needed for checks on size reduction of PDFs". I
believe some compression happens while building the manual that is not recognized
in `devtools::check()`, as the `NOTE`s from rhub suggest the PDF size is approximately 
5.3 MB, but upon building the manual with `build_manual()`, the actual file size is
only 0.39 MB, well below the 1MB suggested limit for manuals. In short, it is my understanding that this
warning in `devtools::check()` (and `NOTE` in rhub builds) does not accurately
reflect the size of the final PDF manual installed upon package build.

## Downstream dependencies

This minor update should not affect any downstream dependencies (other than those impacted by the version 5.0.0 update).
