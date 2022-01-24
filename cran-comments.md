This is a minor update adding some minor features and addressing a few bugs from version 5.1.0.

-------

## Resubmission

This is a resubmission. 

## Test environments

* Tests run on January 23, 2022 (rhub version 1.1.1)

* Build reports can be viewed through their hyperlinked Build ID URLs

* `rhub::check_for_cran()`
    * Platform: Windows Server 2022, R-devel, 64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-c6b1bfcfe0c840baabf92da7ad8dd917
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-c55be11f9571488e8077ece3525d69c4
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-6f693f8d04f74cde8bbf7df801ed86a6
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-a601356c2761418788a1ca4811d8bb66
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-804ac4ab544a43d491cbb3a6538a17d9
        
* `rhub::check_on_solaris()`
    * Platform: Oracle Solaris 10, x86, 32 bit, R-release
        * success (NOTE related to panual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-9ab796fc87b2412e9f5c87f53fd2172f
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.2.0.tar.gz-1cf4cfe699d94b2592a8f003b2e31013
        
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
