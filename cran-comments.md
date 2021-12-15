This is a minor update adding some minor features and addressing a few bugs from version 5.0.0.

-------

## Test environments

* Tests run on December 14, 2021 (rhub version 1.1.1)

* Build reports can be viewed through their hyperlinked Build ID URLs

* `rhub::check_for_cran()`
    * Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-f94189460edb432eb16497e6d7e812b7
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-2fad020053b14838afb28be439a447a6
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-d8416a33d90c4da9bafd2d36a2eb410d
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-fe41ebfe984241479f8c54a4997c5bb3
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-7320ee0635704b84bdf6e7060c52468f
        
* `rhub::check_on_solaris()`
    * Platform: Oracle Solaris 10, x86, 32 bit, R-release
        * success (NOTE related to panual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-e5287e26e0fb43b5a1d4a2f221ff4dd7 
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.1.0.tar.gz-cde17faf7d7542b98e6d685a39ad847d
        
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

This minor update should not affect any downstream dependencies.
