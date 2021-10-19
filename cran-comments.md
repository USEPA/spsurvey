This is a minor update addressing Solaris performance problems.

-------

## Test environments

* Tests run on October 18, 2021 (rhub version 1.1.1)

* Build reports can be viewed through their hyperlinked Build ID URLs

* `rhub::check_for_cran()`
    * Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-a4d656b6638b424ebe6d856cbafe69b5
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-5605a60bc9a244c9a98ec169dc2a6fc5
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-a3841f544902438cb731c8399e582bc5
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-54655dd37aa64eec800ba369ce921ecf
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-cf184d638b814ac3a2770048fdbc2645
        
* `rhub::check_on_solaris()`
    * Platform: Oracle Solaris 10, x86, 32 bit, R-release
        * success (NOTE related to panual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-4b382f8214e24e3ab589d463fc93c417 
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.1.tar.gz-f1f08046f1894225a104b6020907d7b1
        
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

This minor update should not affect any downstream dependencies (apart from those impacted by the release of version 5.0.0)
