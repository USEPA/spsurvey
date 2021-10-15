This is a major update introducing several updates to pre-existing functions
Some of these changes are breaking; we alert users to this fact in the `NEWS.md` file
and in a startup message upon loading the package. In addition, some 
new functions were added.

-------

## Test environments
* Tests run on October 15, 2021 (rhub version 1.1.1)

* Build reports can be viewed through their hyperlinked Build ID URLs

* `rhub::check_for_cran()`
    * Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-2835f96162ea4b4fa9f946614b80775c
    * Platform: Fedora Linux, R-devel, clang, gfortran
        * Status: success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-3e7a8d16eba44f46befc13a83b2680a6
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-cbb896af25454f17a7eaf4aac0d72d76
        
* `rhub::check_on_windows()`
    * Platform: Windows Server 2008 R2 SP1, R-release, 32/64 bit
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-2c912c5200234b0e9f2d296457564757
        
* `rhub::check_on_linux()`
    * Platform: Debian Linux, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-ee578c071fb14bf982b487bfa4c07b89
        
* `rhub::check_on_ubuntu()`
    * Platform: Ubuntu Linux 20.04.1 LTS, R-release, GCC
        * success (NOTE related to manual PDF size -- see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-921468a2297d4721a23c49fddae151d0
        
* `rhub::check(platform = "macos-highsierra-release-cran")`
    * Platform: macOS 10.13.6 High Sierra, R-release, CRAN's setup
        * Status: SUCCESS (NOTE related to manual PDF size see next section)
        * Build ID: https://builder.r-hub.io/status/spsurvey_5.0.0.tar.gz-1bba837e167b4ae5b111ad5ac445a936
        
* GitHub actions (`usethis::use_github_action_check_standard()` also available [here](https://github.com/r-lib/actions/blob/master/examples/check-standard.yaml))
    * Platform: windows-latest (release), macOS-latest (release), ubuntu-20.04 (release), ubundo-20.04 (devel)
    * Build ID: GitHub actions R-CMD-check results for spsurvey are available [here](https://github.com/USEPA/spsurvey/actions/workflows/R-CMD-check.yaml). This push corresponds to 
    R-CMD-check 22 (news and cran comment updates) on October 15, 2021.

        
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

I have let the authors of packages that import and suggest spsurvey know that we 
are releasing a major update.
