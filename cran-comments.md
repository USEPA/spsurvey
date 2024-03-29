This is a minor update that adds a few bug fixes.

-------

## Resubmission

This is a resubmission. 

## R CMD check results

Here is the output from `devtools::check(manual = TRUE)` on R Version 4.2.2,
devtools version 2.4.5, and Windows 10 x64 operating system

0 errors | 1 warnings | 0 notes

The warning: "WARNING qpdf is needed for checks on size reduction of PDFs". I
believe some compression happens while building the manual that is not recognized
in `devtools::check()`, as the `NOTE`s from rhub suggest the PDF size is approximately 
5.3 MB, but upon building the manual with `build_manual()`, the actual file size is
only 0.39 MB, well below the 1MB suggested limit for manuals. In short, it is my understanding that this
warning in `devtools::check()` (and `NOTE` in rhub builds) does not accurately
reflect the size of the final PDF manual installed upon package build.

I have also checked the build on `win_devel` and received a NOTE about a possibly invalid URL. This URL, as best I can tell, is valid.

## Downstream dependencies

This minor update should not affect any downstream dependencies.
