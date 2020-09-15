This is a bug-fix release to specifically address issue:  
A new check in R-devel (part of --as-cran) looks for return without (): 
this is reported on the CRAN results pages for fedora-clang and fedora-gcc.


-------

## Test environments
* local x86_64-w64-mingw32/x64 (64-bit) R 4.0.2
* ubuntu 16.04.6 (on travis-ci), R R 4.0.2
* windows on R-hub - windows server 2008 R2 SP1, R-release, 32/64 bit
* Debian Linux on R-hub - Debian Linux, R-release, GCC

## R CMD check results

0 errors | 0 warnings | 0 notes

