## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.1.2
* win-builder (devel and release)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

## Notes
* Fedora Linux testing fails due to a problem compiling ‘RPostgreSQL’
* This update removes dependencies on retiring packages rgdal, rgeos, and maptools.
* This update replaces dependencies on raster and sp with dependencies on terra and sf (where possible).

## Response to previous comments
