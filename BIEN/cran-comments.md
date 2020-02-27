## Test environments
* local OS X install, R 3.6.2
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √

## Reverse dependencies

* I have run R CMD check on the downstream dependencies of BIEN.
  (Summary at https://github.com/bmaitner/RBIEN/tree/master/revdep)
  All packages passed the R CMD check.
  
## Notes
* I have corrected the issues in the previous CRAN package check results:Documented arguments not in \usage in documentation object 'BIEN_metadata_match_data': '...'
* winbuilder check OK
* ubuntu checks failed because of issues unrelated to my code, e.g. an outdated version of GDAL that is installed: 2262#> configure: error: upgrade GDAL to 1.11.4 or later
* since the checks seemed to revovle around problems in the ubuntu environment used for testing (outdated GDAL) rather than errors in BIEN, I'm pushing the update to CRAN despite the errors.
* I have corrected the issues :Documented arguments not in \usage in documentation object 'BIEN_phylogeny_label_nodes': '...'

