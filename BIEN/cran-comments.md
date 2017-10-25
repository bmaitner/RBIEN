## Test environments
* local OS X install, R 3.3.3
* ubuntu 12.04 (on travis-ci), R 3.3.3
* win-builder (2017-07-30 r73000)
 
## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

* I have run R CMD check on the downstream dependencies of BIEN.
  (Summary at https://github.com/bmaitner/RBIEN/tree/master/revdep)
  All packages passed the R CMD check.
  
## Notes

* I received the following message regarding the previous version:


Unfortunately, this leaves files behind:

Rtmp82Z0AM\selaginella_selaginoides.bib
Rtmp82Z0AM\selaginella_selaginoides.txt

in /tmp/

on non Windows systems as you use


./BIEN/vignettes/BIEN_tutorial.Rmd: bibtex_file = paste(tempdir(),"\\","selaginella_selaginoides.bib",sep=""),
./BIEN/vignettes/BIEN_tutorial.Rmd: acknowledgement_file=paste(tempdir(),"\\","selaginella_selaginoides.txt",sep=""))

Please use file.path() to construct the paths independent of the OS.

Please fix and resubmit.


Best,
Uwe Ligges


*  I have updated the code in the vignettes and examples to use file.path()

* I have updated the offending code in the vignette with `eval=FALSE` so as to not leave behind files.

* Apologies for this, I know your time is valuable