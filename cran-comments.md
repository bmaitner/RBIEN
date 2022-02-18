## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.1.2
* win-builder (devel and release)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

## Notes
* Package was archived due to a change in the database that caused errors in tests.
* I've updated the code so that errors due to database changes or outage now fail gracefully. This required moving from using DBI::dbGetQuery() for connection (which doesn't allow for graceful error handling), to using DBI::dbFetch(), which allows for better error handling.
* Connection issues now print a message and return NULL (invisibly.)
*The Fedora Linux environment on rhub failed because "ERROR: dependency ‘RPostgreSQL’ is not available for package ‘BIEN’""

## Response to previous comments
* We fixed the outdated links in our documentation (caused by transitions between http to https)

>On M1mac, we see lots of segfaults in PostgreSQL.
>So far OK up to the vignettes, which hang for 20m then complete.

>Fedora-clang: ditto (for 27m).

>https://www.top-thesaurus.org/ has a certificate problem, 'expired on
>16/09/2021.'

>Can you look for the long hangs in the vignettes?

* The vignette build times may have been slow due to server load.  I've re-run them a few times locally and it takes approximately 2.5 minutes to build both vignettes per system.time(build_vignettes())

* Re: the certificate problem with https://www.top-thesaurus.org/: we don't maintain this website so we cannot fix this issue directly. By way of a solution, I've updated the vignette so that a hyperlink won't be generated and the link will be rendered as plain text.

