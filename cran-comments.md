## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.4.2
* win-builder
* macOS builder
* ubuntu-latest on GitHub
* macos-13 on GitHub
* macos-latest on GitHub
* windows-latest on GitHub

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

## Notes
* This update fixes a problem where a failure to connect to the database results in an error (rather than failing gracefully)
* This update also addresses a NOTE related to the use of |> pipes in the code

## Response to previous comments
