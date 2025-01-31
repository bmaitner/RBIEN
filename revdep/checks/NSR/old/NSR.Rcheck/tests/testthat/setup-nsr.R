# api urls

# URL for NSR API
url <- "https://nsrapi.xyz/nsr_wsb.php" # production
#url <- "http://vegbiendev.nceas.ucsb.edu:9865/nsr_wsb.php" #testing

# Bad URLs for testing

# url <- "www.google.com"
# url <- "www.hisstank.com"

library("vcr") # *Required* as vcr is set up on loading

invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()
