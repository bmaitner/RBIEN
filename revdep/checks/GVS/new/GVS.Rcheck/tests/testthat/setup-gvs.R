# api urls

# URL for NSR API
url <- "https://gvsapi.xyz/gvs_api.php" # production
#url <- "http://vegbiendev.nceas.ucsb.edu:7775/gvs_api.php" #testing

# Bad URLs for testing

# url <- "www.google.com"
# url <- "www.hisstank.com"

library("vcr") # *Required* as vcr is set up on loading

invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()
