context("Testing gbifRetriever")

library(occCite)

test_that("gbifRetriever behaves as expected", {
  skip_on_cran()
  test <- try(rgbif::occ_count(country = "DK"),
              silent = T
  )
  skip_if(class(test) != "numeric", "GBIF connection unsuccessful")

  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(dir = system.file("extdata/", package = "occCite"))
  taxon <- "Protea cynaroides"

  testResult <- occCite:::gbifRetriever(taxon)

  expect_equal(class(testResult), "list")
  expect_equal(class(testResult[[1]]), "data.frame")
  expect_equal(names(testResult)[[1]], "OccurrenceTable")
  expect_equal(class(testResult[[2]]), "occ_download_meta")
  expect_equal(names(testResult)[[2]], "Metadata")
  expect_equal(class(testResult[[3]]), "occ_download_get")
  expect_equal(names(testResult)[[3]], "RawOccurrences")
})

test_that("gbifRetriever warns when there's no internet connection", {
  skip_on_cran()
  test <- try(rgbif::occ_count(country = "DK"),
              silent = T
  )
  skip_if(is(test, "numeric"), "GBIF connection successful")

  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(dir = system.file("extdata/", package = "occCite"))
  taxon <- "Protea cynaroides"

  expect_warning(occCite:::gbifRetriever())
  expect_warning(occCite:::gbifRetriever(taxon))
})
