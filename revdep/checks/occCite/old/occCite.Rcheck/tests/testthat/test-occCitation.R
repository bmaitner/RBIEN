context("Testing occCitation")

library(occCite)
library(lubridate)

data("myOccCiteObject")

test_that("inputs to occCitation are as expected", {
  expect_true(class(myOccCiteObject) == "occCiteData")
  expect_true(names(myOccCiteObject@occResults) > 0)
  expect_true("BIEN" %in% names(myOccCiteObject@occResults[[1]]))
  expect_true("OccurrenceTable"
  %in% names(myOccCiteObject@occResults[[1]]$BIEN))
  expect_true("datasetKey"
  %in% names(myOccCiteObject@occResults[[1]]$BIEN$OccurrenceTable))

  expect_true("GBIF"
  %in% names(myOccCiteObject@occResults[[1]]))
  expect_true("OccurrenceTable"
  %in% names(myOccCiteObject@occResults[[1]]$GBIF))
  expect_true("datasetKey"
  %in% names(myOccCiteObject@occResults[[1]]$GBIF$OccurrenceTable))
})

test_that("outputs for GBIF results are as expected", {
  skip_on_cran()

  test <- try(rgbif::occ_count(country = "DK"),
    silent = T
  )
  skip_if(class(test) != "numeric", "GBIF connection unsuccessful")

  expect_error(occCitation())
  expect_error(occCitation(2))

  myOccCiteObject@occResults[[1]]$BIEN <- NULL
  testResults <- occCitation(myOccCiteObject)

  expect_true(class(testResults) == "occCiteCitation")

  expect_true("occCitationResults" %in% names(testResults))
  expect_equal(
    class(testResults$occCitationResults[[1]]),
    "data.frame"
  )

  expect_true("occSearch"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$occSearch),
    "character"
  )
  expect_true("Dataset Key"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$`Dataset Key`),
    "character"
  )
  expect_true("Citation"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$Citation),
    "character"
  )
  expect_true("Accession Date"
  %in% names(testResults$occCitationResults[[1]]))
  expect_true(is.Date(as.Date(testResults$occCitationResults[[1]]$`Accession Date`)))
  expect_true("Number of Occurrences"
  %in% names(testResults$occCitationResults[[1]]))
  expect_true(is.numeric(testResults$occCitationResults[[1]]$`Number of Occurrences`))
})

test_that("outputs for BIEN results are as expected", {
  skip_if(!curl::has_internet(), "internet connection unsuccessful")

  data("myOccCiteObject")
  myOccCiteObject@occResults[[1]]$GBIF <- NULL
  testResults <- occCitation(myOccCiteObject)

  expect_true(class(testResults) == "occCiteCitation")

  expect_true("occCitationResults" %in% names(testResults))
  expect_equal(
    class(testResults$occCitationResults[[1]]),
    "data.frame"
  )

  expect_true("occSearch"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$occSearch),
    "character"
  )
  expect_true("Dataset Key"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$`Dataset Key`),
    "character"
  )
  expect_true("Citation"
  %in% names(testResults$occCitationResults[[1]]))
  expect_equal(
    class(testResults$occCitationResults[[1]]$Citation),
    "character"
  )
  expect_true("Accession Date"
  %in% names(testResults$occCitationResults[[1]]))
  expect_true(is.Date(as.Date(testResults$occCitationResults[[1]]$`Accession Date`)))
  expect_true("Number of Occurrences"
  %in% names(testResults$occCitationResults[[1]]))
  expect_true(is.numeric(testResults$occCitationResults[[1]]$`Number of Occurrences`))
})

test_that("warnings work when there is no internet", {
  skip_if(curl::has_internet(), "internet connection established")

  data("myOccCiteObject")
  myOccCiteObject@occResults[[1]]$GBIF <- NULL
  expect_warning(occCitation(myOccCiteObject))
})
