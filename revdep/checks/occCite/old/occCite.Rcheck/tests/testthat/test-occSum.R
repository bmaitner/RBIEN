library(occCite)

data("myOccCiteObject")

test_that("input data object for summary behaves as expected", {
  expect_true(class(myOccCiteObject) == "occCiteData")

  expect_true("userQueryType" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@userQueryType) == "character")

  expect_true("userSpecTaxonomy" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@userSpecTaxonomy) == "character")

  expect_true("cleanedTaxonomy" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@cleanedTaxonomy) == "data.frame")
  expect_true("Input Name" %in% colnames(myOccCiteObject@cleanedTaxonomy))
  expect_true("Best Match" %in% colnames(myOccCiteObject@cleanedTaxonomy))
  expect_true("Taxonomic Databases w/ Matches" %in%
                colnames(myOccCiteObject@cleanedTaxonomy))

  expect_true("occSources" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@occSources) == "character")

  expect_true("occCiteSearchDate" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@occCiteSearchDate) == "character")
  expect_true(class(as.Date(myOccCiteObject@occCiteSearchDate)) == "Date")

  expect_true("occResults" %in% slotNames(myOccCiteObject))
  expect_true(class(myOccCiteObject@occResults) == "list")
  expect_true("GBIF" %in% names(myOccCiteObject@occResults[[1]]))
  expect_true(all(c("OccurrenceTable", "Metadata", "RawOccurrences")
  %in% names(myOccCiteObject@occResults[[1]][[1]])))
  expect_true(all(c("name", "longitude", "latitude",
                    "day", "month", "year",
                    "datasetName", "datasetKey", "dataService") %in%
                    colnames(myOccCiteObject@occResults[[1]][[1]][[1]])))
  expect_true("BIEN" %in% names(myOccCiteObject@occResults[[1]]))
  expect_true(all(c("OccurrenceTable", "Metadata", "RawOccurrences")
  %in% names(myOccCiteObject@occResults[[1]][[2]])))
  expect_true(all(c(
    "name", "longitude", "latitude",
    "day", "month", "year",
    "datasetName", "datasetKey", "dataService"
  )
  %in% colnames(myOccCiteObject@occResults[[1]][[2]][[1]])))
})

test_that("summary behaves as expected", {
  expect_error(occCite:::summary.occCiteData())
  expect_error(occCite:::summary.occCiteData("a"))

  testObject <- occCite:::summary.occCiteData(myOccCiteObject)
  expect_true(class(testObject) == "data.frame")

  myOccCiteObject@occSources <- "gbif"
  testObject <- occCite:::summary.occCiteData(myOccCiteObject)
  expect_true(class(testObject) == "data.frame")

  myOccCiteObject@occSources <- "bien"
  testObject <- occCite:::summary.occCiteData(myOccCiteObject)
  expect_null(testObject)
})
