context("Getting BIEN points")

library(occCite)

test_that("BIEN behaves as expected", {
  skip_on_cran()
  skip_if(!curl::has_internet(), "internet connection unsuccessful")

  testResult <- BIEN::BIEN_occurrence_species(
    species = "Protea cynaroides",
    cultivated = T,
    new.world = F, native.status = F,
    collection.info = T, natives.only = F
  )
  expect_equal(class(testResult), "data.frame")
  expect_true("scrubbed_species_binomial" %in% colnames(testResult))
  expect_true("longitude" %in% colnames(testResult))
  expect_true("latitude" %in% colnames(testResult))
  expect_true("date_collected" %in% colnames(testResult))
  expect_equal(class(testResult$date_collected), "Date")
  expect_true("dataset" %in% colnames(testResult))
  expect_true("datasource_id" %in% colnames(testResult))

  expect_equal(class(BIEN::BIEN_metadata_citation()), "list")
})

test_that("getBIENpoints behaves as expected", {
  skip_on_cran()
  skip_if(!curl::has_internet(), "internet connection unsuccessful")

  expect_error(getBIENpoints())

  testResult <- getBIENpoints(taxon = "Protea cynaroides")
  expect_equal(class(testResult), "list")
  expect_equal(length(testResult), 3)

  expect_true("OccurrenceTable" %in% names(testResult))
  expect_true("Metadata" %in% names(testResult))
  expect_true("RawOccurrences" %in% names(testResult))

  expect_equal(class(testResult$OccurrenceTable), "data.frame")
  expect_equal(class(testResult$Metadata), "list")
  expect_equal(class(testResult$RawOccurrences), "data.frame")

  expect_true("name" %in% colnames(testResult$OccurrenceTable))
  expect_true("longitude" %in% colnames(testResult$OccurrenceTable))
  expect_true("latitude" %in% colnames(testResult$OccurrenceTable))
  expect_true("coordinateUncertaintyInMeters" %in% colnames(testResult$OccurrenceTable))
  expect_true("day" %in% colnames(testResult$OccurrenceTable))
  expect_true("month" %in% colnames(testResult$OccurrenceTable))
  expect_true("year" %in% colnames(testResult$OccurrenceTable))
  expect_true("datasetName" %in% colnames(testResult$OccurrenceTable))
  expect_true("datasetKey" %in% colnames(testResult$OccurrenceTable))
  expect_true("dataService" %in% colnames(testResult$OccurrenceTable))
})
