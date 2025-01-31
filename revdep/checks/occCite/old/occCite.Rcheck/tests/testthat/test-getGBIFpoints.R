context("Getting GBIF points")

library(occCite)

test_that("GBIF retrieval from server behaves as expected", {
  skip_on_cran()
  GBIFLogin <- try(GBIFLoginManager(), silent = T)
  skip_if(is(GBIFLogin, "try-error"),
          message = "GBIF login unsuccessful")

  key <- 5637947
  occD <- prevGBIFdownload(key, GBIFLogin = GBIFLogin)
  res <- rgbif::occ_download_get(
    key = occD, overwrite = TRUE,
    file.path(system.file("extdata/",
      package = "occCite"
    ))
  )

  expect_equal(class(res), "occ_download_get")
})

test_that("getGBIFpoints behaves as expected", {
  skip_on_cran()
  GBIFLogin <- try(GBIFLoginManager(), silent = T)
  skip_if(is(GBIFLogin, "try-error"),
          message = "GBIF login unsuccessful")

  expect_error(getGBIFpoints())

  testResult <- getGBIFpoints(
    taxon = "Protea cynaroides", GBIFLogin,
    file.path(system.file("extdata/",
      package = "occCite"
    ))
  )
  expect_equal(class(testResult), "list")
  expect_equal(length(testResult), 3)

  expect_true("OccurrenceTable" %in% names(testResult))
  expect_true("Metadata" %in% names(testResult))
  expect_true("RawOccurrences" %in% names(testResult))

  expect_equal(class(testResult$OccurrenceTable), "data.frame")
  expect_equal(class(testResult$Metadata), "occ_download_meta")
  expect_equal(class(testResult$RawOccurrences), "occ_download_get")

  expect_true("name" %in% colnames(testResult$OccurrenceTable))
  expect_true("longitude" %in% colnames(testResult$OccurrenceTable))
  expect_true("latitude" %in% colnames(testResult$OccurrenceTable))
  expect_true("day" %in% colnames(testResult$OccurrenceTable))
  expect_true("month" %in% colnames(testResult$OccurrenceTable))
  expect_true("year" %in% colnames(testResult$OccurrenceTable))
  expect_true("datasetName" %in% colnames(testResult$OccurrenceTable))
  expect_true("datasetKey" %in% colnames(testResult$OccurrenceTable))
  expect_true("dataService" %in% colnames(testResult$OccurrenceTable))
})
