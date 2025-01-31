context("GBIF tabulation")

library(occCite)

GBIFLogin <- try(GBIFLoginManager(), silent = T)
key <- 5637947

test_that("data entered into tabGBIF is as expected", {
  skip_on_cran() # Requires GBIF login information
  skip_if(is(GBIFLogin, "try-error"), "GBIF login unsuccessful")

  occD <- prevGBIFdownload(key, GBIFLogin)
  res <- rgbif::occ_download_get(
    key = occD, overwrite = TRUE,
    file.path(system.file("extdata/",
      package = "occCite"
    ))
  )

  expect_equal(class(res), "occ_download_get")
})

test_that("verify occ_download_import results have expected columns", {
  skip_on_cran() # Requires GBIF login information
  skip_if(is(GBIFLogin, "try-error"), "GBIF login unsuccessful")

  occD <- prevGBIFdownload(key, GBIFLogin)
  res <- rgbif::occ_download_get(
    key = occD, overwrite = TRUE,
    file.path(system.file("extdata/",
      package = "occCite"
    ))
  )
  occFromGBIF <- rgbif::occ_download_import(res)

  expect_true("data.frame" %in% class(occFromGBIF))
  expect_true("species" %in% colnames(occFromGBIF))
  expect_true("decimalLongitude" %in% colnames(occFromGBIF))
  expect_true("decimalLatitude" %in% colnames(occFromGBIF))
  expect_true("day" %in% colnames(occFromGBIF))
  expect_true("month" %in% colnames(occFromGBIF))
  expect_true("year" %in% colnames(occFromGBIF))
  expect_true("datasetName" %in% colnames(occFromGBIF))
  expect_true("datasetKey" %in% colnames(occFromGBIF))
})

test_that("tabGBIF results as expected", {
  skip_on_cran() # Requires GBIF login information
  skip_if(is(GBIFLogin, "try-error"), "GBIF login unsuccessful")

  occD <- prevGBIFdownload(key, GBIFLogin)
  res <- rgbif::occ_download_get(
    key = occD, overwrite = TRUE,
    file.path(system.file("extdata/", package = "occCite"))
  )
  occFromGBIF <- occCite:::tabGBIF(GBIFresults = res, "Protea cynaroides")

  expect_equal(class(occFromGBIF), "data.frame")
  expect_true("name" %in% colnames(occFromGBIF))
  expect_true("longitude" %in% colnames(occFromGBIF))
  expect_true("latitude" %in% colnames(occFromGBIF))
  expect_true("day" %in% colnames(occFromGBIF))
  expect_true("month" %in% colnames(occFromGBIF))
  expect_true("year" %in% colnames(occFromGBIF))
  expect_true("datasetName" %in% colnames(occFromGBIF))
  expect_true("datasetKey" %in% colnames(occFromGBIF))
  expect_true("dataService" %in% colnames(occFromGBIF))
})
