context("stratify functions")

test_that("stratify random returns desired folds", {

  # load in sample data

  library(S4DM)
  library(terra)
  library(sf)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  occurrences <- st_as_sf(x = occurrences,coords = c(1,2))

  random_folds <- stratify_random(occurrence_sf = occurrences,
                                  nfolds = 5)

  expect_s3_class(object = random_folds,
                  class = "sf")

  expect_equal(object = length(unique(random_folds$fold)),
               expected = 5)

})


test_that("stratify spatial returns desired folds", {

  # load in sample data

  library(S4DM)
  library(terra)
  library(sf)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  occurrences <- st_as_sf(x = occurrences,coords = c(1,2))

  default <- stratify_spatial(occurrence_sf = occurrences)

  manual <- stratify_spatial(occurrence_sf = occurrences,
                             nfolds = 5,
                             nsubclusters = 5)


  expect_s3_class(object = default,
                  class = "sf")

  expect_s3_class(object = manual,
                  class = "sf")

  expect_equal(object = length(unique(manual$fold)),
               expected = 5)

})
