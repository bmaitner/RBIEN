context("ensemble_range_map")

test_that("ensemble_range_map is sensible", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  #ensemble

  ens_out <-
  ensemble_range_map(occurrences = occurrences,
                     env = env,
                     presence_method = c("kde","gaussian","rangebagging"),
                     background_method =c('gaussian'),
                     background_buffer_width = 10000 )

  expect_s4_class(object = ens_out$ensemble_map,class = "SpatRaster")

  expect_equal(object = length(ens_out$quality_list),expected = 3)

  expect_equal(object = nlyr(ens_out$map_stack),expected = 3)

})
