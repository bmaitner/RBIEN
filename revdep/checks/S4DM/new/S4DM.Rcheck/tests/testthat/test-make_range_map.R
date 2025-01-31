context("make_range_map")

test_that("make_range_map is sensible", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra


  out <- make_range_map(occurrences = occurrences,
                            env = env,
                            method = "gaussian",
                            background_buffer_width = 100000)

  expect_s4_class(object = out,class = "SpatRaster")


})
