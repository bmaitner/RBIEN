context("get_env_bg")

test_that("get_env_bg returns correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  out <- get_env_bg(coords = occurrences,
                 env = env,
                 method = "buffer",
                 width = 100,
                 standardize = FALSE)

  expect_equal(object = length(out),expected = 4)

})
