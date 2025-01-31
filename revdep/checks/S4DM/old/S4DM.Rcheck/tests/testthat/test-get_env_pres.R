context("get_env_pres")

test_that("get_env_pres returns correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  out <- get_env_pres(coords = occurrences,
                    env = env)

  expect_equal(object = length(out),expected = 2)


})

test_that("get_env_pres rescales when intended", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  bg <- get_env_bg(coords = occurrences,
                      env = env,
                      method = "buffer",
                      width = 100,
                      standardize = TRUE)


  out <- get_env_pres(coords = occurrences,
                      env = env,
                      env_bg = bg)

  expect_equal(object = length(out),expected = 2)

  expect_lt(object = mean(out$env[,1]),expected = 10)


})

test_that("get_env_pres doesn't rescale when not intended", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  bg <- get_env_bg(coords = occurrences,
                      env = env,
                      method = "buffer",
                      width = 100,
                      standardize = FALSE)


  out <- get_env_pres(coords = occurrences,
                      env = env,
                      env_bg = bg)

  expect_gt(object = mean(out$env[,1]),expected = 10)

})
