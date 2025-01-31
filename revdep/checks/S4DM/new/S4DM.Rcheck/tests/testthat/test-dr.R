context("dr_ fxs")

test_that("maxnet returns correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  pres <- get_env_pres(coords = occurrences,
                       env = env)

  bg <- get_env_bg(coords = occurrences,
                     env = env,width = 1000)

  out <- S4DM:::dr_maxnet(presence_data = pres$env,
                          background_data = bg$env,method = "fit")


  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "dr_estimate")

  out2 <- S4DM:::dr_maxnet(projection_data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})


##############################

test_that("ulsif returns correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  pres <- get_env_pres(coords = occurrences,
                       env = env)

  bg <- get_env_bg(coords = occurrences,
                   env = env,width = 1000)

  out <- S4DM:::dr_ulsif(presence_data = pres$env,
                          background_data = bg$env,method = "fit")


  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "dr_estimate")

  out2 <- S4DM:::dr_ulsif(projection_data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))

})


##############################

test_that("rulsif returns correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  pres <- get_env_pres(coords = occurrences,
                       env = env)

  bg <- get_env_bg(coords = occurrences,
                   env = env,width = 1000)

  out <- S4DM:::dr_rulsif(presence_data = pres$env,
                         background_data = bg$env,method = "fit")


  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "dr_estimate")

  out2 <- S4DM:::dr_rulsif(projection_data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})

