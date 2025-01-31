context("pnp_ fxs")

test_that("gaussian returns correct format", {

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

  out <- S4DM:::pnp_gaussian(data = pres$env,method = "fit")


  expect_equal(object = length(out),expected = 3)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_gaussian(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})


##############################

test_that("kde returns correct format", {

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

  out <- S4DM:::pnp_kde(data = pres$env,method = "fit")


  expect_equal(object = length(out),expected = 3)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_kde(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})

#########################################

test_that("lobagoc returns correct format", {

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

  out <- S4DM:::pnp_lobagoc(data = pres$env,method = "fit")

  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_lobagoc(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})


#########################################

test_that("rangebagging returns correct format", {

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

  out <- S4DM:::pnp_rangebagging(data = pres$env,method = "fit")

  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_rangebagging(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})


#########################################

test_that("vine returns correct format", {

  skip_on_cran()

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

  out <- S4DM:::pnp_vine(data = pres$env,method = "fit")

  expect_equal(object = length(out),expected = 2)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_vine(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = nrow(bg$env))

})


#########################################

test_that("none returns correct format", {

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

  out <- S4DM:::pnp_none(data = pres$env,method = "fit")

  expect_equal(object = length(out),expected = 1)

  expect_s3_class(object = out, class = "pnp_estimate")

  out2 <- S4DM:::pnp_none(data = bg$env,method = "predict",object = out)

  expect_equal(object = length(out2),expected = 0)


})
