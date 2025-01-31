context("fit and project plug-and-play")

test_that("fit_ and project_plug_and_play return correct format", {

  library(S4DM)
  library(terra)

  # occurrence points
  data("sample_points")
  occurrences <- sample_points

  # environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  env <- terra::scale(env) #Note: this line causes an error without the explicit call to terra

  bg <- get_env_bg(coords = occurrences,
                   env = env,
                   width = 1000)

  pres <- get_env_pres(coords = occurrences,
                      env = env)

  out <- fit_plug_and_play(presence = pres$env,
                    background = bg$env,
                    presence_method = "rangebagging",
                    background_method = "none")

  expect_equal(object = length(out),expected = 6)


  out2 <- project_plug_and_play(pnp_model = out,
                                data = bg$env)

  expect_equal(object = length(out2),expected = nrow(bg$env))


})
