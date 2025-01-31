context("fit and project density_ratio")

test_that("fit_ and project_density_ratio return correct format", {

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

  out <- fit_density_ratio(presence = pres$env,
                           background = bg$env,
                           method = "rulsif")

  expect_equal(object = length(out),
               expected = 2)

  out2 <- project_density_ratio(dr_model = out,
                                data = bg$env)

  expect_equal(object = length(out2),expected = nrow(bg$env))

})
