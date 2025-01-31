context("sdm_threshold")

test_that("sdm_threshold makes rasters binary", {


  library(S4DM)
  library(terra)

  # occurrence points
    data("sample_points")
    occurrences <- sample_points

  # environmental data
    env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

  # rescale the environmental data

    env <- terra::scale(env) #breaks testthat if this isn't done

   bg_data <- get_env_bg(coords = occurrences,
                         env = env,
                         method = "buffer",
                         width = 100000)

   pres_data <- get_env_pres(coords = occurrences,
                             env = env)

   pnp_model <-fit_plug_and_play(presence = pres_data$env,
                     background = bg_data$env,
                     method = "gaussian")

   pnp_continuous <- project_plug_and_play(pnp_model = pnp_model,
                                           data = bg_data$env)

   #Make an empty raster to populate
   out_raster <- env[[1]]
   values(out_raster) <- NA

   # use the bg_data for indexing
   out_raster[bg_data$bg_cells] <- pnp_continuous

   #convert to a binary raster

   out_raster_binary <-
     sdm_threshold(prediction_raster = out_raster,
                 occurrence_sf = pres_data$occurrence_sf,
                 quantile = 0.05,
                 return_binary = TRUE)

   expect_s4_class(object = out_raster_binary,class = "SpatRaster")

   expect_equal(object = length(unique(values(out_raster_binary))),expected = 2)


})
