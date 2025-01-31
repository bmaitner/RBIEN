pkgname <- "S4DM"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('S4DM')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ensemble_range_map")
### * ensemble_range_map

flush(stderr()); flush(stdout())

### Name: ensemble_range_map
### Title: Generate ensemble predictions from S4DM range maps
### Aliases: ensemble_range_map

### ** Examples




cleanEx()
nameEx("evaluate_range_map")
### * evaluate_range_map

flush(stderr()); flush(stdout())

### Name: evaluate_range_map
### Title: Evaluate S4DM range map quality
### Aliases: evaluate_range_map

### ** Examples

{

# load in sample data

 library(S4DM)
 library(terra)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points

 # environmental data
   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

 # rescale the environmental data

   env <- scale(env)

# Evaluate a gaussian/gaussian model calculated with the numbag approach
# using 10 bootstrap replicates.

 evaluate_range_map(occurrences = occurrences,
                    env = env,
                    method = NULL,
                    presence_method = "gaussian",
                    background_method = "gaussian",
                    bootstrap = "numbag",
                    bootstrap_reps = 10,
                    quantile = 0.05,
                    constraint_regions = NULL,
                    background_buffer_width = 100000)



}



cleanEx()
nameEx("fit_density_ratio")
### * fit_density_ratio

flush(stderr()); flush(stdout())

### Name: fit_density_ratio
### Title: Fit density-ratio distribution models in a plug-and-play
###   framework.
### Aliases: fit_density_ratio

### ** Examples




cleanEx()
nameEx("fit_plug_and_play")
### * fit_plug_and_play

flush(stderr()); flush(stdout())

### Name: fit_plug_and_play
### Title: Fit presence-background distribution models in a plug-and-play
###   framework.
### Aliases: fit_plug_and_play

### ** Examples




cleanEx()
nameEx("get_env_bg")
### * get_env_bg

flush(stderr()); flush(stdout())

### Name: get_env_bg
### Title: Extract background data for SDM fitting.
### Aliases: get_env_bg

### ** Examples

{

# load in sample data

 library(S4DM)
 library(terra)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points

 # environmental data
   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

 # rescale the environmental data

   env <- scale(env)

bg_data <- get_env_bg(coords = occurrences,
                      env = env,
                      method = "buffer",
                      width = 100000)


}



cleanEx()
nameEx("get_env_pres")
### * get_env_pres

flush(stderr()); flush(stdout())

### Name: get_env_pres
### Title: Extract presence data for SDM fitting.
### Aliases: get_env_pres

### ** Examples

 {

# load in sample data

 library(S4DM)
 library(terra)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points

 # environmental data
   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

 # rescale the environmental data

   env <- scale(env)

env_pres <- get_env_pres(coords = occurrences,
                        env = env)

}



cleanEx()
nameEx("make_range_map")
### * make_range_map

flush(stderr()); flush(stdout())

### Name: make_range_map
### Title: Make a range map using plug-and-play modeling.
### Aliases: make_range_map

### ** Examples

{

# load in sample data

 library(S4DM)
 library(terra)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points

 # environmental data
   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

 # rescale the environmental data

   env <- scale(env)

   map <- make_range_map(occurrences = occurrences,
                         env = env,
                         method = "gaussian",
                         presence_method = NULL,
                         background_method = NULL,
                         bootstrap = "none",
                         bootstrap_reps = 100,
                         quantile = 0.05,
                         background_buffer_width = 100000)

   plot(map)


}



cleanEx()
nameEx("sdm_threshold")
### * sdm_threshold

flush(stderr()); flush(stdout())

### Name: sdm_threshold
### Title: Thresholds a continuous relative occurrence rate raster to
###   create a binary raster.
### Aliases: sdm_threshold

### ** Examples

{

# load in sample data

library(S4DM)
library(terra)

# occurrence points
  data("sample_points")
  occurrences <- sample_points

# environmental data
  env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

# rescale the environmental data

  env <- scale(env)

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

 plot(out_raster)

 #convert to a binary raster

 out_raster_binary <-
   sdm_threshold(prediction_raster = out_raster,
               occurrence_sf = pres_data$occurrence_sf,
               quantile = 0.05,
               return_binary = TRUE)

 plot(out_raster_binary)

}



cleanEx()
nameEx("stratify_random")
### * stratify_random

flush(stderr()); flush(stdout())

### Name: stratify_random
### Title: Split data for k-fold spatially stratified cross validation
### Aliases: stratify_random

### ** Examples

{

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


}



cleanEx()
nameEx("stratify_spatial")
### * stratify_spatial

flush(stderr()); flush(stdout())

### Name: stratify_spatial
### Title: Split data for k-fold spatially stratified cross validation
### Aliases: stratify_spatial

### ** Examples

{

# load in sample data

 library(S4DM)
 library(terra)
 library(sf)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points


 occurrences <- st_as_sf(x = occurrences,coords = c(1,2))

manual <- stratify_spatial(occurrence_sf = occurrences,nfolds = 5,nsubclusters = 5)
default <- stratify_spatial(occurrence_sf = occurrences)


}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
