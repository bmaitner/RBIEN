pkgname <- "wallace"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('wallace')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ecoClimate_getdata")
### * ecoClimate_getdata

flush(stderr()); flush(stdout())

### Name: ecoClimate_getdata
### Title: ecoClimate_getdata
### Aliases: ecoClimate_getdata

### ** Examples

## Not run: 
##D CCSM_mod_present <- ecoclimate_getdata("CCSM", "Modern", "Present")
##D dev.new()
##D plot(CCSM_mod_present)
## End(Not run)




cleanEx()
nameEx("ecoClimate_select")
### * ecoClimate_select

flush(stderr()); flush(stdout())

### Name: ecoClimate_select
### Title: ecoClimate_select
### Aliases: ecoClimate_select

### ** Examples

## Not run: 
##D CCSM_mod_present <- ecoclimate_getdata("CCSM", "Modern", "Present")
##D Europe_CCSM_m_p_bio1_12 <- ecoClimate_select(CCSM_mod_present, c(1, 12),
##D                                              extent = c(-20, 80, 20, 80))
##D dev.new()
##D plot(Europe_CCSM_m_p_bio1_12)
## End(Not run)





cleanEx()
nameEx("envs_ecoClimate")
### * envs_ecoClimate

flush(stderr()); flush(stdout())

### Name: envs_ecoClimate
### Title: envs_ecoClimate Obtain ecoClimate variables
### Aliases: envs_ecoClimate

### ** Examples

bcAOGCM <- "CCSM"
bcScenario <- "LGM"
ecoClimSel <- c(1,2,3)
## Not run: 
##D varsEcoClimate <- envs_ecoClimate(bcAOGCM, bcScenario, ecoClimSel)
## End(Not run)




cleanEx()
nameEx("envs_userEnvs")
### * envs_userEnvs

flush(stderr()); flush(stdout())

### Name: envs_userEnvs
### Title: envs_userEnvs
### Aliases: envs_userEnvs

### ** Examples

## Not run: 
##D pathRast <- list.files(system.file("extdata/wc", package = "wallace"),
##D                        pattern = ".tif$", full.names = TRUE)
##D nameRast <- list.files(system.file("extdata/wc", package = "wallace"),
##D                        pattern = ".tif$", full.names = FALSE)
##D userEnvs <- envs_userEnvs(rasPath = pathRast, rasName = nameRast)
## End(Not run)




cleanEx()
nameEx("envs_worldclim")
### * envs_worldclim

flush(stderr()); flush(stdout())

### Name: envs_worldclim
### Title: envs_worldclim Obtain WorldClim variables
### Aliases: envs_worldclim

### ** Examples

## Not run: 
##D bcRes <- 10 # (10 arcmin)
##D envar <- c('bio05', 'bio06', 'bio13', 'bio14')
##D arcmin10 <- envs_worldclim(bcRes, bcSel = envar)
## End(Not run)




cleanEx()
nameEx("espace_nicheOv")
### * espace_nicheOv

flush(stderr()); flush(stdout())

### Name: espace_nicheOv
### Title: espace_nicheOv Niche Overlap
### Aliases: espace_nicheOv

### ** Examples

## Not run: 
##D sp.name1 <- "Bassaricyon_alleni"
##D sp.name2 <- "Bassaricyon_neblina"
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D 
##D occs.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                     package = "wallace"))
##D occs.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
##D                     package = "wallace"))
##D 
##D bgPts.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                      package = "wallace"))
##D bgPts.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina_bgPoints.csv",
##D                      package = "wallace"))
##D 
##D occsExt.z1 <- raster::extract(envs, occs.z1[, c("longitude", "latitude")])
##D occsExt.z2 <- raster::extract(envs, occs.z2[, c("longitude", "latitude")])
##D bgExt.z1 <- raster::extract(envs, bgPts.z1[, c("longitude", "latitude")])
##D bgExt.z2 <- raster::extract(envs, bgPts.z2[, c("longitude", "latitude")])
##D pcaZ <- espace_pca(sp.name1, sp.name2,
##D                    occsExt.z1, occsExt.z2,
##D                    bgExt.z1, bgExt.z2)
##D occDens <- espace_occDens(sp.name1, sp.name2, pcaZ)
##D nicheOv <- espace_nicheOv(z1 = occDens[[sp.name1]],
##D                           z2 = occDens[[sp.name2]],
##D                           iter = 100, equivalency = TRUE,
##D                           similarity = TRUE)
## End(Not run)




cleanEx()
nameEx("espace_occDens")
### * espace_occDens

flush(stderr()); flush(stdout())

### Name: espace_occDens
### Title: Occurrence density grid
### Aliases: espace_occDens

### ** Examples

## Not run: 
##D sp.name1 <- "Bassaricyon_alleni"
##D sp.name2 <- "Bassaricyon_neblina"
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D 
##D occs.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                     package = "wallace"))
##D occs.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
##D                     package = "wallace"))
##D 
##D bgPts.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                      package = "wallace"))
##D bgPts.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina_bgPoints.csv",
##D                      package = "wallace"))
##D 
##D occsExt.z1 <- raster::extract(envs, occs.z1[, c("longitude", "latitude")])
##D occsExt.z2 <- raster::extract(envs, occs.z2[, c("longitude", "latitude")])
##D bgExt.z1 <- raster::extract(envs, bgPts.z1[, c("longitude", "latitude")])
##D bgExt.z2 <- raster::extract(envs, bgPts.z2[, c("longitude", "latitude")])
##D pcaZ <- espace_pca(sp.name1, sp.name2,
##D                    occsExt.z1, occsExt.z2,
##D                    bgExt.z1, bgExt.z2)
##D occDens <- espace_occDens(sp.name1, sp.name2, pcaZ)
## End(Not run)




cleanEx()
nameEx("espace_pca")
### * espace_pca

flush(stderr()); flush(stdout())

### Name: espace_pca
### Title: espace_pca Principal component analysis
### Aliases: espace_pca

### ** Examples

## Not run: 
##D sp.name1 <- "Bassaricyon_alleni"
##D sp.name2 <- "Bassaricyon_neblina"
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D 
##D occs.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                     package = "wallace"))
##D occs.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
##D                     package = "wallace"))
##D 
##D bgPts.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                      package = "wallace"))
##D bgPts.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina_bgPoints.csv",
##D                      package = "wallace"))
##D 
##D occsExt.z1 <- raster::extract(envs, occs.z1[, c("longitude", "latitude")])
##D occsExt.z2 <- raster::extract(envs, occs.z2[, c("longitude", "latitude")])
##D bgExt.z1 <- raster::extract(envs, bgPts.z1[, c("longitude", "latitude")])
##D bgExt.z2 <- raster::extract(envs, bgPts.z2[, c("longitude", "latitude")])
##D pcaZ <- espace_pca(sp.name1, sp.name2,
##D                    occsExt.z1, occsExt.z2,
##D                    bgExt.z1, bgExt.z2)
## End(Not run)



cleanEx()
nameEx("model_bioclim")
### * model_bioclim

flush(stderr()); flush(stdout())

### Name: model_bioclim
### Title: model_bioclim Generate Bioclim model
### Aliases: model_bioclim

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                  package = "wallace"))
##D bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                package = "wallace"))
##D partblock <- part_partitionOccs(occs, bg, method = 'block')
##D m <- model_bioclim(occs, bg, partblock, envs)
## End(Not run)




cleanEx()
nameEx("model_maxent")
### * model_maxent

flush(stderr()); flush(stdout())

### Name: model_maxent
### Title: model_maxent Generate maxent.jar or maxnet model
### Aliases: model_maxent

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                  package = "wallace"))
##D bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                package = "wallace"))
##D partblock <- part_partitionOccs(occs, bg, method = 'block')
##D rms <- c(1:2)
##D rmsStep <- 1
##D fcs <- c('L', 'LQ')
##D m <- model_maxent(occs = occs, bg = bg, user.grp = partblock,
##D                   bgMsk = envs, rms = rms, rmsStep, fcs,
##D                   clampSel = TRUE, algMaxent = "maxnet",
##D                   parallel = FALSE)
## End(Not run)




cleanEx()
nameEx("occs_queryDb")
### * occs_queryDb

flush(stderr()); flush(stdout())

### Name: occs_queryDb
### Title: occs_queryDb Query online database for species occurrence
###   records.
### Aliases: occs_queryDb

### ** Examples

## Not run: 
##D occs_queryDb(spName = "Bassaricyon alleni", occDb = "gbif", occNum = 10)
## End(Not run)



cleanEx()
nameEx("occs_userOccs")
### * occs_userOccs

flush(stderr()); flush(stdout())

### Name: occs_userOccs
### Title: occs_userOccs Loads user provided occurrence records
### Aliases: occs_userOccs

### ** Examples

txtPath <- system.file("extdata/Bassaricyon_alleni.csv", package = "wallace")
txtName <- 'Bassaricyon_alleni'
user.occs <- occs_userOccs(txtPath, txtName)





cleanEx()
nameEx("part_partitionOccs")
### * part_partitionOccs

flush(stderr()); flush(stdout())

### Name: part_partitionOccs
### Title: part_partitionOccs Partition occurrence data
### Aliases: part_partitionOccs

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                  package = "wallace"))
##D bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                package = "wallace"))
##D partblock <- part_partitionOccs(occs, bg, method = 'rand', kfold = 4)
## End(Not run)




cleanEx()
nameEx("penvs_bgExtent")
### * penvs_bgExtent

flush(stderr()); flush(stdout())

### Name: penvs_bgExtent
### Title: penvs_bgExtent Generate background extent
### Aliases: penvs_bgExtent

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)




cleanEx()
nameEx("penvs_bgMask")
### * penvs_bgMask

flush(stderr()); flush(stdout())

### Name: penvs_bgMask
### Title: penvs_bgMask Mask environmental data
### Aliases: penvs_bgMask

### ** Examples

## Not run: 
##D occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                  package = "wallace"))[, 2:3]
##D occs$occID <- 1:nrow(occs)
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
##D bgMask <- penvs_bgMask(occs, envs, bgExt)
## End(Not run)




cleanEx()
nameEx("penvs_bgSample")
### * penvs_bgSample

flush(stderr()); flush(stdout())

### Name: penvs_bgSample
### Title: penvs_bgSample Sample background points
### Aliases: penvs_bgSample

### ** Examples

## Not run: 
##D occs <-  occs_queryDb(spName = "Panthera onca", occDb = "gbif",
##D                       occNum = 100)
##D occs <- as.data.frame(occs[[1]]$cleaned)
##D envs <- envs_worldclim(bcRes = 10,
##D                        bcSel = c("bio03", "bio04", "bio13", "bio14"),
##D                        doBrick = TRUE)
##D bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
##D bgMask <- penvs_bgMask(occs, envs, bgExt)
##D bgsample <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000)
## End(Not run)




cleanEx()
nameEx("penvs_drawBgExtent")
### * penvs_drawBgExtent

flush(stderr()); flush(stdout())

### Name: penvs_drawBgExtent
### Title: penvs_drawBgExtent: Draw background extent
### Aliases: penvs_drawBgExtent

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867,
               -142.19085, -45.55045, -28.56050)
latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350,
              56.35954, 54.55045, -7.11861)
expertDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE,
                         ncol = 2)
drawBgBf <- penvs_drawBgExtent(polyExtXY = expertDrawPoly, polyExtID = 1,
                               drawBgBuf = 0.5, occs)



cleanEx()
nameEx("penvs_userBgExtent")
### * penvs_userBgExtent

flush(stderr()); flush(stdout())

### Name: penvs_userBgExtent
### Title: penvs_userBgExtent: user provided background extent
### Aliases: penvs_userBgExtent

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
pathShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = TRUE)
nameShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = FALSE)
userBgbf <- penvs_userBgExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                               userBgBuf = 0.2, occs = occs)




cleanEx()
nameEx("poccs_removeByID")
### * poccs_removeByID

flush(stderr()); flush(stdout())

### Name: poccs_removeByID
### Title: poccs_removeByID Remove occurrence by ID
### Aliases: poccs_removeByID

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
out.ID <- poccs_removeByID(occs, 11)




cleanEx()
nameEx("poccs_selectOccs")
### * poccs_selectOccs

flush(stderr()); flush(stdout())

### Name: poccs_selectOccs
### Title: poccs_selectOccs Remove occurrences outside of polygon
### Aliases: poccs_selectOccs

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                             package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
               -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167,
              0.98391, 6.09208, 12.74980)
expertAddedPoly <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
out.occs <- poccs_selectOccs(occs, polySelXY = expertAddedPoly,
                             polySelID = 1)



cleanEx()
nameEx("poccs_thinOccs")
### * poccs_thinOccs

flush(stderr()); flush(stdout())

### Name: poccs_thinOccs
### Title: poocs_thinOccs Thin occurrences
### Aliases: poccs_thinOccs

### ** Examples

occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                             package = "wallace"))
occs$occID <- 1:nrow(occs)
out.thin <- poccs_thinOccs(occs = occs, thinDist = 30)





cleanEx()
nameEx("run_wallace")
### * run_wallace

flush(stderr()); flush(stdout())

### Name: run_wallace
### Title: Run _Wallace_ Application
### Aliases: run_wallace

### ** Examples

if(interactive()) {
run_wallace()
}



cleanEx()
nameEx("vis_bioclimPlot")
### * vis_bioclimPlot

flush(stderr()); flush(stdout())

### Name: vis_bioclimPlot
### Title: vis_bioclimPlot Visualize bivariate plot of BIOCLIM model
### Aliases: vis_bioclimPlot

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
##D                  package = "wallace"))
##D bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
##D                package = "wallace"))
##D partblock <- part_partitionOccs(occs, bg, method = 'block')
##D m <- model_bioclim(occs, bg, partblock, envs)
##D bioclimPlot <- vis_bioclimPlot(x = m@models$bioclim,
##D                                a = 1, b = 2, p = 1)
## End(Not run)




cleanEx()
nameEx("xfer_area")
### * xfer_area

flush(stderr()); flush(stdout())

### Name: xfer_area
### Title: xfer_area Transfer model to a new area
### Aliases: xfer_area

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D # extent of transfer
##D longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
##D                -66.47149, -66.71319, -71.11931)
##D latitude <- c(13.18379, 7.52315, 0.93105,
##D               -1.70167, 0.98391, 6.09208, 12.74980)
##D selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
##D polyExt <-
##D   sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
##D                       ID = 1)))
##D # load model
##D m <- readRDS(system.file("extdata/model.RDS",
##D                          package = "wallace"))
##D modXfer <- xfer_area(evalOut = m, curModel = 1, envs,
##D                      outputType = 'cloglog', alg = 'maxent.jar',
##D                      clamp = TRUE, xfExt = polyExt)
## End(Not run)




cleanEx()
nameEx("xfer_draw")
### * xfer_draw

flush(stderr()); flush(stdout())

### Name: xfer_draw
### Title: xfer_draw Draw extent of transfer
### Aliases: xfer_draw

### ** Examples

longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867,
               -142.19085, -45.55045, -28.56050)
latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350,
              56.35954, 54.55045, -7.11861)
userDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE,
                       ncol = 2)
drawXfBuf <- 0.5
polyXfID <- 1
polygonTest <- xfer_draw(polyXfXY = userDrawPoly, polyXfID,
                         drawXfBuf)




cleanEx()
nameEx("xfer_mess")
### * xfer_mess

flush(stderr()); flush(stdout())

### Name: xfer_mess
### Title: xfer_mess generate MESS map for transferred raster
### Aliases: xfer_mess

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                            package = "wallace"),
##D                       pattern = ".tif$", full.names = FALSE))
##D # load model
##D m <- readRDS(system.file("extdata/model.RDS",
##D                          package = "wallace"))
##D occsEnvs <- m@occs
##D bgEnvs <- m@bg
##D envsFut <- list.files(path = system.file('extdata/wc/future',
##D                                          package = "wallace"),
##D                       full.names = TRUE)
##D envsFut <- raster::stack(envsFut)
##D ## run function
##D xferMess <- xfer_mess(occs = occsEnvs, bg = bgEnvs, bgMsk = envs,
##D                       xferExtRas = envsFut)
## End(Not run)



cleanEx()
nameEx("xfer_time")
### * xfer_time

flush(stderr()); flush(stdout())

### Name: xfer_time
### Title: xfer_time Transfer model to a new time
### Aliases: xfer_time

### ** Examples

## Not run: 
##D envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
##D                                                        package = "wallace"),
##D                                            pattern = ".tif$",
##D                                            full.names = TRUE),
##D                       rasName = list.files(system.file("extdata/wc",
##D                                                        package = "wallace"),
##D                                            pattern = ".tif$",
##D                                            full.names = FALSE))
##D ## extent to transfer
##D # set coordinates
##D longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
##D                -71.11931)
##D latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
##D # generate matrix
##D selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
##D polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
##D                                                  ID = 1)))
##D # load model
##D m <- readRDS(system.file("extdata/model.RDS",
##D                          package = "wallace"))
##D occsEnvs <- m@occs
##D bgEnvs <- m@bg
##D envsFut <- list.files(path = system.file('extdata/wc/future',
##D                                          package = "wallace"),
##D                       full.names = TRUE)
##D envsFut <- raster::stack(envsFut)
##D modXfer <- xfer_time(evalOut = m, curModel = 1,
##D                      envs = envsFut, alg = 'maxent.jar',
##D                      xfExt = polyExt, clamp = FALSE, outputType = 'cloglog')
## End(Not run)



cleanEx()
nameEx("xfer_userEnvs")
### * xfer_userEnvs

flush(stderr()); flush(stdout())

### Name: xfer_userEnvs
### Title: xfer_userEnvs Transfer model to user specified area and time
### Aliases: xfer_userEnvs

### ** Examples

## Not run: 
##D ## extent to transfer
##D # set coordinates
##D longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
##D                -71.11931)
##D latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
##D # generate matrix
##D selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
##D polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
##D                                                  ID = 1)))
##D # load model
##D m <- readRDS(system.file("extdata/model.RDS",
##D                          package = "wallace"))
##D envsFut <- list.files(path = system.file('extdata/wc/future',
##D                                          package = "wallace"),
##D                       full.names = TRUE)
##D envsFut <- raster::stack(envsFut)
##D ### run function
##D modXfer <- xfer_userEnvs(evalOut = m, curModel = 1, envs = envsFut,
##D                          outputType = "cloglog", alg = "maxent.jar",
##D                          clamp = FALSE, xfExt = polyExt)
## End(Not run)




cleanEx()
nameEx("xfer_userExtent")
### * xfer_userExtent

flush(stderr()); flush(stdout())

### Name: xfer_userExtent
### Title: xfer_userExtent: user provided extent of transfer
### Aliases: xfer_userExtent

### ** Examples

pathShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = TRUE)
nameShp <- list.files(system.file("extdata/shp", package = "wallace"),
                      full.names = FALSE)
xferUser <- xfer_userExtent(bgShp_path = pathShp, bgShp_name = nameShp,
                            userBgBuf = 1)



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
