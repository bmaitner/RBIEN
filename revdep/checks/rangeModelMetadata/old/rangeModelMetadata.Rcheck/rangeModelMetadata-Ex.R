pkgname <- "rangeModelMetadata"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rangeModelMetadata')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("csvToRMM")
### * csvToRMM

flush(stderr()); flush(stdout())

### Name: csvToRMM
### Title: Create rangeModelMetaData ('rmm') object from a .csv File
### Aliases: csvToRMM

### ** Examples

csv <- "somePathOnYourMachine/rmm_example.csv";
## Not run: temp <- csvToRMM(csv);




cleanEx()
nameEx("rmmAutofillBIEN")
### * rmmAutofillBIEN

flush(stderr()); flush(stdout())

### Name: rmmAutofillBIEN
### Title: Add occurrence metadata from a BIEN query to an rmm object
### Aliases: rmmAutofillBIEN

### ** Examples

## Not run: 
##D rmm <- rmmTemplate()
##D xs <- BIEN::BIEN_occurrence_species(species="Xanthium strumarium")
##D rmmAutofillBIEN(rmm = rmm, occurrences = xs)
## End(Not run)



cleanEx()
nameEx("rmmAutofillEnvironment")
### * rmmAutofillEnvironment

flush(stderr()); flush(stdout())

### Name: rmmAutofillEnvironment
### Title: Add relevant environmental data information to an rmm object
### Aliases: rmmAutofillEnvironment

### ** Examples

## Not run: 
##D rmm=rmmTemplate()
##D rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
##D                        pattern='grd', full.names=TRUE)
##D #make a stack of the rasters
##D env=raster::stack(rasterFiles)
##D # for fitting environment
##D rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
##D # for the first environment that you're transfering to
##D rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
##D # for the second environment that you're transfering to, etc.
##D rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
## End(Not run)



cleanEx()
nameEx("rmmAutofillPackageCitation")
### * rmmAutofillPackageCitation

flush(stderr()); flush(stdout())

### Name: rmmAutofillPackageCitation
### Title: Add all package citations to an rmm object
### Aliases: rmmAutofillPackageCitation

### ** Examples

rmm=rmmTemplate()
rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))




cleanEx()
nameEx("rmmAutofillspocc")
### * rmmAutofillspocc

flush(stderr()); flush(stdout())

### Name: rmmAutofillspocc
### Title: Add occurrence metadata from a spocc query to an rmm object
### Aliases: rmmAutofillspocc

### ** Examples

## Not run: 
##D rmm=rmmTemplate()
##D xs <- spocc::occ("Xanthium strumarium")
##D rmmAutofillspocc(rmm = rmm, occ = xs)
## End(Not run)



cleanEx()
nameEx("rmmCheckEmpty")
### * rmmCheckEmpty

flush(stderr()); flush(stdout())

### Name: rmmCheckEmpty
### Title: Check an rmm object for empty fields
### Aliases: rmmCheckEmpty

### ** Examples

#First, make an empty rmm object:
rmm<-rmmTemplate()
#Next, we check for emtpy fields:
empties1<-rmmCheckEmpty(rmm = rmm)
#If looks like there are quite a few empty obligate fields.  Let's populate a few:
rmm$data$occurrence$taxon<-"Acer rubrum"
rmm$data$environment$variableNames<-"Bio1"
#Now, if we run rmmCheckEmpty again, we see there are 2 fewer empty, obligate fields
empties2<-rmmCheckEmpty(rmm = rmm)





cleanEx()
nameEx("rmmCheckFinalize")
### * rmmCheckFinalize

flush(stderr()); flush(stdout())

### Name: rmmCheckFinalize
### Title: Run a final check of an rmm object
### Aliases: rmmCheckFinalize

### ** Examples

rmm<-rmmTemplate() # Make an empty template
rmmCheckFinalize(rmm)





cleanEx()
nameEx("rmmCheckMissingNames")
### * rmmCheckMissingNames

flush(stderr()); flush(stdout())

### Name: rmmCheckMissingNames
### Title: Check for missing fields
### Aliases: rmmCheckMissingNames

### ** Examples

rmm<-rmmTemplate() # Make an empty template





cleanEx()
nameEx("rmmCheckName")
### * rmmCheckName

flush(stderr()); flush(stdout())

### Name: rmmCheckName
### Title: Check field names of a range model metadata list against
###   conventions
### Aliases: rmmCheckName

### ** Examples

rmm<-rmmTemplate() # Make an empty template
rmm$dataPrep$biological$taxonomicHarmonization$taxonomy_source<-"The Plant List"
# Add a new, non-standard field
rmm.1=rmmCheckName(rmm)
# Checking the names should identify the new, non-standard field we've added ("taxonomy_source")





cleanEx()
nameEx("rmmCheckShiny")
### * rmmCheckShiny

flush(stderr()); flush(stdout())

### Name: rmmCheckShiny
### Title: RangeModelMetadata Check in Shiny
### Aliases: rmmCheckShiny

### ** Examples

## Not run: 
##D rmm1=rmmTemplate()
##D rmm1=rmmAutofillPackageCitation(rmm1,c('raster','sp'))
##D rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
##D                        pattern='grd', full.names=TRUE)
##D make a stack of the rasters
##D env=raster::stack(rasterFiles)
##D # for fitting environment
##D rmm1=rmmAutofillEnvironment(rmm1,env,transfer=0)
##D # for transfer environment 1 (assuming different than for fitting)
##D rmm1=rmmAutofillEnvironment(rmm1,env,transfer=1)
##D # for transfer environment 2 (assuming different than 1)
##D rmm1=rmmAutofillEnvironment(rmm1,env,transfer=2)
## End(Not run)
## Not run:  rmmCheckShiny(rmm1) 




cleanEx()
nameEx("rmmCheckValue")
### * rmmCheckValue

flush(stderr()); flush(stdout())

### Name: rmmCheckValue
### Title: Check values of a range model metadata list against commonly
###   used values
### Aliases: rmmCheckValue

### ** Examples

rmm<-rmmTemplate() #First, we create an empty rmm template
rmm$data$environment$variableNames<- c("bio1", "bio 2", "bio3", "cromulent")
#We add 3 of the bioclim layers, including a spelling error (an extra space) in bio2,
# and a word that is clearly not a climate layer, 'cromulent'.
rmmCheckValue(rmm = rmm)
#Now, when we check the values, we see that bio1 and bio2 are reported as exact matches,
#while 'bio 2' is flagged as a partial match with a suggested value of 'bio2',
# and cromulent is flagged as not matched at all.
#If we'd like to return a dataframe containing this information in a perhaps more useful format:
rmmCheckValue_output<-rmmCheckValue(rmm = rmm,returnData = TRUE)




cleanEx()
nameEx("rmmCleanNULLs")
### * rmmCleanNULLs

flush(stderr()); flush(stdout())

### Name: rmmCleanNULLs
### Title: Remove NULL entries range model metadata list
### Aliases: rmmCleanNULLs

### ** Examples

# see vignette('rmm_vignette')



cleanEx()
nameEx("rmmDataDictionary")
### * rmmDataDictionary

flush(stderr()); flush(stdout())

### Name: rmmDataDictionary
### Title: Open range model metadata dictionary.
### Aliases: rmmDataDictionary

### ** Examples

dd=rmmDataDictionary()



cleanEx()
nameEx("rmmFamilies")
### * rmmFamilies

flush(stderr()); flush(stdout())

### Name: rmmFamilies
### Title: Print supported family names for rmm objects
### Aliases: rmmFamilies

### ** Examples

rmmFamilies()



cleanEx()
nameEx("rmmTemplate")
### * rmmTemplate

flush(stderr()); flush(stdout())

### Name: rmmTemplate
### Title: Range modeling metadata
### Aliases: rmmTemplate

### ** Examples

rmm1=rmmTemplate()
rmm2=rmmTemplate(family=c('base'))
str(rmm2)




cleanEx()
nameEx("rmmToCSV")
### * rmmToCSV

flush(stderr()); flush(stdout())

### Name: rmmToCSV
### Title: Create .csv File From rangeModelMetaData Object
### Aliases: rmmToCSV

### ** Examples

rmm=rmmTemplate()
rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
                       pattern='grd', full.names=TRUE)
#make a stack of the rasters
env=raster::stack(rasterFiles)
# for fitting environment
rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
# for the first environment that you're transfering to
rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
# for the second environment that you're transfering to, etc.
rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
## Not run: 
##D tmp=rmmToCSV(rmm,file='somePathOnYourMachine/rmm_example.csv')
## End(Not run)



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
