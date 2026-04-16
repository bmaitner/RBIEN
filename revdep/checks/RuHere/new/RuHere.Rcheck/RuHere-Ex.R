pkgname <- "RuHere"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RuHere')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("available_datasets")
### * available_datasets

flush(stderr()); flush(stdout())

### Name: available_datasets
### Title: Check the available distribution datasets for a set of species
### Aliases: available_datasets

### ** Examples

# Set directory where datasets were saved
# Here, we'll use the directory where the example datasets are stored
datadir <- system.file("extdata", "datasets",  package = "RuHere")
# Check available datasets
d <- available_datasets(data_dir = datadir,
                        species = c("Araucaria angustifolia",
                                    "Handroanthus serratifolius",
                                    "Cyanocorax caeruleus"))
# Check available datasets and return distribution
d2 <- available_datasets(data_dir = datadir,
                         species = c("Araucaria angustifolia",
                                     "Handroanthus serratifolius",
                                     "Cyanocorax caeruleus"),
                         return_distribution = TRUE)




cleanEx()
nameEx("bien_here")
### * bien_here

flush(stderr()); flush(stdout())

### Name: bien_here
### Title: Download species distribution information from BIEN
### Aliases: bien_here

### ** Examples




cleanEx()
nameEx("bind_here")
### * bind_here

flush(stderr()); flush(stdout())

### Name: bind_here
### Title: Bind occurrences after standardizing columns
### Aliases: bind_here

### ** Examples

# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Import and standardize SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Import and standardize BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Import and standardize idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
# Merge all
all_occ <- bind_here(gbif_standardized, splink_standardized,
                      bien_standardized, idig_standardized)




cleanEx()
nameEx("check_countries")
### * check_countries

flush(stderr()); flush(stdout())

### Name: check_countries
### Title: Check if the records fall in the country assigned in the
###   metadata
### Aliases: check_countries

### ** Examples

# Load example data
data("occurrences", package = "RuHere") #Import data example
# Standardize country names
occ_country <- standardize_countries(occ = occurrences,
                                     return_dictionary = FALSE)
# Check whether records fall within assigned countries
occ_country_checked <- check_countries(occ = occ_country,
                                       country_column = "country_suggested")



cleanEx()
nameEx("check_states")
### * check_states

flush(stderr()); flush(stdout())

### Name: check_states
### Title: Check if the records fall in the state assigned in the metadata
### Aliases: check_states

### ** Examples

# Load example data
data("occurrences", package = "RuHere") #Import data example
# Subset occurrences for Araucaria angustifolia
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Standardize country names
occ_country <- standardize_countries(occ = occ,
                                     return_dictionary = FALSE)
# Standardize state names
occ_state <- standardize_states(occ = occ_country,
                                country_column = "country_suggested",
                                return_dictionary = FALSE)
# Check whether records fall within assigned states
occ_state_checked <- check_states(occ = occ_state,
                                    state_column = "state_suggested")



cleanEx()
nameEx("country_dictionary")
### * country_dictionary

flush(stderr()); flush(stdout())

### Name: country_dictionary
### Title: Country dictionary for standardizing country names and codes
### Aliases: country_dictionary
### Keywords: datasets

### ** Examples

data(country_dictionary)

head(country_dictionary$country_name)
head(country_dictionary$country_code)




cleanEx()
nameEx("country_from_coords")
### * country_from_coords

flush(stderr()); flush(stdout())

### Name: country_from_coords
### Title: Extract country from coordinates
### Aliases: country_from_coords

### ** Examples

# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
gbif_countries <- country_from_coords(occ = gbif_standardized)



cleanEx()
nameEx("create_metadata")
### * create_metadata

flush(stderr()); flush(stdout())

### Name: create_metadata
### Title: Create metadata template
### Aliases: create_metadata

### ** Examples

# Load data example
# Occurrences of Puma concolor from the atlanticr R package
data("puma_atlanticr", package = "RuHere")
# Create metadata to standardize the occurrences
puma_metadata <- create_metadata(occ = puma_atlanticr,
                                 scientificName = "actual_species_name",
                                 decimalLongitude = "longitude",
                                 decimalLatitude = "latitude",
                                 elevation = "altitude",
                                 country = "country",
                                 stateProvince = "state",
                                 municipality = "municipality",
                                 locality = "study_location",
                                 year = "year_finish",
                                 habitat = "vegetation_type",
                                 datasetName = "reference")
# Now, we can use this metadata to standardize the columns
puma_occ <- format_columns(occ = puma_atlanticr, metadata = puma_metadata,
                           binomial_from = "actual_species_name",
                           data_source = "atlanticr")




cleanEx()
nameEx("cultivated")
### * cultivated

flush(stderr()); flush(stdout())

### Name: cultivated
### Title: Dictionary of terms used to flag cultivated individuals
### Aliases: cultivated
### Keywords: datasets

### ** Examples

data(cultivated)

cultivated$cultivated
cultivated$not_cultivated




cleanEx()
nameEx("fake_data")
### * fake_data

flush(stderr()); flush(stdout())

### Name: fake_data
### Title: Fake occurrence data for testing coordinate validation functions
### Aliases: fake_data
### Keywords: datasets

### ** Examples

data(fake_data)



cleanEx()
nameEx("faunabr_here")
### * faunabr_here

flush(stderr()); flush(stdout())

### Name: faunabr_here
### Title: Download the latest version of the Fauna do Brazil (Taxonomic
###   Catalog of the Brazilian Fauna)
### Aliases: faunabr_here

### ** Examples




cleanEx()
nameEx("fix_countries")
### * fix_countries

flush(stderr()); flush(stdout())

### Name: fix_countries
### Title: Identify and correct coordinates based on country information
### Aliases: fix_countries

### ** Examples

# Load example data
data("occurrences", package = "RuHere") # Import example data

# Standardize country names
occ_country <- standardize_countries(occ = occurrences,
                                     return_dictionary = FALSE)

# Check whether records fall within the assigned countries
occ_country_checked <- check_countries(occ = occ_country,
                                       country_column = "country_suggested")

# Fix records with incorrect or misassigned countries
occ_country_fixed <- fix_countries(occ = occ_country_checked,
                                   country_column = "country_suggested")



cleanEx()
nameEx("fix_states")
### * fix_states

flush(stderr()); flush(stdout())

### Name: fix_states
### Title: Identify and correct coordinates based on state information
### Aliases: fix_states

### ** Examples

# Load example data
data("occurrences", package = "RuHere") # Import example data
# Subset records of Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Standardize country names
occ_country <- standardize_countries(occ = occ,
                                     return_dictionary = FALSE)

# Standardize state names
occ_state <- standardize_states(occ = occ_country,
                                country_column = "country_suggested",
                                return_dictionary = FALSE)

# Check whether records fall within the assigned states
occ_states_checked <- check_states(occ = occ_state,
                                   state_column = "state_suggested")

# Fix records with incorrect or misassigned states
occ_states_fixed <- fix_states(occ = occ_states_checked,
                               state_column = "state_suggested")



cleanEx()
nameEx("flag_bien")
### * flag_bien

flush(stderr()); flush(stdout())

### Name: flag_bien
### Title: Identify records outside natural ranges according to BIEN
### Aliases: flag_bien

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences for golden trumpet tree
occ <- occurrences[occurrences$species == "Handroanthus serratifolius", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'bien_here()' beforehand to download the necessary data files
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using BIEN specialist information
occ_bien <- flag_bien(data_dir = dataset_dir, occ = occ)




cleanEx()
nameEx("flag_colors")
### * flag_colors

flush(stderr()); flush(stdout())

### Name: flag_colors
### Title: Color palette for flagged records
### Aliases: flag_colors
### Keywords: datasets

### ** Examples

data(flag_colors)

# View all flag categories and their colors
flag_colors




cleanEx()
nameEx("flag_consensus")
### * flag_consensus

flush(stderr()); flush(stdout())

### Name: flag_consensus
### Title: Get consensus across multiple flags
### Aliases: flag_consensus

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")

# Get consensus using florabr, wcvp, and iucn flags
 # Valid (TRUE) only when all flags are TRUE
occ_consensus_all <- flag_consensus(occ = occ_flagged,
                                    flags = c("florabr", "wcvp", "iucn"),
                                    consensus_rule = "all_true")
# Valid (TRUE) when at least one flag is TRUE
occ_consensus_any <- flag_consensus(occ = occ_flagged,
                                    flags = c("florabr", "wcvp", "iucn"),
                                    consensus_rule = "any_true")




cleanEx()
nameEx("flag_cultivated")
### * flag_cultivated

flush(stderr()); flush(stdout())

### Name: flag_cultivated
### Title: Flag occurrence records of cultived individuals
### Aliases: flag_cultivated

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Flag fossil records
occ_cultivated <- flag_cultivated(occ = occurrences)



cleanEx()
nameEx("flag_duplicates")
### * flag_duplicates

flush(stderr()); flush(stdout())

### Name: flag_duplicates
### Title: Flag duplicated records
### Aliases: flag_duplicates

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Duplicate some records as example
occurrences <- rbind(occurrences[1:1000, ], occurrences[1:100,])
# Flag duplicates
occ_dup <- flag_duplicates(occ = occurrences)
sum(!occ_dup$duplicated_flag) #Number of duplicated records



cleanEx()
nameEx("flag_env_moran")
### * flag_env_moran

flush(stderr()); flush(stdout())

### Name: flag_env_moran
### Title: Select Environmentally Thinned Occurrences Using Moran's I
###   Autocorrelation
### Aliases: flag_env_moran

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Subset occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Select thinned occurrences
occ_env_moran <- flag_env_moran(occ = occ,
                                  n_bins = c(5, 10, 20, 30, 40, 50),
                                  env_layers = r)
# Selected number of bins
occ_env_moran$n_bins
# Number of flagged and unflagged records
sum(occ_env_moran$occ$thin_env_flag) #Retained
sum(!occ_env_moran$occ$thin_env_flag) #Flagged for thinning out
# Results os the spatial autocorrelation analysis
occ_env_moran$imoran



cleanEx()
nameEx("flag_faunabr")
### * flag_faunabr

flush(stderr()); flush(stdout())

### Name: flag_faunabr
### Title: Identify records outside natural ranges according to Fauna do
###   Brasil
### Aliases: flag_faunabr

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Azure Jay
occ <- occurrences[occurrences$species == "Cyanocorax caeruleus", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'faunabr_here()' beforehand to download the necessary data files for your species
dataset_dir <- system.file("extdata/datasets", package = "RuHere")
# Flag records using faunabr specialist information
occ_fauna <- flag_faunabr(data_dir = dataset_dir, occ = occ)



cleanEx()
nameEx("flag_florabr")
### * flag_florabr

flush(stderr()); flush(stdout())

### Name: flag_florabr
### Title: Identify records outside natural ranges according to Flora e
###   Funga do Brasil
### Aliases: flag_florabr

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'florabr_here()' beforehand to download the necessary data files for your species
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using specialist information from Flora do Brasil
occ_flora <- flag_florabr(data_dir = dataset_dir, occ = occ)




cleanEx()
nameEx("flag_fossil")
### * flag_fossil

flush(stderr()); flush(stdout())

### Name: flag_fossil
### Title: Flag fossil records
### Aliases: flag_fossil

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Flag fossil records
occ_fossil <- flag_fossil(occ = occurrences)



cleanEx()
nameEx("flag_geo_moran")
### * flag_geo_moran

flush(stderr()); flush(stdout())

### Name: flag_geo_moran
### Title: Select Spatially Thinned Occurrences Using Moran's I
###   Autocorrelation
### Aliases: flag_geo_moran

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Subset occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Select thinned occurrences
occ_geo_moran <- flag_geo_moran(occ = occ, d = c(5, 10, 20, 30),
                                  env_layers = r)
# Selected distance
occ_geo_moran$distance
# Number of flagged and unflagged records
sum(occ_geo_moran$occ$thin_geo_flag) #Retained
sum(!occ_geo_moran$occ$thin_geo_flag) #Flagged for thinning out
# Results os the spatial autocorrelation analysis
occ_geo_moran$imoran




cleanEx()
nameEx("flag_inaturalist")
### * flag_inaturalist

flush(stderr()); flush(stdout())

### Name: flag_inaturalist
### Title: Flag occurrence records sourced from iNaturalist
### Aliases: flag_inaturalist

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Flag only iNaturalist records without Research Grade
occ_inat <- flag_inaturalist(occ = occurrences, research_grade = FALSE)
table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
# Flag all iNaturalist records (including Research Grade)
occ_inat <- flag_inaturalist(occ = occurrences, research_grade = TRUE)
table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)



cleanEx()
nameEx("flag_iucn")
### * flag_iucn

flush(stderr()); flush(stdout())

### Name: flag_iucn
### Title: Identify records outside natural ranges according to the IUCN
### Aliases: flag_iucn

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in tha package
# You must run 'iucn_here()' beforehand to download the necessary data files
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using IUCN specialist information
occ_iucn <- flag_iucn(data_dir = dataset_dir, occ = occ)




cleanEx()
nameEx("flag_wcvp")
### * flag_wcvp

flush(stderr()); flush(stdout())

### Name: flag_wcvp
### Title: Identify records outside natural ranges according to the World
###   Checklist of Vascular Plants
### Aliases: flag_wcvp

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences for Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'wcvp_here()' beforehand to download the necessary data files
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using WCVP specialist information
occ_wcvp <- flag_wcvp(data_dir = dataset_dir, occ = occ)




cleanEx()
nameEx("flag_year")
### * flag_year

flush(stderr()); flush(stdout())

### Name: flag_year
### Title: Flag records outside a year range
### Aliases: flag_year

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Flag records collected before 1980 and after 2010
occ_year <- flag_year(occ = occurrences, lower_limit = 1980,
                      upper_limit = 2010)



cleanEx()
nameEx("florabr_here")
### * florabr_here

flush(stderr()); flush(stdout())

### Name: florabr_here
### Title: Download the latest version of Flora e Funga do Brasil database
### Aliases: florabr_here

### ** Examples




cleanEx()
nameEx("format_columns")
### * format_columns

flush(stderr()); flush(stdout())

### Name: format_columns
### Title: Format and standardize column names and data types of an
###   occurrence dataset
### Aliases: format_columns

### ** Examples

# Example with GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Example with SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Example with BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Example with idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")




cleanEx()
nameEx("get_bien")
### * get_bien

flush(stderr()); flush(stdout())

### Name: get_bien
### Title: Download occurrence records from BIEN
### Aliases: get_bien

### ** Examples





cleanEx()
nameEx("get_env_bins")
### * get_env_bins

flush(stderr()); flush(stdout())

### Name: get_env_bins
### Title: Identify Environmental Blocks and Group Nearby Records in
###   Environmental Space
### Aliases: get_env_bins

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Get bins
b <- get_env_bins(occ = occ, env_layers = r, n_bins = 5)




cleanEx()
nameEx("get_idigbio")
### * get_idigbio

flush(stderr()); flush(stdout())

### Name: get_idigbio
### Title: get_idigbio
### Aliases: get_idigbio

### ** Examples





cleanEx()
nameEx("get_specieslink")
### * get_specieslink

flush(stderr()); flush(stdout())

### Name: get_specieslink
### Title: Download occurrence records from SpeciesLink
### Aliases: get_specieslink

### ** Examples

## Not run: 
##D # Retrieve records for Arecaceae in São Paulo
##D res <- get_specieslink(
##D   family = "Arecaceae",
##D   country = "Brazil",
##D   stateProvince = "São Paulo",
##D   basisOfRecord = "PreservedSpecimen",
##D   limit = 10
##D )
##D 
##D # Save results as compressed CSV
##D get_specieslink(
##D   family = "Arecaceae",
##D   country = "Brazil",
##D   save = TRUE,
##D   dir = tempdir(),
##D   filename = "arecaceae_sp",
##D   compress = TRUE
##D )
## End(Not run)




cleanEx()
nameEx("ggmap_here")
### * ggmap_here

flush(stderr()); flush(stdout())

### Name: ggmap_here
### Title: Static Visualization of Occurrence Flags with ggplot
### Aliases: ggmap_here

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")
# Visualize all flags with ggplot
ggmap_here(occ = occ_flagged)
# Visualize each flag in a separate panel
ggmap_here(occ = occ_flagged, facet_wrap = TRUE)



cleanEx()
nameEx("ggrid_here")
### * ggrid_here

flush(stderr()); flush(stdout())

### Name: ggrid_here
### Title: Static Visualization of Richness and Trait Maps
### Aliases: ggrid_here

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")

# Simple richness map
r_records <- richness_here(occ_flagged, summary = "records", res = 2)
ggrid_here(r_records)

# Density of specific flags
# Let's see where 'florabr' flags are concentrated
r_flags <- richness_here(occ_flagged, summary = "records",
                         field = "florabr_flag",
                         field_name = "Records flagged by florabr",
                         fun = function(x, ...) sum(!x, na.rm = TRUE),
                         res = 2)
ggrid_here(r_flags)




cleanEx()
nameEx("import_gbif")
### * import_gbif

flush(stderr()); flush(stdout())

### Name: import_gbif
### Title: Import a download requested from GBIF
### Aliases: import_gbif

### ** Examples

 ## Not run: 
##D # Prepare data to request GBIF download
##D gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
##D # Submit a request to download occurrences
##D gbif_requested <- request_gbif(gbif_info = gbif_prepared)
##D # Check progress
##D rgbif::occ_download_wait(gbif_requested)
##D # After succeeded, import data
##D occ_gbif <- import_gbif(request_key = gbif_requested)
## End(Not run)



cleanEx()
nameEx("iucn_here")
### * iucn_here

flush(stderr()); flush(stdout())

### Name: iucn_here
### Title: Download species distribution information from IUCN
### Aliases: iucn_here

### ** Examples

## Not run: 
##D # Define a directory to save the data
##D data_dir <- tempdir() # Here, a temporary directory
##D 
##D # Download species distribution information from IUCN
##D iucn_here(data_dir = data_dir, species = "Araucaria angustifolia")
## End(Not run)



cleanEx()
nameEx("map_here")
### * map_here

flush(stderr()); flush(stdout())

### Name: map_here
### Title: Interactive Visualization of Occurrence Flags with mapview
### Aliases: map_here

### ** Examples




cleanEx()
nameEx("moranfast")
### * moranfast

flush(stderr()); flush(stdout())

### Name: moranfast
### Title: Fast Moran's I Autocorrelation Index
### Aliases: moranfast

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences of Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Extract values for bio_1
bio_1 <- terra::extract(r$bio_1,
                        occ[, c("decimalLongitude", "decimalLatitude")],
                        ID = FALSE, xy = TRUE)
#Remove NAs
bio_1 <- na.omit(bio_1)
# Convert values to numeric
v <- as.numeric(bio_1$bio_1)
# Compute geographic distance matrix
d <- fields::rdist.earth(x1 = as.matrix(bio_1[, c("x", "y")]), miles = FALSE)
# Inverse-distance weights
d <- 1/d
# Fill diagonal with 0
diag(d) <- 0
# Remove finite values
d[is.infinite(d)] <- 0
# Compute Moran's I
m <- moranfast(x = v, weight = d, scale = TRUE)
# Print results
m



cleanEx()
nameEx("occ_bien")
### * occ_bien

flush(stderr()); flush(stdout())

### Name: occ_bien
### Title: Occurrence records of Yellow Trumpet Tree from BIEN
### Aliases: occ_bien
### Keywords: datasets

### ** Examples

# View dataset
head(occ_bien)

# Number of records
nrow(occ_bien)





cleanEx()
nameEx("occ_flagged")
### * occ_flagged

flush(stderr()); flush(stdout())

### Name: occ_flagged
### Title: Flagged occurrence records of _Araucaria angustifolia_
### Aliases: occ_flagged
### Keywords: datasets

### ** Examples

# First rows
head(occ_flagged)

# Count flagged vs. unflagged records
table(occ_flagged$correct_country)





cleanEx()
nameEx("occ_gbif")
### * occ_gbif

flush(stderr()); flush(stdout())

### Name: occ_gbif
### Title: Occurrence records of _Araucaria angustifolia_ from GBIF
### Aliases: occ_gbif
### Keywords: datasets

### ** Examples

# Preview dataset
head(occ_gbif)

# Number of cleaned records
nrow(occ_gbif)




cleanEx()
nameEx("occ_idig")
### * occ_idig

flush(stderr()); flush(stdout())

### Name: occ_idig
### Title: Occurrence records of azure jay from iDigBio
### Aliases: occ_idig
### Keywords: datasets

### ** Examples

# First rows
head(occ_idig)

# Number of cleaned records
nrow(occ_idig)




cleanEx()
nameEx("occ_splink")
### * occ_splink

flush(stderr()); flush(stdout())

### Name: occ_splink
### Title: Occurrence records of azure jay from SpeciesLink
### Aliases: occ_splink
### Keywords: datasets

### ** Examples

# First rows
head(occ_splink)

# Number of cleaned records
nrow(occ_splink)




cleanEx()
nameEx("occurrences")
### * occurrences

flush(stderr()); flush(stdout())

### Name: occurrences
### Title: Integrated occurrence dataset for three example species
### Aliases: occurrences
### Keywords: datasets

### ** Examples

# Show the first rows
head(occurrences)

# Number of occurrences per species
table(occurrences$species)




cleanEx()
nameEx("plot_env_bins")
### * plot_env_bins

flush(stderr()); flush(stdout())

### Name: plot_env_bins
### Title: Plot Environmental Bins (2D Projection)
### Aliases: plot_env_bins

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Get bins
b <- get_env_bins(occ = occ, env_layers = r, n_bins = 10)
# Plot
plot_env_bins(b, x_var = "bio_1", y_var = "bio_12",
              xlab = "Temperature", ylab = "Precipitation")



cleanEx()
nameEx("prepare_gbif_download")
### * prepare_gbif_download

flush(stderr()); flush(stdout())

### Name: prepare_gbif_download
### Title: Prepare data to request GBIF download
### Aliases: prepare_gbif_download

### ** Examples




cleanEx()
nameEx("prepared_metadata")
### * prepared_metadata

flush(stderr()); flush(stdout())

### Name: prepared_metadata
### Title: Metadata templates used internally by 'format_columns()'
### Aliases: prepared_metadata
### Keywords: datasets

### ** Examples

# View template for GBIF records
prepared_metadata$gbif





cleanEx()
nameEx("puma_atlanticr")
### * puma_atlanticr

flush(stderr()); flush(stdout())

### Name: puma_atlanticr
### Title: Occurrence records of _Puma concolor_ from AtlanticR
### Aliases: puma_atlanticr
### Keywords: datasets

### ** Examples

# Preview first rows
head(puma_atlanticr)

# Count occurrences per year
table(puma_atlanticr$year)





cleanEx()
nameEx("remove_accent")
### * remove_accent

flush(stderr()); flush(stdout())

### Name: remove_accent
### Title: Remove accents and special characters from strings
### Aliases: remove_accent

### ** Examples

remove_accent(c("Colômbia", "São Paulo"))



cleanEx()
nameEx("remove_flagged")
### * remove_flagged

flush(stderr()); flush(stdout())

### Name: remove_flagged
### Title: Remove flagged records
### Aliases: remove_flagged

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")

# Remove all flagged records
occ_valid <- remove_flagged(occ = occ_flagged)

# Remove flagged records and force removal of some unflagged records
to_remove <- c("gbif_5987", "specieslink_2301", "gbif_18761")
occ_valid2 <- remove_flagged(occ = occ_flagged,
                             force_remove = to_remove)

# Remove flagged records but keep some flagged ones
to_keep <- c("gbif_14501", "gbif_12002", "gbif_5168")
occ_valid3 <- remove_flagged(occ = occ_flagged,
                            force_keep = to_keep)



cleanEx()
nameEx("remove_invalid_coordinates")
### * remove_invalid_coordinates

flush(stderr()); flush(stdout())

### Name: remove_invalid_coordinates
### Title: Identify and remove invalid coordinates
### Aliases: remove_invalid_coordinates

### ** Examples

# Create fake data example
occ <- data.frame("species" = "spp",
                  "decimalLongitude" = c(10, -190, 20, 50, NA),
                  "decimalLatitude" = c(20, 20, 240, 50, NA))
# Split valid and invalid coordinates
occ_valid <- remove_invalid_coordinates(occ)



cleanEx()
nameEx("request_gbif")
### * request_gbif

flush(stderr()); flush(stdout())

### Name: request_gbif
### Title: Submit a request to download occurrence data from GBIF.
### Aliases: request_gbif

### ** Examples

## Not run: 
##D # Prepare data to request GBIF download
##D gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
##D # Submit a request to download occurrences
##D gbif_requested <- request_gbif(gbif_info = gbif_prepared)
##D # Check progress
##D rgbif::occ_download_wait(gbif_requested)
## End(Not run)



cleanEx()
nameEx("richness_here")
### * richness_here

flush(stderr()); flush(stdout())

### Name: richness_here
### Title: Species Richness and Occurrence Summary Mapping
### Aliases: richness_here

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")

# Mapping the density of records
r_density <- richness_here(occ_flagged, summary = "records", res = 0.5)
ggrid_here(r_density)

# We can also summarize key features:
# 1. Identifying problematic regions by summing error flags
# We create a variable to store the sum of logical flags (TRUE = 1, FALSE = 0)
total_flags <- occ_flagged$florabr_flag +
 occ_flagged$wcvp_flag +
 occ_flagged$iucn_flag +
 occ_flagged$cultivated_flag +
 occ_flagged$inaturalist_flag +
 occ_flagged$duplicated_flag
names(total_flags) <- occ_flagged$record_id

# Using summary = "records" with to see the average accumulation of errors
# with fun = mean to see the average accumulation
r_flags <- richness_here(occ_flagged, summary = "records",
                        field = total_flags,
                        field_name = "Number of flags",
                        fun = mean, res = 0.5)
ggrid_here(r_flags)

# 2. Or we can summarize organisms traits spatially
# Simulating a trait (e.g., mass) for each unique record
spp <- unique(occ_flagged$record_id)
sim_mass <- setNames(runif(length(spp), 10, 50), spp)

r_trait <- richness_here(occ_flagged, summary = "records",
                        field = sim_mass, field_name = "Mass",
                        fun = mean, res = 0.5)
ggrid_here(r_trait)



cleanEx()
nameEx("set_gbif_credentials")
### * set_gbif_credentials

flush(stderr()); flush(stdout())

### Name: set_gbif_credentials
### Title: Store GBIF credentials
### Aliases: set_gbif_credentials

### ** Examples

## Not run: 
##D set_gbif_credentials(gbif_username = "my_username",
##D                      gbif_email = "my_email@example.com",
##D                      gbif_password = "my_password")
## End(Not run)



cleanEx()
nameEx("set_iucn_credentials")
### * set_iucn_credentials

flush(stderr()); flush(stdout())

### Name: set_iucn_credentials
### Title: Store SpeciesLink credential
### Aliases: set_iucn_credentials

### ** Examples

## Not run: 
##D set_iucn_credentials(iucn_key = "my_key")
## End(Not run)




cleanEx()
nameEx("set_specieslink_credentials")
### * set_specieslink_credentials

flush(stderr()); flush(stdout())

### Name: set_specieslink_credentials
### Title: Store SpeciesLink credential
### Aliases: set_specieslink_credentials

### ** Examples

## Not run: 
##D set_specieslink_credentials(specieslink_key = "my_key")
## End(Not run)




cleanEx()
nameEx("spatial_kde")
### * spatial_kde

flush(stderr()); flush(stdout())

### Name: spatial_kde
### Title: Kernel Density Estimation (Heatmap) for occurrence data
### Aliases: spatial_kde

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")
# Remove flagged records
occ <- remove_flagged(occ_flagged)
# Generate heatmap
heatmap <- spatial_kde(occ = occ, resolution = 0.25, buffer_extent = 50,
                       radius = 2)
# Plot heatmap with terra
terra::plot(heatmap)
# Plot heatmap with ggplot
ggmap_here(occ = occ, heatmap = heatmap)



cleanEx()
nameEx("spatialize")
### * spatialize

flush(stderr()); flush(stdout())

### Name: spatialize
### Title: Spatialize occurrence records
### Aliases: spatialize

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Spatialize the occurrence records
pts <- spatialize(occurrences)
# Plot the resulting SpatVector
terra::plot(pts)




cleanEx()
nameEx("standardize_countries")
### * standardize_countries

flush(stderr()); flush(stdout())

### Name: standardize_countries
### Title: Standardize country names
### Aliases: standardize_countries

### ** Examples

# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Import and standardize SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Import and standardize BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Import and standardize idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
# Merge all
all_occ <- bind_here(gbif_standardized, splink_standardized,
                     bien_standardized, idig_standardized)
# Standardize countries
occ_standardized <- standardize_countries(occ = all_occ)



cleanEx()
nameEx("standardize_states")
### * standardize_states

flush(stderr()); flush(stdout())

### Name: standardize_states
### Title: Standardize state names
### Aliases: standardize_states

### ** Examples

# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Import and standardize SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Import and standardize BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Import and standardize idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
# Merge all
all_occ <- bind_here(gbif_standardized, splink_standardized,
                     bien_standardized, idig_standardized)
# Standardize countries
occ_standardized <- standardize_countries(occ = all_occ)
# Standardize states
occ_standardized2 <- standardize_states(occ = occ_standardized$occ)



cleanEx()
nameEx("states")
### * states

flush(stderr()); flush(stdout())

### Name: states
### Title: Administrative Units (States, Provinces, and Regions)
### Aliases: states
### Keywords: datasets

### ** Examples

data(states)
states <- terra::unwrap(states)
terra::plot(states)



cleanEx()
nameEx("states_dictionary")
### * states_dictionary

flush(stderr()); flush(stdout())

### Name: states_dictionary
### Title: States dictionary for standardizing state and province names and
###   codes
### Aliases: states_dictionary
### Keywords: datasets

### ** Examples

data(states_dictionary)
head(states_dictionary$states_name)
head(states_dictionary$states_code)



cleanEx()
nameEx("states_from_coords")
### * states_from_coords

flush(stderr()); flush(stdout())

### Name: states_from_coords
### Title: Extract state from coordinates
### Aliases: states_from_coords

### ** Examples

# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
gbif_states <- states_from_coords(occ = gbif_standardized)



cleanEx()
nameEx("summarize_flags")
### * summarize_flags

flush(stderr()); flush(stdout())

### Name: summarize_flags
### Title: Summarize flags
### Aliases: summarize_flags

### ** Examples

# Load example data
data("occ_flagged", package = "RuHere")
# Summarize flags
sum_flags <- summarize_flags(occ = occ_flagged)
# Plot
sum_flags$plot_summary



cleanEx()
nameEx("thin_env")
### * thin_env

flush(stderr()); flush(stdout())

### Name: thin_env
### Title: Flag records that are close to each other in the enviromnetal
###   space
### Aliases: thin_env

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Flag records that are close to each other in the enviromnetal space
occ_env_thin <- thin_env(occ = occ, env_layers = r)
# Number of flagged (redundant) records
sum(!occ_env_thin$thin_env_flag) #Number of flagged records



cleanEx()
nameEx("thin_geo")
### * thin_geo

flush(stderr()); flush(stdout())

### Name: thin_geo
### Title: Flag records that are close to each other in the geographic
###   space
### Aliases: thin_geo

### ** Examples

# Load example data
data("occurrences", package = "RuHere")
# Subset occurrences for Araucaria angustifolia
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Thin records using a 10 km distance threshold
occ_thin <- thin_geo(occ = occ, d = 10)
sum(!occ_thin$thin_geo_flag)  # Number of records flagged for removal
# Prioritizing more recent records within each cluster
occ_thin_recent <- thin_geo(occ = occ, d = 10, prioritary_column = "year")
sum(!occ_thin_recent$thin_geo_flag)  # Number of records flagged for removal




cleanEx()
nameEx("wcvp_here")
### * wcvp_here

flush(stderr()); flush(stdout())

### Name: wcvp_here
### Title: Download distribution data from the World Checklist of Vascular
###   Plants (WCVP)
### Aliases: wcvp_here

### ** Examples




cleanEx()
nameEx("world")
### * world

flush(stderr()); flush(stdout())

### Name: world
### Title: World Countries
### Aliases: world
### Keywords: datasets

### ** Examples

data(world)
world <- terra::unwrap(world)
terra::plot(world)



cleanEx()
nameEx("worldclim")
### * worldclim

flush(stderr()); flush(stdout())

### Name: worldclim
### Title: Bioclimatic Variables from WorldClim (bio_1, bio_7, bio_12)
### Aliases: worldclim
### Keywords: datasets

### ** Examples

data(worldclim)
bioclim <- terra::unwrap(worldclim)
terra::plot(bioclim)



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
