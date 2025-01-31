pkgname <- "NSR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('NSR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NSR")
### * NSR

flush(stderr()); flush(stdout())

### Name: NSR
### Title: Check the native status for plant species in a political region
### Aliases: NSR

### ** Examples

## Not run: 
##D 
##D results <- NSR(occurrence_dataframe = nsr_testfile)
##D   
##D # Inspect the results
##D head(results, 10)
##D # That's a lot of columns. Let's display one row vertically
##D # to get a better understanding of the output fields
##D results.t <- t(results[,2:ncol(results)]) 
##D results.t[,1,drop =FALSE]
##D # Summarize the main results
##D results[ 1:10, 
##D c("species", "country", "state_province", "native_status", "native_status_reason")]
##D 
##D # Compare summary flag isIntroduced to more detailed native_status values
##D # and inspect souces consulted
##D results[ 1:10, 
##D c("species", "country", "state_province", "native_status", "isIntroduced", "native_status_sources")]
##D 
##D 
##D 
##D 
## End(Not run)



cleanEx()
nameEx("NSR_citations")
### * NSR_citations

flush(stderr()); flush(stdout())

### Name: NSR_citations
### Title: Get citation information
### Aliases: NSR_citations

### ** Examples

{
citation_info <- NSR_citations()
}




cleanEx()
nameEx("NSR_data_dictionary")
### * NSR_data_dictionary

flush(stderr()); flush(stdout())

### Name: NSR_data_dictionary
### Title: Get NSR data dictionary
### Aliases: NSR_data_dictionary

### ** Examples

{
NSR_fields <- NSR_data_dictionary()

status_codes <- NSR_data_dictionary(native_status = TRUE)
}




cleanEx()
nameEx("NSR_metadata")
### * NSR_metadata

flush(stderr()); flush(stdout())

### Name: NSR_metadata
### Title: Get NSR metadata
### Aliases: NSR_metadata

### ** Examples

{
metadata <- NSR_metadata()
}




cleanEx()
nameEx("NSR_political_divisions")
### * NSR_political_divisions

flush(stderr()); flush(stdout())

### Name: NSR_political_divisions
### Title: Get information on political divisions with checklists within
###   the NSR
### Aliases: NSR_political_divisions

### ** Examples

## Not run: 
##D 
##D #To get a list of all political divisions with comprehensive checklists:
##D checklists_per_country <- NSR_political_divisions()
##D 
##D #To get a list of all checklists the associated countries, set "by_country" to FALSE
##D countries_per_checklist <- NSR_political_divisions(by_country=FALSE)
##D 
## End(Not run)



cleanEx()
nameEx("NSR_simple")
### * NSR_simple

flush(stderr()); flush(stdout())

### Name: NSR_simple
### Title: Check the native status for plant species in a political region
### Aliases: NSR_simple

### ** Examples

## Not run: 
##D 
##D results <- NSR_simple(species = "Acer rubrum",
##D            country = "Canada",state_province = "Ontario")
##D 
##D results <- NSR_simple(species = c("Acer rubrum", "Aspen tremuloides") , 
##D            country = c("Canada","Canada"),state_province = c("Ontario","Ontario"))
##D 
## End(Not run)



cleanEx()
nameEx("NSR_sources")
### * NSR_sources

flush(stderr()); flush(stdout())

### Name: NSR_sources
### Title: Get information on sources used by the NSR
### Aliases: NSR_sources

### ** Examples

{
sources <- NSR_sources()
}




cleanEx()
nameEx("NSR_template")
### * NSR_template

flush(stderr()); flush(stdout())

### Name: NSR_template
### Title: Make a template for an NSR query
### Aliases: NSR_template

### ** Examples

## Not run: 
##D 
##D template<-NSR_template(nrow = 2)
##D template$genus<-"Acer"
##D template$species<-c("Acer rubrum", "Acer saccharum")
##D template$country<-"Canada"
##D template$user_id<-1:2
##D results <- NSR(occurrence_dataframe = template)
##D 
## End(Not run)



cleanEx()
nameEx("NSR_version")
### * NSR_version

flush(stderr()); flush(stdout())

### Name: NSR_version
### Title: Get metadata on current NSR version
### Aliases: NSR_version

### ** Examples

{
NSR_version_metadata <- NSR_version()
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
