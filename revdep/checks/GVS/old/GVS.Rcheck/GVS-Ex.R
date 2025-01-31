pkgname <- "GVS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('GVS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("GVS")
### * GVS

flush(stderr()); flush(stdout())

### Name: GVS
### Title: Check the validity of coordinates
### Aliases: GVS

### ** Examples




cleanEx()
nameEx("GVS_citations")
### * GVS_citations

flush(stderr()); flush(stdout())

### Name: GVS_citations
### Title: Get citation information
### Aliases: GVS_citations

### ** Examples

{
citation_info <- GVS_citations()
}




cleanEx()
nameEx("GVS_collaborators")
### * GVS_collaborators

flush(stderr()); flush(stdout())

### Name: GVS_collaborators
### Title: Get collaborator information
### Aliases: GVS_collaborators

### ** Examples

{
collaborator_info <- GVS_collaborators()
}




cleanEx()
nameEx("GVS_data_dictionary")
### * GVS_data_dictionary

flush(stderr()); flush(stdout())

### Name: GVS_data_dictionary
### Title: Get data dictionary
### Aliases: GVS_data_dictionary

### ** Examples

{
data_dictionary <- GVS_data_dictionary()
}




cleanEx()
nameEx("GVS_metadata")
### * GVS_metadata

flush(stderr()); flush(stdout())

### Name: GVS_metadata
### Title: Get GVS metadata
### Aliases: GVS_metadata

### ** Examples

{
metadata <- GVS_metadata()
}




cleanEx()
nameEx("GVS_sources")
### * GVS_sources

flush(stderr()); flush(stdout())

### Name: GVS_sources
### Title: Get information on sources used by the GVS
### Aliases: GVS_sources

### ** Examples

{
sources <- GVS_sources()
}




cleanEx()
nameEx("GVS_version")
### * GVS_version

flush(stderr()); flush(stdout())

### Name: GVS_version
### Title: Get metadata on current GVS version
### Aliases: GVS_version

### ** Examples

{
NSR_version_metadata <- GVS_version()
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
