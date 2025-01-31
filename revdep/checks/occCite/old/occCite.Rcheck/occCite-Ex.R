pkgname <- "occCite"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('occCite')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("GBIFLogin-class")
### * GBIFLogin-class

flush(stderr()); flush(stdout())

### Name: GBIFLogin-class
### Title: GBIFLogin Data Class
### Aliases: GBIFLogin-class GBIFLogin

### ** Examples





cleanEx()
nameEx("GBIFLoginManager")
### * GBIFLoginManager

flush(stderr()); flush(stdout())

### Name: GBIFLoginManager
### Title: GBIF Login Manager
### Aliases: GBIFLoginManager

### ** Examples

## Inputting user particulars
## Not run: 
##D myLogin <- GBIFLoginManager(
##D   user = "theWoman",
##D   email = "ireneAdler@laScala.org",
##D   pwd = "sh3r"
##D )
## End(Not run)

## Not run: 
##D ## Can also be mined from your system environment
##D myLogin <- GBIFLoginManager(
##D   user = NULL,
##D   email = NULL, pwd = NULL
##D )
## End(Not run)




cleanEx()
nameEx("getBIENpoints")
### * getBIENpoints

flush(stderr()); flush(stdout())

### Name: getBIENpoints
### Title: Download occurrence points from BIEN
### Aliases: getBIENpoints

### ** Examples

## Not run: 
##D getBIENpoints(taxon = "Protea cynaroides")
## End(Not run)




cleanEx()
nameEx("getGBIFpoints")
### * getGBIFpoints

flush(stderr()); flush(stdout())

### Name: getGBIFpoints
### Title: Download occurrences from GBIF
### Aliases: getGBIFpoints

### ** Examples

## Not run: 
##D getGBIFpoints(
##D   taxon = "Gadus morhua",
##D   GBIFLogin = myGBIFLogin,
##D   GBIFDownloadDirectory = NULL
##D )
## End(Not run)




cleanEx()
nameEx("myOccCiteObject")
### * myOccCiteObject

flush(stderr()); flush(stdout())

### Name: myOccCiteObject
### Title: Results of an occCite search for *Protea cynaroides*
### Aliases: myOccCiteObject
### Keywords: datasets

### ** Examples




cleanEx()
nameEx("occCitation")
### * occCitation

flush(stderr()); flush(stdout())

### Name: occCitation
### Title: Occurrence Citations
### Aliases: occCitation

### ** Examples

## Not run: 
##D data(myOccCiteObject)
##D myCitations <- occCitation(x = myOccCiteObject)
## End(Not run)



cleanEx()
nameEx("occCiteMap")
### * occCiteMap

flush(stderr()); flush(stdout())

### Name: occCiteMap
### Title: Generating a map of downloaded points
### Aliases: occCiteMap

### ** Examples

## Not run: 
##D data(myOccCiteObject)
##D occCiteMap(myOccCiteObject, cluster = FALSE)
## End(Not run)




cleanEx()
nameEx("occQuery")
### * occQuery

flush(stderr()); flush(stdout())

### Name: occQuery
### Title: Query from Taxon List
### Aliases: occQuery

### ** Examples

## Not run: 
##D ## If you have already created a occCite object, and have not previously
##D ## downloaded GBIF data.
##D occQuery(
##D   x = myOccCiteObject,
##D   datasources = c("gbif", "bien"),
##D   GBIFLogin = myLogin,
##D   GBIFDownloadDirectory = "./Desktop",
##D   loadLocalGBIFDownload = F
##D )
##D 
##D ## If you don't have an occCite object yet
##D occQuery(
##D   x = c("Buteo buteo", "Protea cynaroides"),
##D   datasources = c("gbif", "bien"),
##D   GBIFLogin = myLogin,
##D   GBIFDownloadDirectory = "./Desktop",
##D   loadLocalGBIFDownload = F
##D )
##D 
##D ## If you have previously downloaded occurrence data from GBIF
##D ## and saved it in a folder called "GBIFDownloads".
##D occQuery(
##D   x = c("Buteo buteo", "Protea cynaroides"),
##D   datasources = c("gbif", "bien"),
##D   GBIFLogin = myLogin,
##D   GBIFDownloadDirectory = "./Desktop/GBIFDownloads",
##D   loadLocalGBIFDownload = T
##D )
## End(Not run)




cleanEx()
nameEx("plot.occCiteData")
### * plot.occCiteData

flush(stderr()); flush(stdout())

### Name: plot.occCiteData
### Title: Plotting summary figures for occCite search results
### Aliases: plot.occCiteData

### ** Examples

data(myOccCiteObject)
plot(
  x = myOccCiteObject, bySpecies = FALSE,
  plotTypes = c("yearHistogram", "source", "aggregator")
)



cleanEx()
nameEx("prevGBIFdownload")
### * prevGBIFdownload

flush(stderr()); flush(stdout())

### Name: prevGBIFdownload
### Title: Download previously-prepared GBIF data sets
### Aliases: prevGBIFdownload

### ** Examples

## Not run: 
##D GBIFLogin <- GBIFLoginManager(
##D   user = "theWoman",
##D   email = "ireneAdler@laScala.org",
##D   pwd = "sh3r"
##D )
##D taxKey <- rgbif::name_suggest(
##D   q = "Protea cynaroides",
##D   rank = "species"
##D )$key[1]
##D prevGBIFdownload(
##D   taxonKey = taxKey,
##D   GBIFLogin = myGBIFLogin
##D )
## End(Not run)




cleanEx()
nameEx("print.occCiteCitation")
### * print.occCiteCitation

flush(stderr()); flush(stdout())

### Name: print.occCiteCitation
### Title: Print occCite citation object
### Aliases: print.occCiteCitation

### ** Examples


# Print citations for all species together
data(myOccCiteObject)
print(myOccCiteObject)

# Print citations for each species individually
data(myOccCiteObject)
print(myOccCiteObject, bySpecies = TRUE)



cleanEx()
nameEx("studyTaxonList")
### * studyTaxonList

flush(stderr()); flush(stdout())

### Name: studyTaxonList
### Title: Study Taxon List
### Aliases: studyTaxonList

### ** Examples

## Inputting a vector of taxon names
studyTaxonList(
  x = c(
    "Buteo buteo",
    "Buteo buteo hartedi",
    "Buteo japonicus"
  ),
  datasources = c("National Center for Biotechnology Information")
)




cleanEx()
nameEx("summary.occCiteData")
### * summary.occCiteData

flush(stderr()); flush(stdout())

### Name: summary.occCiteData
### Title: Summary for occCite data objects
### Aliases: summary.occCiteData

### ** Examples


data(myOccCiteObject)
summary(myOccCiteObject)



cleanEx()
nameEx("taxonRectification")
### * taxonRectification

flush(stderr()); flush(stdout())

### Name: taxonRectification
### Title: Taxon Rectification
### Aliases: taxonRectification

### ** Examples

# Inputting taxonomic name and specifying what taxonomic sources to search
taxonRectification(
  taxName = "Buteo buteo hartedi",
  datasources = "National Center for Biotechnology Information",
  skipTaxize = TRUE
)



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
