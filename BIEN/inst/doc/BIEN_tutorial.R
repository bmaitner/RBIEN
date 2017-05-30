## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=F,warning=FALSE, results='hide'-----------------------------
library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sp) # A package for spatial data

## ---- eval=FALSE---------------------------------------------------------
#  vignette("BIEN")

## ------------------------------------------------------------------------
Xanthium_strumarium <- BIEN_occurrence_species(species = "Xanthium strumarium")

## ------------------------------------------------------------------------
str(Xanthium_strumarium)
head(Xanthium_strumarium)

## ------------------------------------------------------------------------
Xanthium_strumarium_full <- BIEN_occurrence_species(species = "Xanthium strumarium",cultivated = T,only.new.world = F,all.taxonomy = T,native.status = T,observation.type = T,political.boundaries = T)

str(Xanthium_strumarium_full)

## ------------------------------------------------------------------------
# Make a quick map to plot our points on

map('world',fill=T , col= "grey", bg="light blue") 

#Plot the points from the full query in red

points(cbind(Xanthium_strumarium_full$longitude,Xanthium_strumarium_full$latitude),col="red",pch=20,cex=1) 

# Plot the points from the default query in blue

points(cbind(Xanthium_strumarium$longitude,Xanthium_strumarium$latitude),col="blue",pch=20,cex=1) 


## ------------------------------------------------------------------------
Bahamas <- BIEN_occurrence_country(country =  "Bahamas")

#Let's see how many species we have
length(unique(Bahamas$scrubbed_species_binomial))
#Nearly 1000 species, not bad.

#Now, let's take a look at where those occurrences are:
map(regions = "Bahamas" ,fill=T , col= "grey", bg="light blue")

points(cbind(Bahamas$longitude,Bahamas$latitude),col="blue",pch=20,cex=1) 

#Looks like some islands are considerably better sampled than others.


## ------------------------------------------------------------------------

Xanthium_strumarium_range <- BIEN_ranges_load_species(species = "Xanthium strumarium")


## ------------------------------------------------------------------------
#First, let's add a base map so that our range has some context:

map('world',fill=T , col= "grey", bg="light blue",xlim = c(-180,-20),ylim = c(-60,80))

#Now, we can add the range map:
plot(Xanthium_strumarium_range,col="green",add=T)




## ------------------------------------------------------------------------
map('world',fill=T , col= "grey", bg="light blue",xlim = c(-180,-20),ylim = c(-60,80))
plot(Xanthium_strumarium_range,col="green",add=T)
points(cbind(Xanthium_strumarium$longitude,Xanthium_strumarium$latitude),col="blue",pch=20,cex=1)



## ------------------------------------------------------------------------
LUQUILLO <- BIEN_plot_name(plot.name = "LUQUILLO")
head(LUQUILLO)

## ------------------------------------------------------------------------
LUQUILLO_full <- BIEN_plot_name(plot.name = "LUQUILLO",cultivated = T,all.taxonomy = T,native.status = T,political.boundaries = T,all.metadata = T)


## ------------------------------------------------------------------------

Salix_traits<-BIEN_trait_genus(genus = "Salix")


## ------------------------------------------------------------------------

BIEN_trait_list()


## ------------------------------------------------------------------------

leaf_area <- BIEN_trait_trait(trait = "leaf area")


## ------------------------------------------------------------------------

Asclepias_taxonomy<-BIEN_taxonomy_genus(genus = "Asclepias")

#We see that the genus Asclepias falls within the family Apocynaceae and the order Gentianales.

#You'll also notice that a given species may appear more than once (due to multiple circumscriptions, some of which may be illegitimate).

#If we'd just like to know all the speciess that aren't illegitimate:
Asclepias_species<-unique(Asclepias_taxonomy$scrubbed_species_binomial[Asclepias_taxonomy$scrubbed_taxonomic_status %in% c("accepted",  "no opinion")])



## ------------------------------------------------------------------------

phylo <- BIEN_phylogeny_conservative()

#Let's make sure it looks alright

plot.phylo(x = phylo, show.tip.label =  FALSE)

#If we just want to see which species are included

phylo_species <- phylo$tip.label



## ------------------------------------------------------------------------

Cupressus_arizonica_stems<-BIEN_stem_species("Cupressus arizonica")


## ------------------------------------------------------------------------
Bahamas_species_list<-BIEN_list_country(country = "Bahamas")

#Notice that this time, we have 998 species, whereas previously we saw that there were 999 unique species.  What happened?  The list functions ignore NA values for species names, but R does not.  R counted NA values as a unique species name, giving one extra unique value.


## ------------------------------------------------------------------------

country_vector<-c("Haiti","Dominican Republic")
Haiti_DR <- BIEN_list_country(country = country_vector)


## ------------------------------------------------------------------------

BIEN_metadata_database_version()


