#Tests for BIEN package

library(testthat)
library(BIEN)

#List tests

test_that("List functions return a dataframe",{
expect_that(BIEN_list_all(),is_a("data.frame"))
expect_that(BIEN_list_country("Bahamas"),is_a("data.frame"))
expect_that(BIEN_list_county(country = "United States",state = "Michigan",county = "Kent",cultivated = T,only.new.world = F),is_a("data.frame"))
expect_that(BIEN_list_state(country = "United States",state = "Rhode Island"),is_a("data.frame"))
})

#Metadata tests
test_that("Metadata functions return a dataframe",{
expect_that(BIEN_metadata_database_version(),is_a("data.frame"))
})
test_that("Metadata_citation function returns a list",{
  expect_that(BIEN_metadata_citation(),is_a("list"))
})



#Occurrence tests
test_that("Occurrence functions return a dataframe",{
  expect_that(BIEN_occurrence_box(min.lat = 32.9,max.lat = 33,min.long = -114,max.long = -113.9, cultivated = TRUE, only.new.world = FALSE,all.taxonomy = T,native.status = T,natives.only = T,observation.type = T,political.boundaries = T),is_a("data.frame"))
  expect_that(BIEN_occurrence_country("Bahamas") ,is_a("data.frame"))
  expect_that(BIEN_occurrence_county(country = "United States",state = "Arizona",county = "Pima") ,is_a("data.frame"))
  expect_that(BIEN_occurrence_family(family = "Cactaceae",limit=1) ,is_a("data.frame"))
  expect_that(BIEN_occurrence_genus(genus = "Xanthium") ,is_a("data.frame"))
  expect_that(BIEN_occurrence_species(species = "Xanthium strumarium",cultivated = T,only.new.world = F,all.taxonomy = T,native.status = T,observation.type = T,political.boundaries = T,limit=1) ,is_a("data.frame"))
  expect_that(BIEN_occurrence_state(country = "United States",state = "Rhode Island") ,is_a("data.frame"))
  #expect_that(BIEN_occurrence_shapefile( BIEN_ranges_load_species(species = "Aa argyrolepis")),is_a("data.frame"))
  #expect_that(BIEN_occurrence_records_per_species() ,is_a("data.frame"))
  })



#Phylogeny
test_that("Phylogeny functions return a phylogeny",{
  expect_that(BIEN_phylogeny_complete(n_phylogenies = 2) ,is_a("multiPhylo"))
  expect_that(BIEN_phylogeny_conservative() ,is_a("phylo"))
})

#Plot
test_that("Plot functions return a dataframe",{
  expect_that(BIEN_plot_country("test",cultivated = T,only.new.world = F,all.taxonomy = T,native.status = T,political.boundaries = T,all.metadata = T) ,is_a("data.frame"))
  expect_that(BIEN_plot_dataset(dataset = "test") ,is_a("data.frame"))
  expect_that(BIEN_plot_datasource("test") ,is_a("data.frame"))
  expect_that(BIEN_plot_list_datasource() ,is_a("data.frame"))
  expect_that(BIEN_plot_list_sampling_protocols(),is_a("data.frame"))
  expect_that(BIEN_plot_metadata() ,is_a("data.frame"))
  expect_that(BIEN_plot_name("test"),is_a("data.frame"))
  expect_that(BIEN_plot_sampling_protocol(sampling_protocol = "test",natives.only = T),is_a("data.frame"))
  expect_that(BIEN_plot_state(country = "United States",state = "test"),is_a("data.frame"))
  })

#Ranges
test_that("Ranges functions return a SpatialPolygonsDataFrame",{
  expect_that(BIEN_ranges_load_species(species = "Abies amabilis") ,is_a("SpatialPolygonsDataFrame"))
})

#Stem
test_that("Stem functions return a dataframe",{
  expect_that(BIEN_stem_species(species = "Abies amabilis",natives.only = T,native.status = T) ,is_a("data.frame"))
  expect_that(BIEN_stem_genus(genus = "Abies",natives.only = T,native.status = T) ,is_a("data.frame"))
  expect_that(BIEN_stem_family(family = "Selaginellaceae",natives.only = T,native.status = T) ,is_a("data.frame"))
  
})

#Taxonomy
test_that("Taxonomy functions return a dataframe",{
  expect_that(BIEN_taxonomy_family("Cactaceae") ,is_a("data.frame"))
  expect_that(BIEN_taxonomy_genus("Xanthium") ,is_a("data.frame"))
  expect_that(BIEN_taxonomy_species("Xanthium strumarium") ,is_a("data.frame"))
  })

#trait
test_that("Trait functions return a dataframe",{
  expect_that(BIEN_trait_family("Cactaceae") ,is_a("data.frame"))
  expect_that(BIEN_trait_genus("Xanthium") ,is_a("data.frame"))
  expect_that(BIEN_trait_list() ,is_a("data.frame"))
  expect_that(BIEN_trait_mean(species = "Xanthium strumarium",trait = "leaf dry mass per leaf fresh mass") ,is_a("data.frame"))
  expect_that(BIEN_trait_species("Xanthium strumrium",all.taxonomy = T,political.boundaries = T) ,is_a("data.frame"))
  expect_that(BIEN_trait_trait("leaf dry mass per leaf fresh mass") ,is_a("data.frame"))
  expect_that(BIEN_trait_traitbyfamily(trait = "leaf dry mass per leaf fresh mass",family = "Asteraceae") ,is_a("data.frame"))
  expect_that(BIEN_trait_traitbygenus(trait = "leaf dry mass per leaf fresh mass",genus = "Xanthium") ,is_a("data.frame"))
  expect_that(BIEN_trait_traitbyspecies(trait = "leaf dry mass per leaf fresh mass",species = "Xanthium strumarium") ,is_a("data.frame"))
  expect_that(BIEN_trait_traits_per_species() ,is_a("data.frame"))
})



