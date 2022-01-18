#Internal functions

######################

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.cultivated_check<-function(cultivated){
  if(!cultivated){
    query<-"AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
    select<-""
  }else{
    query<-""
    select<-",is_cultivated_observation,is_cultivated_in_region, is_location_cultivated"
  }
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}


##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.newworld_check<-function(new.world){
  
  if(is.null(new.world)){
    query<-""
    select<-""
    output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
    colnames(output)<-c("query","select")
    return(output)  
    
  }
  
  if(!new.world){
    query<-"AND is_new_world = 0 "    
    select<-", is_new_world"}
  
  if(new.world){
    query<-"AND is_new_world = 1 "    
    select<-", is_new_world"}
  
  
  
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}


##

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.geovalid_check<-function(only.geovalid){
  if(!only.geovalid){
    query<-""
    select<-",is_geovalid"
  }else{
    query<-"AND is_geovalid = 1"
    select<-""
  }
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}


##


#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.taxonomy_check<-function(all.taxonomy){
  if(!all.taxonomy){
    select<-""
  }else{
    select<-", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
  
}  

##

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.native_check<-function(native.status){
  if(!native.status){
    select<-""
  }else{
    select<-",native_status, native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.observation_check<-function(observation.type){
  if(!observation.type){
    query<-"AND observation_type IN ('plot','specimen','literature','checklist')"
    select<-""
  }else{
    query<-""
    select<-",observation_type"
  }
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}   

##

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.political_check<-function(political.boundaries){
  if(!political.boundaries){
    select<-""
  }else{
    select<-", country,state_province,county,locality,elevation_m"
  }
  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   

##

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.natives_check<-function(natives.only){
  if(!natives.only){
    query<-""
  }else{
    query<-"AND (is_introduced=0 OR is_introduced IS NULL) "
  }  
  
  output<-as.data.frame(cbind(query),stringsAsFactors = F)  
  colnames(output)<-c("query")
  
  return(output)  
  
}   

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.collection_check<-function(collection.info){
  if(!collection.info){
    select<-""
  }else{
    select<-",catalog_number, recorded_by, record_number, date_collected, identified_by, date_identified, identification_remarks  "
  }  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
} 


##########################
#Traits

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.political_check_traits<-function(political.boundaries){
if(political.boundaries){
  select <- "region, country, state_province, locality_description"  
}else{
  select <- ""  
  
}

  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   
  
##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.taxonomy_check_traits<-function(all.taxonomy){
  
if(all.taxonomy){
  select <- "verbatim_family, verbatim_scientific_name, name_submitted, family_matched, name_matched, name_matched_author, 
    higher_plant_group, tnrs_warning, matched_taxonomic_status, scrubbed_taxonomic_status, scrubbed_family, scrubbed_genus, 
    scrubbed_specific_epithet, scrubbed_taxon_name_no_author, scrubbed_taxon_canonical, 
    scrubbed_author, scrubbed_taxon_name_with_author, scrubbed_species_binomial_with_morphospecies"
  
}else{
  select <- ""  
  
}

  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
} 

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.source_check_traits<-function(source.citation){
  
  if(source.citation){
    select <- ",source_citation"
    
  }else{
    select <- ""  
    
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
} 




#####################################
#Plots

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.cultivated_check_plot<-function(cultivated){
  if(!cultivated){
    query<-"AND (view_full_occurrence_individual.is_cultivated_observation = 0 OR view_full_occurrence_individual.is_cultivated_observation IS NULL) AND view_full_occurrence_individual.is_location_cultivated IS NULL"
    select<-""
  }else{
    query<-""
    select<-",view_full_occurrence_individual.is_cultivated_observation,view_full_occurrence_individual.is_cultivated_in_region,view_full_occurrence_individual.is_location_cultivated"
  }
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}

##



##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.newworld_check_plot<-function(new.world){
  
  if(is.null(new.world)){
    query<-""
    select<-""
    output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
    colnames(output)<-c("query","select")
    return(output)  
    
  }
  
  if(!new.world){
    query<-"AND view_full_occurrence_individual.is_new_world = 0 "    
    select<-",view_full_occurrence_individual.is_new_world"}
  
  if(new.world){
    query<-"AND view_full_occurrence_individual.is_new_world = 1 "    
    select<-",view_full_occurrence_individual.is_new_world"}
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}



##  

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.taxonomy_check_plot<-function(all.taxonomy){
  
  if(!all.taxonomy){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.verbatim_family,view_full_occurrence_individual.verbatim_scientific_name,view_full_occurrence_individual.family_matched,view_full_occurrence_individual.name_matched,view_full_occurrence_individual.name_matched_author,view_full_occurrence_individual.higher_plant_group,view_full_occurrence_individual.scrubbed_taxonomic_status,view_full_occurrence_individual.scrubbed_family,view_full_occurrence_individual.scrubbed_author"
  }
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}

##

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.native_check_plot<-function(native.status){
  
  if(!native.status){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.native_status,view_full_occurrence_individual.native_status_reason,view_full_occurrence_individual.native_status_sources,view_full_occurrence_individual.is_introduced,view_full_occurrence_individual.native_status_country,view_full_occurrence_individual.native_status_state_province,view_full_occurrence_individual.native_status_county_parish"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}

##  

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.natives_check_plot<-function(natives.only){  
  
  if(!natives.only){
    query<-""
  }else{
    query<-"AND (view_full_occurrence_individual.is_introduced=0 OR view_full_occurrence_individual.is_introduced IS NULL) "
  }  
  
  output<-as.data.frame(cbind(query),stringsAsFactors = F)  
  colnames(output)<-c("query")
  
  return(output)  
  
}  


##  


##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.political_check_plot<-function(political.boundaries){  
  
  if(!political.boundaries){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}  

##

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.collection_check_plot<-function(collection.info){    
  
  if(!collection.info){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.catalog_number, view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.record_number, view_full_occurrence_individual.date_collected, view_full_occurrence_individual.identified_by, view_full_occurrence_individual.date_identified, view_full_occurrence_individual.identification_remarks  "
  }  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}  


##
##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.md_check_plot<-function(all.metadata){    
  
  if(!all.metadata){
    select<-""
  }else{
    select<-",plot_metadata.coord_uncertainty_m,plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
  } 

#

#'Set query details
#'
#'Helper function to set query components.
#' @param species Single species or vector of species.
#' @keywords internal
.species_check<-function(species){    
  
  if(is.null(species)){
    query<-""
  }else{
    query<-species_select<-paste(" AND", "scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ")  
  }
  
  output<-as.data.frame(cbind(query),stringsAsFactors = F)  
  colnames(output)<-c("query")
  
  return(output)  
  
} 

#

#'Set query details
#'
#'Helper function to set query components.
#' @param genus Single genus or vector of genera.
#' @keywords internal
.genus_check<-function(genus){    
  
  if(is.null(genus)){
    query<-""
  }else{
    query<-species_select<-paste(" AND", "scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ")  
  }
  
  output<-as.data.frame(cbind(query),stringsAsFactors = F)  
  colnames(output)<-c("query")
  
  return(output)  
  
} 


########################################
#Stem


#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.cultivated_check_stem<-function(cultivated){    
  
  if(!cultivated){
    query<-"AND (analytical_stem.is_cultivated_observation = 0 OR analytical_stem.is_cultivated_observation IS NULL) AND analytical_stem.is_location_cultivated IS NULL"
    select<-""
  }else{
    query<-""
    select<-",analytical_stem.is_cultivated_observation,view_full_occurrence_individual.is_cultivated_in_region,analytical_stem.is_location_cultivated"
  }
  
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}   

#  

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.newworld_check_stem<-function(new.world){
  
  if(is.null(new.world)){
    query<-""
    select<-""
    output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
    colnames(output)<-c("query","select")
    return(output)  
    
  }
  
  
  if(!new.world){
    query<-"AND analytical_stem.is_new_world = 0 "    
    select<-",analytical_stem.is_new_world"}
  
  if(new.world){
    query<-"AND analytical_stem.is_new_world = 1 "    
    select<-",analytical_stem.is_new_world"}
  
  output<-as.data.frame(cbind(query,select),stringsAsFactors = F)  
  colnames(output)<-c("query","select")
  
  return(output)  
  
}





#

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.taxonomy_check_stem<-function(all.taxonomy){    
  
  if(!all.taxonomy){
    select<-""
  }else{
    select<-",analytical_stem.verbatim_family,analytical_stem.verbatim_scientific_name,analytical_stem.family_matched,analytical_stem.name_matched,analytical_stem.name_matched_author,analytical_stem.higher_plant_group,analytical_stem.scrubbed_taxonomic_status,analytical_stem.scrubbed_family,analytical_stem.scrubbed_author"
  }
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
}

#  

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.native_check_stem<-function(native.status){    
  if(!native.status){
    select<-""
  }else{
    select<-",analytical_stem.native_status,analytical_stem.native_status_reason,analytical_stem.native_status_sources,analytical_stem.is_introduced,analytical_stem.native_status_country,analytical_stem.native_status_state_province,analytical_stem.native_status_county_parish"
  }
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
}


#


#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.natives_check_stem<-function(natives.only){    
  
  if(!natives.only){
    query<-""
  }else{
    query<-"AND (view_full_occurrence_individual.is_introduced=0 OR view_full_occurrence_individual.is_introduced IS NULL)"
  }  
  
  output<-as.data.frame(cbind(query),stringsAsFactors = F)  
  colnames(output)<-c("query")
  
  return(output)  
}

#  

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.political_check_stem<-function(political.boundaries){    
  
  if(!political.boundaries){
    select<-""
  }else{
    select<-",analytical_stem.country,analytical_stem.state_province,analytical_stem.county,analytical_stem.locality"
  }
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
}

#


#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.collection_check_stem<-function(collection.info){   
  
  if(!collection.info){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.catalog_number, view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.record_number, view_full_occurrence_individual.date_collected, view_full_occurrence_individual.identified_by, view_full_occurrence_individual.date_identified, view_full_occurrence_individual.identification_remarks  "
  }  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
}

#

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.vfoi_check_stem<-function(native.status,cultivated,natives.only,collection.info){ 
  
  if(native.status | cultivated |natives.only | collection.info){
    join<-" JOIN view_full_occurrence_individual ON (analytical_stem.taxonobservation_id  = view_full_occurrence_individual.taxonobservation_id)"}else{
      join<-""  
    }
  
  output<-as.data.frame(cbind(join),stringsAsFactors = F)  
  colnames(output)<-c("join")
  
  return(output)  
}

#  

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
.md_check_stem<-function(all.metadata){   
  
  if(!all.metadata){
    select<-""
  }else{
    select<-",plot_metadata.coord_uncertainty_m,plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  }

#########################################
#Value checkers
#########################################
##################################
#'Check that value is logical
#'
#'Helper function to check data format.
#' @keywords internal
#' @examples \dontrun{
#' is_log(TRUE)}
.is_log <- function(x) {
  if (!inherits(x, 'logical')) {
    stop(sys.call()[-1], " should be logical", call. = FALSE)
  }
}


##################################
#'Check that value is logical or null
#'
#'Helper function to check data format.
#' @keywords internal
#' @examples \dontrun{
#' is_log_or_null(new.world)}
.is_log_or_null <- function(x) {
  if (!inherits(x, c('logical','NULL'))) {
    stop(sys.call()[-1], " should be logical or NULL", call. = FALSE)
  }
}

###################################
#'Check that value is character
#'
#'Helper function to check data format.
#' @keywords internal
#' @examples \dontrun{
#' is_char(species)}
.is_char <- function(x) {
  if (!inherits(x ,c("character","NULL"))) {
    stop(sys.call()[-1]," should be character", call. = FALSE)
  }
}

###################################

#'Check that value is numeric
#'
#'Helper function to check data format.
#' @keywords internal
#' @examples \dontrun{
#' is_num(min.lat)}
.is_num <- function(x) {
  if (!inherits(x ,'numeric')) {
    stop(sys.call()[-1]," should be numeric", call. = FALSE)
  }
}

#################################
#################################
