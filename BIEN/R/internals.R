#Internal functions

######################

#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
cultivated_check<-function(cultivated){
  if(!cultivated){
    query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    select<-""
  }else{
    query<-""
    select<-",is_cultivated,is_cultivated_in_region"
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
newworld_check<-function(only.new.world){
  if(!only.new.world){
    query<-""
    select<-",is_new_world"
  }else{
    query<-"AND is_new_world = 1 "
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
taxonomy_check<-function(all.taxonomy){
  if(!all.taxonomy){
    select<-""
  }else{
    select<-", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author"
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
native_check<-function(native.status){
  if(!native.status){
    select<-""
  }else{
    select<-",native_status, native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish"
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
observation_check<-function(observation.type){
  if(!observation.type){
    select<-""
  }else{
    select<-",observation_type"
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
political_check<-function(political.boundaries){
  if(!political.boundaries){
    select<-""
  }else{
    select<-", country,state_province,county,locality"
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
natives_check<-function(natives.only){
  if(!natives.only){
    query<-""
  }else{
    query<-"AND ( native_status IS NULL OR native_status NOT IN ( 'I', 'Ie' ) )"
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
collection_check<-function(collection.info){
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
political_check_traits<-function(political.boundaries){
if(political.boundaries){
  select <- "region, country, stateprovince, lower_political, locality_description"  
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
taxonomy_check_traits<-function(all.taxonomy){
  
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
#####################################
#Plots

##
#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
cultivated_check_plot<-function(cultivated){
  if(!cultivated){
    query<-"AND (view_full_occurrence_individual.is_cultivated = 0 OR view_full_occurrence_individual.is_cultivated IS NULL)"
    select<-""
  }else{
    query<-""
    select<-",view_full_occurrence_individual.is_cultivated,view_full_occurrence_individual.is_cultivated_in_region"
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
newworld_check_plot<-function(only.new.world){
  
  if(!only.new.world){
    query<-""
    select<-",view_full_occurrence_individual.is_new_world"
  }else{
    query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    select<-""
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
taxonomy_check_plot<-function(all.taxonomy){
  
  if(!all.taxonomy){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.verbatim_family,view_full_occurrence_individual.verbatim_scientific_name,view_full_occurrence_individual.family_matched,view_full_occurrence_individual.name_matched,view_full_occurrence_individual.name_matched_author,view_full_occurrence_individual.higher_plant_group,view_full_occurrence_individual.taxonomic_status,view_full_occurrence_individual.scrubbed_family,view_full_occurrence_individual.scrubbed_author"
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
native_check_plot<-function(native.status){
  
  if(!native.status){
    select<-""
  }else{
    select<-",view_full_occurrence_individual.native_status,view_full_occurrence_individual.native_status_reason,view_full_occurrence_individual.native_status_sources,view_full_occurrence_individual.isintroduced,view_full_occurrence_individual.native_status_country,view_full_occurrence_individual.native_status_state_province,view_full_occurrence_individual.native_status_county_parish"
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
natives_check_plot<-function(natives.only){  
  
  if(!natives.only){
    query<-""
  }else{
    query<-"AND ( view_full_occurrence_individual.native_status IS NULL OR view_full_occurrence_individual.native_status NOT IN ( 'I', 'Ie' ) )"
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
political_check_plot<-function(political.boundaries){  
  
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
collection_check_plot<-function(collection.info){    
  
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
md_check_plot<-function(all.metadata){    
  
  if(!all.metadata){
    select<-""
  }else{
    select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
  } 

########################################
#Stem


#'Set query details
#'
#'Helper function to set query components.
#' @keywords internal
cultivated_check_stem<-function(cultivated){    
  
  if(!cultivated){
    query<-"AND (analytical_stem.is_cultivated = 0 OR analytical_stem.is_cultivated IS NULL)"
    select<-""
  }else{
    query<-""
    select<-",analytical_stem.is_cultivated,view_full_occurrence_individual.is_cultivated_in_region"
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
newworld_check_stem<-function(only.new.world){    
  
  if(!only.new.world){
    query<-""
    select<-",analytical_stem.is_new_world"
  }else{
    query<-"AND analytical_stem.is_new_world = 1 "
    select<-""
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
taxonomy_check_stem<-function(all.taxonomy){    
  
  if(!all.taxonomy){
    select<-""
  }else{
    select<-",analytical_stem.verbatim_family,analytical_stem.verbatim_scientific_name,analytical_stem.family_matched,analytical_stem.name_matched,analytical_stem.name_matched_author,analytical_stem.higher_plant_group,analytical_stem.taxonomic_status,analytical_stem.scrubbed_family,analytical_stem.scrubbed_author"
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
native_check_stem<-function(native.status){    
  if(!native.status){
    select<-""
  }else{
    select<-",native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish"
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
natives_check_stem<-function(natives.only){    
  
  if(!natives.only){
    query<-""
  }else{
    query<-"AND ( view_full_occurrence_individual.native_status IS NULL OR view_full_occurrence_individual.native_status NOT IN ( 'I', 'Ie' ) )"
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
political_check_stem<-function(political.boundaries){    
  
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
collection_check_stem<-function(collection.info){   
  
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
vfoi_check_stem<-function(native.status,cultivated,natives.only,collection.info){ 
  
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
md_check_stem<-function(all.metadata){   
  
  if(!all.metadata){
    select<-""
  }else{
    select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
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
#' is_log(only.new.world)}
is_log <- function(x) {
  if (!inherits(x, 'logical')) {
    stop(sys.call()[-1], " should be logical", call. = FALSE)
  }
}

###################################
#'Check that value is character
#'
#'Helper function to check data format.
#' @keywords internal
#' @examples \dontrun{
#' is_char(species)}
is_char <- function(x) {
  if (!inherits(x ,'character')) {
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
is_num <- function(x) {
  if (!inherits(x ,'numeric')) {
    stop(sys.call()[-1]," should be numeric", call. = FALSE)
  }
}


