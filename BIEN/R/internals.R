#Internal functions

######################

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
taxonomy_check<-function(all.taxonomy){
  if(!all.taxonomy){
    select<-""
  }else{
    select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
  
}  

##


native_check<-function(native.status){
  if(!native.status){
    select<-""
  }else{
    select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   

##
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

political_check<-function(political.boundaries){
  if(!political.boundaries){
    select<-""
  }else{
    select<-"country,state_province,county,locality,"
  }
  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   

##

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
collection_check<-function(collection.info){
  if(!collection.info){
    select<-""
  }else{
    select<-"catalog_number, recorded_by, record_number, date_collected, identified_by, date_identified, identification_remarks  "
  }  
  
  output<-as.data.frame(cbind(select),stringsAsFactors = F)  
  colnames(output)<-c("select")
  
  return(output)  
  
}   