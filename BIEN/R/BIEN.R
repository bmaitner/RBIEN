#' BIEN: Tools for accessing the BIEN database.
#'
#' @description The Botanical Information and Ecology Network(BIEN) R package provides access to the BIEN database as well as useful tools for working with the BIEN data.
#' 
#' @section Getting started:
#' Type vignette("BIEN") to view the vignette, which contains useful information on the BIEN package.
#' 
#' @references Enquist, B.J., Sandel, B., Boyle, B., Donoghue II, J.C., Regetz, J., Svenning, J.C., McGill, B.J., Peet, R.K., Jorgensen, P.M., Condit, R., Thiers, B., Schildhauer, M., Smith, S.A., Hinchliff, C.E., Wiser, S.K., Violle, C., Simova, I., Spencer, N., Dolins, S., Morueta-Holme, N., Marcuse-Kubitza, A., Kraft, N.J.B., Ott, J.E., Andelman, S., ter Steege, H., Phillips, O., Sloat, L.L., Narro, M.L., Casler, N., Guaderama, D.,  Merow, C., Maitner, B.S. (in prep) A general signature of taxonomic and phylogenetic diversity across the Land Plants of the New World.
#' 
#' @docType package
#' @name BIEN
#' @aliases BIEN-package
NULL

###################

.onAttach <- function(libname,pkgname) {
  packageStartupMessage('Type vignette("BIEN") to get started')
}



#####################

#'Extract occurrence data for specified species from BIEN
#'
#'BIEN_occurrence_species downloads occurrence records for specific species from the BIEN database.
#' @param species A single species, or a vector of species.  Genus and species should be separated by a space. Genus should be capitalized.
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified species.
#' @examples \dontrun{
#' BIEN_occurrence_species("Abies amabilis")
#' species_vector<-c("Abies amabilis", "Acer nigrum")
#' BIEN_occurrence_species(species_vector)
#' BIEN_occurrence_species(species_vector,all.taxonomy=TRUE)}
#' @family occurrence functions
BIEN_occurrence_species<-function(species,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,observation.type=FALSE,political.boundaries=FALSE,...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(species)
  is_log(print.query)
  is_log(native.status)
  is_log(observation.type)
  is_log(political.boundaries)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",is_new_world"
  }else{
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial,",taxon_select,native_select,political_select," latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select),"FROM view_full_occurrence_individual WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}
##############

#'Extract occurrence data for specified shapefile
#'
#'BIEN_occurrence_shapefile downloads occurrence records falling within a user-specified shapefile.
#' @param shapefile An object of class SpatialPolygonsDataFrame.  Note that the polygon must be in WGS84.
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified species.
#' @examples \dontrun{
#' BIEN_ranges_species("Carnegiea gigantea")#saves ranges to the current working directory
#' shape<-readOGR(dsn = ".",layer = "Carnegiea_gigantea")
#' #shapefiles should be read with readOGR(), see note.
#' species_occurrenes<-BIEN_occurrences_shapefile(shapefile=shape)}
#' @family occurrence functions
BIEN_occurrence_shapefile<-function(shapefile,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,observation.type=FALSE,political.boundaries=FALSE,...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_log(print.query)
  is_log(native.status)
  is_log(observation.type)
  is_log(political.boundaries)
  
  wkt<-rgeos::writeWKT(shapefile)
  long_min<-shapefile@bbox[1,1]
  long_max<-shapefile@bbox[1,2]
  lat_min<-shapefile@bbox[2,1]
  lat_max<-shapefile@bbox[2,2]
  
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",is_new_world"
  }else{
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial,",taxon_select,native_select,political_select," latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select),"FROM 
                 (SELECT * FROM view_full_occurrence_individual WHERE higher_plant_group IS NOT NULL AND is_geovalid =1 AND latitude BETWEEN ",lat_min," AND ",lat_max,"AND longitude BETWEEN ",long_min," AND ",long_max,") a 
                 WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),a.geom)",cultivated_query,newworld_query, "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  #query = rangeQuery
  #print(query)
  # create query to retrieve
  df <- BIEN_sql(query)
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  if(length(df)==0){
    message("No occurrences found")
  }else{
    return(df)
    
  }
  
}

###############################

#'Extract species list by country
#'
#'BIEN_list_country downloads a list of all species within a country or countries from the BIEN database.
#' @param country A single country or a vector of countries.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing species list(s) for the specified country or countries.
#' @examples \dontrun{
#' BIEN_list_country("Canada")
#' country_vector<-c("Canada","United States")
#' BIEN_list_country(country_vector)}
#' @family list functions
BIEN_list_country<-function(country,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,...){
  is_log(cultivated)
  is_log(only.new.world)
  is_char(country)
  is_log(print.query)

  #set base query components
  sql_select <-  paste("SELECT DISTINCT country, scrubbed_species_binomial ")
  sql_from <- paste(" FROM species_by_political_division ")
  sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")
  sql_order_by <- paste(" ORDER BY scrubbed_species_binomial ")

 # adjust for optional parameters
  if(!cultivated){
    sql_where <- paste(sql_where, " AND (is_cultivated = 0 OR is_cultivated IS NULL) ")
  }else{
    sql_select  <- paste(sql_select, ",is_cultivated,is_cultivated_in_region")
  }

  if(!only.new.world){
    sql_select <- paste(sql_select,",is_new_world")
  }else{
    sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  }

  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  #print(query)

  # execute the query
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

############################

#'Extract a species list by state/province
#'
#'BIEN_list_state produces a list of all species with geovalidated occurrences falling within specified state(s) or province(s).
#' @param state A single state/province, or a vector of states/provinces.
#' @param country A single country or a vector of countries equal in length to the vector of states/provinces.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing species list(s) for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_list_state("United States","Michigan")
#' state_vector<-c("Michigan","Arizona")
#' BIEN_list_state(country="United States", state= state_vector)}
#' @family list functions
BIEN_list_state<-function(country,state,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,...){
  is_char(country)
  is_char(state)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  
  # set base query components
  sql_select <-  paste("SELECT DISTINCT country, state_province, scrubbed_species_binomial ")
  sql_from <- paste(" FROM species_by_political_division ")
  
  
  if(length(country)==1){
    sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                       AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                       AND scrubbed_species_binomial IS NOT NULL")
  }else{
    
    if(length(country)==length(state)){
      
      sql_where<-"WHERE ("
      
      for(i in 1:length(country)){
        
        condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), ")")
        if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
        sql_where<-paste(sql_where,condition_i)
        
      }#for i  
      
      sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
      
    }else{
      stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
      
    }  
    
    
    
  }#if length(country>1)
  
  
  
  sql_order_by <- paste(" ORDER BY scrubbed_species_binomial ")
  
  # adjust for optional parameters
  if(!cultivated){
    sql_where <- paste(sql_where, " AND (is_cultivated = 0 OR is_cultivated IS NULL) ")
  }else{
    sql_select  <- paste(sql_select, ",is_cultivated,is_cultivated_in_region")
  }
  
  if(!only.new.world){
    sql_select <- paste(sql_select,",is_new_world")
  }else{
    sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  }
  
  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}

###########################

#'Extract a species list by county.
#'
#'BIEN_list_county produces a list of all species with geovalidated occurrences falling within specified county or counties.
#' @param country A single country or vector of countries
#' @param state A single state or vector of states.
#' @param county A single county or vector of counties.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing species list(s) for the specified states/provinces.
#' @note This function requires you supply either 1) a single state and country with one or more counties, or 2) vectors of equal length for each political level.
#' @examples \dontrun{
#' BIEN_list_county("United States", "Michigan", "Kent")
#' BIEN_list_county(country = "United States", state = "Michigan", county = "Kent")
#' county_vector<-c("Kent","Kalamazoo")
#' BIEN_list_county(country = "United States", state = "Michigan", county = county_vector)}
#' @family list functions
BIEN_list_county<-function(country,state,county,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE, ...){
  is_char(country)
  is_char(state)
  is_char(county)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  
  # set base query components
  sql_select <-  paste("SELECT DISTINCT country, state_province, county,  scrubbed_species_binomial ")
  sql_from <- paste(" FROM species_by_political_division ")
  
  #sql where
  if(length(country)==1 & length(state)==1){
    sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                       AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                       AND county in (", paste(shQuote(county, type = "sh"),collapse = ', '), ")
                       AND scrubbed_species_binomial IS NOT NULL")
  }else{
    
    if(length(country)==length(state) & length(country)==length(county)){
      
      sql_where<-"WHERE ("
      
      for(i in 1:length(country)){
        
        condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " 
                            AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), "
                            AND county = ", paste(shQuote(county[i], type = "sh"),collapse = ', '), ")
                            ")
        
        if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
        sql_where<-paste(sql_where,condition_i)
        
      }#for i  
      
      sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
      
    }else{
      stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")  
      
    }  
    
    
    
  }#if length(country>1)
  
  sql_order_by <- paste(" ORDER BY scrubbed_species_binomial ")
  
  # adjust for optional parameters
  if(!cultivated){
    sql_where <- paste(sql_where, " AND (is_cultivated = 0 OR is_cultivated IS NULL) ")
  }else{
    sql_select  <- paste(sql_select, ",is_cultivated,is_cultivated_in_region")
  }
  
  if(!only.new.world){
    sql_select <- paste(sql_select,",is_new_world")
  }else{
    sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  }
  
  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}


###########################

#'Extract a list of all species in the BIEN database.
#'
#'BIEN_list_all produces a list of all species in the BIEN database.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing a list of all species in the BIEN database.
#' @examples \dontrun{
#' species_list<-BIEN_list_all()}
#' @family list functions
BIEN_list_all<-function(print.query=FALSE, ...){
  is_log(print.query)

  query <- paste("SELECT species FROM bien_species_all ORDER BY species;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
###########################

#'Extract a list of species within a given shapefile.
#'
#'BIEN_list_shapefile produces a list of all species with occurrence record falling within a user-supplied GIS shapefile.
#' @param shapefile An object of class SpatialPolygonsDataFrame.  Note that the polygon must be in WGS84.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing a list of all species with occurrences in the supplied shapefile.
#' @note We recommend using the function readOGR() in the rgdal package to read in shapefiles.  Other methods may cause problems related to handling holes in polygons.
#' @examples \dontrun{
#' BIEN_ranges_species("Carnegiea gigantea")#saves ranges to the current working directory
#' shape<-readOGR(dsn = ".",layer = "Carnegiea_gigantea")
#' #shapefiles should be read with readOGR(), see note.
#' species_list<-BIEN_list_shapefile(shapefile=shape)}
#' @family list functions
BIEN_list_shapefile<-function(shapefile,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  
  wkt<-rgeos::writeWKT(shapefile)
  long_min<-shapefile@bbox[1,1]
  long_max<-shapefile@bbox[1,2]
  lat_min<-shapefile@bbox[2,1]
  lat_max<-shapefile@bbox[2,2]
  
  # adjust for optional parameters
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",is_new_world"
  }else{
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }
  
  
  
  #rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
  query<-paste("SELECT DISTINCT scrubbed_species_binomial",cultivated_select,newworld_select ,"FROM  
                 (SELECT * FROM view_full_occurrence_individual WHERE higher_plant_group IS NOT NULL AND is_geovalid =1 AND latitude BETWEEN ",lat_min," AND ",lat_max,"AND longitude BETWEEN ",long_min," AND ",long_max,") a
                 WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),a.geom)",cultivated_query,newworld_query,";")
  
  #query = rangeQuery
  #print(query)
  # create query to retrieve
  df <- BIEN_sql(query)
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  if(length(df)==0){
    message("No species found")
  }else{
    return(df)
    
  }
  
}



###########################
###########################

#'Extract occurrence data from BIEN for specified genera
#'
#'BIEN_occurrence_genus downloads occurrence records for specific genus/genera from the BIEN database.
#' @param genus A single genus, or a vector of genera. Genera should be capitalized.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified genera.
#' @examples \dontrun{
#' BIEN_occurrence_genus("Abutilon")
#' genus_vector<-c("Abutilon","Abronia")
#' BIEN_occurrence_genus(genus_vector)
#' BIEN_occurrence_genus(genus = "Abutilon",cultivated = TRUE,only.new.world = FALSE)}
#' @family occurrence functions
BIEN_occurrence_genus<-function(genus,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,native.status=FALSE,print.query=FALSE,observation.type=FALSE,political.boundaries=FALSE, ...){
  is_char(genus)
  is_log(cultivated)
  is_log(all.taxonomy)
  is_log(only.new.world)
  is_log(native.status)
  is_log(print.query)
  is_log(observation.type)
  is_log(political.boundaries)
  
  #set conditions for query
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",is_new_world"
  }else{
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  # set the query
  query <-
    paste("SELECT scrubbed_genus, scrubbed_species_binomial,",taxon_select,native_select,political_select," latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select), "
          FROM view_full_occurrence_individual 
          WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}
############################

#'Extract species occurrences by family.
#'
#'BIEN_occurrence_family extracts all occurrences for a given family (or families) from the BIEN database.
#' @param family A single family or a vector of families.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified family/families.
#' @examples \dontrun{
#' BIEN_occurrence_family("Theaceae")
#' family_vector<-c("Theaceae","Ericaceae")
#' BIEN_occurrence_family(family_vector)}
#' @family occurrence functions
BIEN_occurrence_family<-function(family,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,observation.type=FALSE,all.taxonomy=FALSE,native.status=FALSE,political.boundaries=FALSE, ...){
  is_char(family)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(observation.type)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(political.boundaries)
  
  #set conditions for query
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(only.new.world){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }else{
    newworld_query<-""
    newworld_select<-",is_new_world"
  }
  
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  # set the query
  query <- paste("SELECT scrubbed_family,",taxon_select,native_select,political_select,"scrubbed_species_binomial, latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select),"
                 FROM view_full_occurrence_individual 
                 WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}


#######################

#'Extract species occurrence records by state.
#'
#'BIEN_occurrence_state extracts occurrences records for the specified state(s).
#' @param state A single state or a vector of states.
#' @param country A single country or vector of countries.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @note This function requires you supply either 1) a single country with one or more states, or 2) vectors of equal length for each political level.
#' @return Dataframe containing occurrence records for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_occurrence_state("United States","Rhode Island")
#' state_vector<-c("Rhode Island","Maryland")
#' BIEN_occurrence_state(country="United States",state=state_vector)}
#' @family occurrence functions
BIEN_occurrence_state<-function(country,state,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,all.taxonomy=FALSE, native.status=FALSE,observation.type=FALSE,political.boundaries=FALSE, ...){
  is_char(country)
  is_char(state)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(observation.type)
  is_log(political.boundaries)
  
  #set conditions for query
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(only.new.world){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }else{
    newworld_query<-""
    newworld_select<-",is_new_world"
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-"observation_type"
  }
  
  if(!political.boundaries){
    political_select<-"state_province,"
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  ##state where
  if(length(country)==1){
    sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                       AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                       AND scrubbed_species_binomial IS NOT NULL")
  }else{
    
    if(length(country)==length(state)){
      
      sql_where<-"WHERE ("
      
      for(i in 1:length(country)){
        
        condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), ")")
        if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
        sql_where<-paste(sql_where,condition_i)
        
      }#for i  
      
      sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
      
    }else{
      stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
      
    }  
    
    
    
  }#if length(country>1)
  
  
  
  
  # set the query
  query <- paste("SELECT ",political_select," scrubbed_species_binomial," ,taxon_select , "latitude, longitude,date_collected,datasource,
                 dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,native_select,
                                                                                      observation_select),"
                 FROM view_full_occurrence_individual ",
                 sql_where,cultivated_query,newworld_query," 
                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) 
                 ORDER BY scrubbed_species_binomial;")
  
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}

#############################

#'Extract species occurrence records by country.
#'
#'BIEN_occurrence_country extracts occurrences records for the specified country/countries.
#' @param country A single country or a vector of country.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified country.
#' @examples \dontrun{
#' library(RPostgreSQL)
#' BIEN_occurrence_country("Cuba")
#' country_vector<-c("Cuba","Bahamas")
#' BIEN_occurrence_country(country_vector)}
#' @family occurrence functions
BIEN_occurrence_country<-function(country,cultivated=FALSE,only.new.world=TRUE,print.query=FALSE,all.taxonomy=FALSE,native.status=FALSE,observation.type=FALSE,political.boundaries=FALSE, ...){
  is_char(country)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(observation.type)
  is_log(political.boundaries)
  
  #set conditions for query
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(only.new.world){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }else{
    newworld_query<-""
    newworld_select<-",is_new_world"
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-"country,"
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  # set the query
  query <- paste("SELECT ",political_select ," scrubbed_species_binomial,",taxon_select,native_select,"latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select),"
                 FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY country,scrubbed_species_binomial;")
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  return(BIEN_sql(query, ...))
  
}

##############################

#'Extract species occurrence records by county.
#'
#'BIEN_occurrence_county extracts occurrences records for the specified county or counties.
#' @param country A single country or vector of countries.
#' @param state A single state or a vector of states.
#' @param county A single county or a vector of counties.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @note This function requires you supply either 1) a single country with one or more states, or 2) vectors of equal length for each political level.
#' @return Dataframe containing occurrence records for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_occurrence_county("United States","Arizona","Pima")
#' country_vector<-c("United States","United States")
#' state_vector<-c("Arizona","Michigan")
#' county_vector<-c("Pima","Kent")
#' BIEN_occurrence_county(country=country_vector, state = state_vector, county = county_vector)}
#' @family occurrence functions
BIEN_occurrence_county<-function(country, state, county, cultivated=FALSE, only.new.world=TRUE, print.query=FALSE, all.taxonomy=FALSE, native.status=FALSE, observation.type=FALSE,political.boundaries=FALSE, ...){
  is_char(country)
  is_char(state)
  is_char(county)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(observation.type)
  is_log(political.boundaries)
  
  #set conditions for query
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(only.new.world){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }else{
    newworld_query<-""
    newworld_select<-",is_new_world"
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-"observation_type"
  }
  
  if(!political.boundaries){
    political_select<-"state_province,"
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  #sql where
  if(length(country)==1 & length(state)==1){
    sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                       AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                       AND county in (", paste(shQuote(county, type = "sh"),collapse = ', '), ")
                       AND scrubbed_species_binomial IS NOT NULL")
  }else{
    
    if(length(country)==length(state) & length(country)==length(county)){
      
      sql_where<-"WHERE ("
      
      for(i in 1:length(country)){
        
        condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " 
                            AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), "
                            AND county = ", paste(shQuote(county[i], type = "sh"),collapse = ', '), ")
                            ")
        
        if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
        sql_where<-paste(sql_where,condition_i)
        
      }#for i  
      
      sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
      
    }else{
      stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")  
      
    }  
    
    
    
  }#if length(country>1)
  
  
  
  
  # set the query
  query <- paste("SELECT ",political_select," scrubbed_species_binomial," ,taxon_select , "latitude, longitude,date_collected,datasource,
                 dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,native_select,
                                                                                      observation_select),"
                 FROM view_full_occurrence_individual ",
                 sql_where,cultivated_query,newworld_query," 
                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) 
                 ORDER BY scrubbed_species_binomial;")
  
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}


############################
#'Extract species occurrence records by a latitude/longitude bounding box.
#'
#'BIEN_occurrence_box extracts occurrences records falling within the specific area.
#' @param min.lat Minimum latitude
#' @param max.lat Maximum latitude
#' @param min.long Minimum longitude
#' @param max.long Maximum longitude
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified area.
#' @examples \dontrun{
#' output_test<-
#' BIEN_occurrence_box(min.lat = 32,max.lat = 33,min.long = -114,max.long = -113,
#' cultivated = TRUE, only.new.world = FALSE)}
#' @family occurrence functions
BIEN_occurrence_box<-function(min.lat,max.lat,min.long,max.long,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,native.status=FALSE,observation.type=FALSE,political.boundaries=TRUE,print.query=FALSE, ...){
  is_num(min.lat)
  is_num(max.lat)
  is_num(min.long)
  is_num(max.long)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(observation.type)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(only.new.world){
    newworld_query<-"AND is_new_world = 1 "
    newworld_select<-""
  }else{
    newworld_query<-""
    newworld_select<-",is_new_world"
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!observation.type){
    observation_select<-""
  }else{
    observation_select<-",observation_type"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"country,state_province,county,locality,"
  }
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial,", taxon_select,political_select,native_select,"latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,observation_select),"
                 FROM view_full_occurrence_individual WHERE latitude between " , paste(shQuote(min.lat, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.lat, type = "sh"),collapse = ', '),"AND longitude between ", paste(shQuote(min.long, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.long, type = "sh"),collapse = ', '), paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}

#####

#'Download range maps for given species.
#'
#'BIEN_ranges_species extracts range maps for the specified species.
#' @param species A single species or a vector of species.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param matched Return a list of species that were downloaded. Default is TRUE.
#' @param match_names_only Check for range maps for the species specified without downloading range maps. Default is FALSE.
#' @param include.gid Should the files returned have a unique GID appended to them? This is needed if downloading multiple maps for the same species.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Range maps for specified species.
#' @examples \dontrun{
#' species_vector<-c("Abies_lasiocarpa","Abies_amabilis")
#' testwd<-"C:/wherever/you/want/files/saved/" #Set a working directory
#' BIEN_ranges_species(species_vector)
#' BIEN_ranges_species(species_vector,match_names_only = TRUE)
#' BIEN_ranges_species(species_vector,test_wd)#saves ranges to a specified working directory
#' BIEN_ranges_species("Abies_lasiocarpa")
#' BIEN_ranges_species("Abies_lasiocarpa","C:/wherever/you/want/files/saved/")
#'
#' #Reading files
#' setwd("C:/wherever/your/shapefiles/are/")
#' Abies_poly<-readShapePoly("Abies_lasiocarpa")
#' Abies_poly<-readShapePoly("C:/wherever/your/shapefiles/are/Abies_lasiocarpa.shp")
#'
#' #Plotting files
#' plot(Abies_poly)#plots the shapefile, but doesn't mean much without any reference
#' require(maps)#easy source of maps
#' map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#' plot(Abies_poly,col="forest green",add=TRUE) #adds the range of Abies lasiocarpa to the map
#'
#' #Getting data from the files (currently only species names)
#' Abies_poly$Species#gives the species name associated with "Abies_poly"}
#' @family range functions
BIEN_ranges_species<-function(species,directory=NULL,matched=TRUE,match_names_only=FALSE,include.gid=FALSE,print.query=FALSE, ...){
  is_char(species)
  is_log(matched)
  is_log(match_names_only)
  is_log(print.query)
  
  #make sure there are no spaces in the species names
  species<-gsub(" ","_",species)
  
  if(match_names_only==FALSE){
    
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory<-getwd()
    }
    
    
    # set the query
    query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species matched")
    }else{
      
      for(l in 1:length(df$species)){
        Species<-df$species[l]
        #sp_range<-readWKT(df$st_astext[l])
        sp_range<-rgeos::readWKT(df$st_astext[l],p4s="+init=epsg:4326")
        #proj4string(sp_range) <- CRS("+init=epsg:3857")
        #sp_range<-sp::spTransform(sp_range,sp::CRS("+init=epsg:4326"))#no longer needed, since files are now in 4326
        #assign(paste(species),sp_range,envir=.GlobalEnv)
        
        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
        #class(spdf)
        #maptools::writePolyShape(x=spdf,fn = Species)
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
          directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        }
        
        if(include.gid==T){
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l],"_",df$gid[l],sep=""),driver = "ESRI Shapefile",overwrite_layer = T)
        }else{
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l]),driver = "ESRI Shapefile",overwrite_layer = T)  
        }
        
        #save output
        
      }#for species in df loop
    }#else
    
    #list matched species
    if(matched==TRUE){
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found)<-c("Species","Range_map_downloaded?")
      found$`Range_map_downloaded?`<-as.character(found$`Range_map_downloaded?`)
      found$`Range_map_downloaded?`[which(species%in%df$species)]<-"Yes"
      return(found)
    }#matched = true
  }#match names only if statement
  
  if(match_names_only==TRUE){
    
    rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species matched")
    }else{
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found)<-c("Species","Range_map_available?")
      found$`Range_map_available?`<-as.character(found$`Range_map_available?`)
      found$`Range_map_available?`[which(species%in%df$species)]<-"Yes"
      return(found)
      
    }
    
  } #matched_names_only ==TRUE
}
####################################
#'Download range maps for given genus.
#'
#'BIEN_ranges_genus extracts range maps for the specified genera.
#' @param genus A single genus or a vector of genera.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param matched Return a list of species that were downloaded. Default is TRUE.
#' @param match_names_only Check for range maps for the genera specified without downloading range maps. Default is FALSE.
#' @param include.gid Should the filenames returned have a unique GID appended to them? This is needed if downloading multiple maps for the same species. Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Range maps for all available species within the specified genera.
#' @examples \dontrun{
#' genus_vector<-c("Abies","Acer")
#' testwd<-"C:/wherever/you/want/files/saved/" #Set a working directory
#' BIEN_ranges_genus(genus_vector)
#' BIEN_ranges_genus(genus_vector,match_names_only = TRUE)
#' BIEN_ranges_genus(genus_vector,test_wd)#saves ranges to a specified working directory
#' BIEN_ranges_genus("Abies")
#' BIEN_ranges_genus("Abies","C:/wherever/you/want/files/saved/")
#'
#' #Reading files
#' setwd("C:/wherever/your/shapefiles/are/")
#' Abies_poly<-readShapePoly("Abies_lasiocarpa")
#' Abies_poly<-readShapePoly("C:/wherever/your/shapefiles/are/Abies_lasiocarpa.shp")
#'
#' #Plotting files
#' plot(Abies_poly)#plots the shapefile, but doesn't mean much without any reference
#' require(maps)#easy source of maps
#' map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#' plot(Abies_poly,col="forest green",add=TRUE) #adds the range of Abies lasiocarpa to the map
#'
#' #Getting data from the files (currently only species names)
#' Abies_poly$Species#gives the species name associated with "Abies_poly"}
#' @family range functions
BIEN_ranges_genus<-function(genus,directory=NULL,matched=TRUE,match_names_only=FALSE,include.gid=FALSE,print.query=FALSE, ...){
  is_char(genus)
  is_log(matched)
  is_log(match_names_only)
  is_log(include.gid)
  is_log(print.query)

  #modify the genus list to make searching easier
  genus<-paste("(",genus,"_",")",sep = "")

if(match_names_only==FALSE){
  #record original working directory,change to specified directory if given
  if(is.null(directory)){
    directory<-getwd()
  }
  


    # set the query
    query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species ~ '",paste(genus,collapse="|"),"' ORDER BY species;",sep="")

    # create query to retrieve
    df <- BIEN_sql(query, ...)

    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }

    if(length(df)==0){
      message("No species matched")
    }else{

      for(l in 1:length(df$species)){
        Species<-df$species[l]
        sp_range<-rgeos::readWKT(df$st_astext[l],p4s="+init=epsg:4326")

        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
        #class(spdf)
        #maptools::writePolyShape(x=spdf,fn = Species)
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
          directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        }
        
        if(include.gid==T){
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l],"_",df$gid[l],sep=""),driver = "ESRI Shapefile",overwrite_layer = T)
        }else{
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l]),driver = "ESRI Shapefile",overwrite_layer = T)  
        }
        
        #save output
        

        
        
        
        
        
      }#for species in df loop
    }#else

    #setwd(wd) #return wd to original

    #list matched species
    if(matched==TRUE){
      found<-as.data.frame(df$species)
      return(found)
    }#matched = true
  }#match names only if statement

  if(match_names_only==TRUE){

    query <- paste("SELECT species FROM ranges WHERE species ~ '",paste(genus,collapse="|"),"' ORDER BY species;",sep="")

    #print(query)
    # create query to retrieve
    df <- BIEN_sql(query, ...)

    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }

    if(length(df)==0){
      message("No species matched")
    }else{
      found<-as.data.frame(df$species)
      return(found)
    }

  } #matched_names_only ==TRUE

}

#######################################
#'Download range maps that intersect a specified bounding box.
#'
#'BIEN_ranges_box extracts range maps for a specified bounding box.
#' @param min.lat Minimum latitude of the ranges included.
#' @param max.lat Maximumlatitude of the ranges included.
#' @param min.long Minimum longitude of the ranges included.
#' @param max.long Maximum longitude of the ranges included.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param species_names_only  Get a list of the species in the bounding box without downloading range maps. Default is FALSE.
#' @param return.species.list Should a species list be returned?  Only meaningful when maps are being downloaded.
#' @param crop.ranges Should the range maps be cropped to the GIS bounding box?  Default is FALSE.
#' @param include.gid Should the filenames returned have a unique GID appended to them? This is needed if downloading multiple maps for the same species. Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Range maps for all available species within the specified bounding box.
#' @examples \dontrun{
#' testwd<-"C:/wherever/you/want/files/saved/" #Set a working directory
#' BIEN_ranges_box(42,43,-85,-84,species_names_only = TRUE)
#' BIEN_ranges_box(42,43,-85,-84,directory = testwd)}
#' @family range functions
BIEN_ranges_box<-function(min.lat, max.lat, min.long, max.long, directory=NULL, species_names_only=FALSE, return.species.list = TRUE ,crop.ranges=FALSE,include.gid=FALSE,print.query=FALSE, ...){
  is_num(min.lat)
  is_num(max.lat)
  is_num(min.long)
  is_num(max.long)
  is_log(include.gid)
  is_log(return.species.list)
  is_log(species_names_only)
  is_log(print.query)
  
  if(species_names_only==FALSE){
    
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory<-getwd()
    }    
    
    # set the query
    #query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    
    if(crop.ranges){
        query<-paste("SELECT ST_AsText(ST_intersection(geom,ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326))),species,gid FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)") 
    }else{
        query<-paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)")  
    }
    
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species matched")
    }else{
      
      for(l in 1:length(df$species)){
        Species<-df$species[l]
        #sp_range<-readWKT(df$st_astext[l])
        sp_range<-rgeos::readWKT(df$st_astext[l],p4s="+init=epsg:4326")
        #proj4string(sp_range) <- CRS("+init=epsg:3857")
        #sp_range<-sp::spTransform(sp_range,sp::CRS("+init=epsg:4326"))#no longer needed, since files are now in 4326
        #assign(paste(species),sp_range,envir=.GlobalEnv)
        
        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
        #class(spdf)
        #maptools::writePolyShape(x=spdf,fn = Species)
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
          directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        }
        
        if(include.gid==T){
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l],"_",df$gid[l],sep=""),driver = "ESRI Shapefile",overwrite_layer = T)
        }else{
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l]),driver = "ESRI Shapefile",overwrite_layer = T)  
        }
        
        
        
        
        
        
        
        #save output
        
      }#for species in df loop
    
      if(return.species.list){
        return(df[,2])  
      }#if return.species.list  
      
    }#else
    
    #setwd(wd) #return wd to original
    
    
  }#species names only if statement
  
  if(species_names_only==TRUE){
    
    #rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    query<-paste("SELECT species FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)")  
    
    #query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species found")
    }else{
      return(df)
      
    }
    
  } #species_names_only ==TRUE
}
#######################################
#'Download range maps that intersect the range of a given species.
#'
#'BIEN_ranges_intersect_species extracts range maps for a specified bounding box.
#' @param species Focal species (or a vector of species) for which to extract intersecting ranges.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param species.names.only  Get a list of the species with ranges overlapping the focal species' range without downloading range maps. Default is FALSE.
#' @param include.focal Should the focal species be returned as well? Default is TRUE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param return.species.list Should a species list be returned?  Only meaningful when maps are being downloaded.
#' @param include.gid Should the filenames returned have a unique GID appended to them? This is needed if downloading multiple maps for the same species. Default is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Range maps for all available species that intersect the range of the focal species.
#' @examples \dontrun{
#' testwd<-"C:/wherever/you/want/files/saved/" #Set a working directory
#' BIEN_ranges_intersect_species(species = "Carnegiea_gigantea",
#' directory = testwd,include.focal = TRUE)
#' species_vector<-c("Carnegiea_gigantea","Echinocereus coccineus")
#' BIEN_ranges_intersect_species(species = speciesvector,species.names.only = TRUE)}
#' @family range functions
#' @author Daniel Guaderrama
BIEN_ranges_intersect_species<-function(species, directory=NULL, species.names.only=FALSE, include.focal=TRUE,return.species.list=TRUE,include.gid=FALSE,print.query=FALSE, ...){
  is_char(species)
  is_log(species.names.only)
  is_log(print.query)
  is_log(include.focal)
  is_log(include.gid)
  
  #make sure there are no spaces in the species names
  species<-gsub(" ","_",species)
  
  #set query chunk to include focal species
  if(include.focal){
    focal.query <- ""  
  }else{
    focal.query <- "a.species != b.species AND" 
  }
  
  
  if(species.names.only==FALSE){
    
    #set directory for saving
    if(is.null(directory)){
      directory<-getwd()
    }  
    
    # set the query
    
    #query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    
    #query<- paste("SELECT b.species AS focal_species, a.species AS intersecting_species, ST_AsText(a.geom) AS geom FROM ranges AS a, (SELECT species, geom FROM ranges WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE a.species != b.species AND ST_Intersects(a.geom, b.geom);")
    query<- paste("SELECT b.species AS focal_species, a.species AS intersecting_species,a.species,a.gid, ST_AsText(a.geom) AS geom FROM ranges AS a, (SELECT species, geom FROM ranges WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE", focal.query," ST_Intersects(a.geom, b.geom);")  
    
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species matched")
    }else{
      
      for(l in 1:length(df$intersecting_species)){
        Species<-df$intersecting_species[l]
        #sp_range<-readWKT(df$st_astext[l])
        sp_range<-rgeos::readWKT(df$geom[l],p4s="+init=epsg:4326")
        #proj4string(sp_range) <- CRS("+init=epsg:3857")
        #sp_range<-sp::spTransform(sp_range,sp::CRS("+init=epsg:4326"))#no longer needed, since files are now in 4326
        #assign(paste(species),sp_range,envir=.GlobalEnv)
        
        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
        #class(spdf)
        #maptools::writePolyShape(x=spdf,fn = Species)
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
          directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        }
        
        if(include.gid==T){
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l],"_",df$gid[l],sep=""),driver = "ESRI Shapefile",overwrite_layer = T)
        }else{
          rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l]),driver = "ESRI Shapefile",overwrite_layer = T)  
        }
        
        #save output
        
      }#for species in df loop
      
      if(return.species.list){
        return(df[,1:2])  
      }    
      
    }#else
    
    #setwd(wd) #return wd to original
    
  }#species names only if statement
  
  if(species.names.only==TRUE){
    
    #rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    #query<- paste("SELECT b.species AS focal_species, a.species AS intersecting_species FROM ranges AS a, (SELECT species, geom FROM ranges WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE a.species != b.species AND ST_Intersects(a.geom, b.geom);")
    query<- paste("SELECT b.species AS focal_species, a.species AS intersecting_species FROM ranges AS a, (SELECT species, geom FROM ranges WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE", focal.query," ST_Intersects(a.geom, b.geom);")  
    
    
    #query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- BIEN_sql(query, ...)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species found")
    }else{
      return(df)
      
    }
    
  } #species_names_only ==TRUE
}

#######################################
#'Download range maps that intersect a user-supplied shapefile.
#'
#'BIEN_ranges_shapefile extracts range maps that interesect a specified shapefile.
#' @param shapefile An object of class SpatialPolygonsDataFrame.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param species_names_only  Get a list of the species with ranges overlapping the focal species' range without downloading range maps. Default is FALSE.
#' @param return.species.list Should a species list be returned?  Only meaningful when maps are being downloaded.
#' @param crop.ranges Should the returned shapefiles be cropped to the supplied shapefile?
#' @param include.gid Should the filenames returned have a unique GID appended to them? This is needed if downloading multiple maps for the same species. Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return All range maps that intersect the user-supplied shapfile.
#' @note We recommend using the function readOGR() in the rgdal package to read in shapefiles.  Other methods may cause problems related to handling holes in polygons.
#' @examples \dontrun{
#' BIEN_ranges_species("Carnegiea gigantea")#saves ranges to the current working directory
#' shape<-readOGR(dsn = ".",layer = "Carnegiea_gigantea")
#' #shapefiles should be read with readOGR(), see note.
#' BIEN_ranges_shapefile(shapefile = shape) 
#' #Note that this will save many shapefiles to the working directory.
#' }
#' @family range functions
BIEN_ranges_shapefile<-function(shapefile, directory=NULL, species_names_only=FALSE, return.species.list = TRUE ,crop.ranges=FALSE,include.gid=FALSE,print.query=FALSE,...){
  is_log(return.species.list)
  is_log(species_names_only)
  is_log(print.query)
  is_log(crop.ranges)
  is_log(include.gid)
  
  wkt<-rgeos::writeWKT(shapefile)
  
  
  
  if(species_names_only==FALSE){
    
    #set directory for saving
    if(is.null(directory)){
      directory<-getwd()
    }  
    
    # set the query
    #query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    
    if(crop.ranges){
      query<-paste("SELECT ST_AsText(ST_intersection(geom,ST_GeographyFromText('SRID=4326;",paste(wkt),"'))),species,gid FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom)") 
    }else{
      query<-paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom)")  
    }
    
    # create query to retrieve
    df <- BIEN_sql(query)
    
    if(print.query){
      print(query)
    }
    
    if(length(df)==0){
      message("No species matched")
    }else{
      
      for(l in 1:length(df$species)){
        Species<-df$species[l]
        #sp_range<-readWKT(df$st_astext[l])
        sp_range<-rgeos::readWKT(df$st_astext[l],p4s="+init=epsg:4326")
        if(!is.null(sp_range)){
          
          #proj4string(sp_range) <- CRS("+init=epsg:3857")
          #sp_range<-sp::spTransform(sp_range,sp::CRS("+init=epsg:4326"))#no longer needed, since files are now in 4326
          #assign(paste(species),sp_range,envir=.GlobalEnv)
          
          #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
          spdf<-as.data.frame(Species)
          spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
          #class(spdf)
          #maptools::writePolyShape(x=spdf,fn = Species)
          
          #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
          if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
            directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
          }
          
          if(include.gid==T){
            rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l],"_",df$gid[l],sep=""),driver = "ESRI Shapefile",overwrite_layer = T)
          }else{
            rgdal::writeOGR(obj = spdf,dsn = directory,layer = paste(df$species[l]),driver = "ESRI Shapefile",overwrite_layer = T)  
          }
          
          #save output
        }#if sp_range is not null  
      }#for species in df loop
      if(return.species.list){
        
        return(df[,2])  
      }#if return.species.list  
      
    }#else
    
  }#species names only if statement
  
  if(species_names_only==TRUE){
    
    query<-paste("SELECT species FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom)")  
    
    # create query to retrieve
    df <- BIEN_sql(query)
    
    if(print.query){
      query<-gsub(pattern = "\n",replacement = "",query)
      query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
      print(query)
    }
    
    if(length(df)==0){
      message("No species found")
    }else{
      return(df)
      
    }
    
  } #species_names_only ==TRUE
}

#######################################
#'Load range maps for specified species.
#'
#'BIEN_ranges_load_species returns spatial data for the specified species.
#' @param species A single species or a vector of species.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A SpatialPolygonsDataFrame containing range maps for the specified species.
#' @examples \dontrun{
#' species_vector<-c("Abies_lasiocarpa","Abies_amabilis")
#' abies_maps<-BIEN_ranges_load_species(species = species_vector)
#' xanthium_strumarium<-BIEN_ranges_load_species(species = "Xanthium strumarium")
#' 
#' #Plotting files
#' plot(abies_maps)#plots the shapefile, but doesn't mean much without any reference
#' require(maps) #easy source of maps
#' map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#' plot(xanthium_strumarium,col="forest green",add=TRUE) #adds the range of X. strumarium
#' plot(abies_maps[1,], add = T, col ="light green")}
#' @family range functions
BIEN_ranges_load_species<-function(species,print.query=FALSE, ...){
  is_char(species)
  is_log(print.query)
  
  #make sure there are no spaces in the species names
  species<-gsub(" ","_",species)
  
  # set the query
  query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
  
  # create query to retrieve
  df <- BIEN_sql(query, ...)
  #df <- BIEN_sql(query)
  
  
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  if(length(df)==0){
    message("No species matched")
  }else{
    
    poly<-list()
    for(l in 1:length(df$species)){
      Species<-df$species[l]
      #sp_range<-readWKT(df$st_astext[l])
      poly[[l]]<-rgeos::readWKT(df$st_astext[l],p4s="+init=epsg:4326")
      methods::slot(object = poly[[l]]@polygons[[1]],name = "ID")<-as.character(df$gid[l])#assigns a unique ID to each species' polygon
      
      #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
      #spdf<-as.data.frame(Species)
      #spdf<-sp::SpatialPolygonsDataFrame(sp_range,spdf)
      #class(spdf)
      #maptools::writePolyShape(x=spdf,fn = Species)
      #save output
      
    }#for species in df loop
    
    
  }#else
  poly<-sp::SpatialPolygons(unlist(lapply(poly, function(x) x@polygons)))
  poly<-sp::SpatialPolygonsDataFrame(Sr = poly,data = df['species'],match.ID = FALSE)    
  poly@proj4string<-sp::CRS(projargs = "+init=epsg:4326")
  return(poly) 
  
}



########################################
########################################

#'Download trait data for given species.
#'
#'BIEN_trait_species extracts trait data for the species specified.
#' @param species A single species or a vector of species.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all available trait data for the given species.
#' @examples \dontrun{
#' BIEN_trait_species("Poa annua")
#' species_vector<-c("Poa annua","Juncus trifidus")
#' BIEN_trait_species(species_vector)}
#' @family trait functions
BIEN_trait_species<-function(species,print.query=FALSE, ...){
  is_char(species)
  is_log(print.query)

  # set the query
  query <- paste("SELECT * FROM agg_traits WHERE taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
############################
#'Calculates species mean values for a given trait, using Genus or Family level data where Species level data is lacking.
#'
#'BIEN_trait_mean Estimates species mean values for a given trait, using Genus or Family level data where Species level data is absent.
#' @param species A single species or a vector of species.
#' @param trait A single trait.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of estimated trait means and associated metadata for the given species.
#' @examples \dontrun{
#' BIEN_trait_mean(species=c("Poa annua","Juncus trifidus"),trait="Height") }
#' @family trait functions
BIEN_trait_mean<-function(species,trait, ...){
  
  #first, get taxonomic info for the species
  is_char(trait)
  is_char(species)
  
  # create query to retreive taxonomic info
  genera<-unlist(lapply(X = strsplit(species," "),FUN = function(x){x[1]}))
  
  query <- paste("SELECT DISTINCT scrubbed_family,scrubbed_genus,scrubbed_species_binomial FROM bien_taxonomy WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") or scrubbed_genus in (", paste(shQuote(genera, type = "sh"),collapse = ', '), ");")
  
  
  taxonomy_for_traits <- BIEN_sql(query, ...)
  if(length(taxonomy_for_traits)==0){stop("Taxonomic data missing, check species name(s)")}
  
  
  #then, query the various taxonomic levels to get trait data
  #old query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND family in (", paste(shQuote(unique(taxonomy_for_traits$scrubbed_family)  , type = "sh"),collapse = ', '), ") ORDER BY family,taxon,trait_name;")
  
  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND (family in (", paste(shQuote(unique(taxonomy_for_traits$scrubbed_family)  , type = "sh"),collapse = ', '), ") or  genus in (", paste(shQuote(unique(taxonomy_for_traits$scrubbed_genus)  , type = "sh"),collapse = ', '), ")) ORDER BY family,taxon,trait_name;")
  
  traits_df <- BIEN_sql(query, ...)
  
  #finally, choose the best available trait data
  
  output_data<-NULL
  for(i in 1:length(species)){
    
    species_i_data<-list()
    species_i_data[[1]]<-traits_df$trait_value[which(traits_df$taxon==species[i])]
    species_i_data[[2]]<-traits_df$trait_value[which(traits_df$genus==taxonomy_for_traits$scrubbed_genus[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])]
    if(length(species_i_data[[2]])==0){
      species_i_data[[2]]<-traits_df$trait_value[which(traits_df$genus==strsplit(species[i]," ")[[1]][1])]
    }
    #species_i_data[[3]]<-traits_df$trait_value[which(traits_df$family==taxonomy_for_traits$scrubbed_family[i])]
    species_i_data[[3]]<-traits_df$trait_value[which(traits_df$family==taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])]
    if(length(species_i_data[[3]])==0){
      species_i_data[[3]]<-traits_df$trait_value[which(traits_df$family==unique(taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_genus==strsplit(species[i]," ")[[1]][1])]))]
    }
    species_i_data[[4]]<-"NA"
    
    names(species_i_data)<-c("Species","Genus","Family","NA")
    
    species_i_data<-species_i_data[which(lengths(species_i_data)>0)]#prunes list to include only taxonomic levels with data
    
    #trait_mean<- species_i_data[1]
    
    if(length(species_i_data)>0){
      level_used<-names(species_i_data[1])
      if(species_i_data[[1]][1]=="NA"){sample_size<-0}else{sample_size<-length(species_i_data[[1]])}
      if(species_i_data[[1]][1]=="NA"){mean_value<-"NA"}else{mean_value<-mean(as.numeric(species_i_data[[1]]))}
      unit<-unique(traits_df$unit)
      output_data<-rbind(output_data,cbind(species[i],mean_value,trait,unit,level_used,sample_size))
    }#if data is available
    
    
    
  }#i loop
  
  colnames(output_data)[1]<-"species"
  return(output_data)
  
}
############################

#'Download all measurements of a specific trait(s).
#'
#'BIEN_trait_trait downloads all measurements of the trait(s) specified.
#' @param trait A single trait or a vector of traits.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all available trait data for the given trait(s).
#' @examples \dontrun{
#' BIEN_trait_trait("Height")
#' trait_vector<-c("Height", "Leaf dry mass")
#' BIEN_trait_trait(trait_vector)}
#' @family trait functions
BIEN_trait_trait<-function(trait,print.query=FALSE, ...){
  is_char(trait)
  is_log(print.query)

# set the query
  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ORDER BY taxon;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
############################

#'Download trait data for given species and trait.
#'
#'BIEN_trait_traitbyspecies extracts entries that contain the specified species and trait(s).
#' @param species A single species or a vector of species.
#' @param trait A single trait or a vector of traits.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all data matching the specified trait(s) and species.
#' @examples \dontrun{
#' BIEN_trait_traitbyspecies(trait = "Height", species = "Carex capitata")
#' trait_vector<-c("Height", "Leaf dry mass")
#' species_vector<-c("Carex capitata","Betula nana")
#' BIEN_trait_traitbyspecies(trait=trait_vector,species=species_vector)}
#' @family trait functions
BIEN_trait_traitbyspecies<-function(trait,species,print.query=FALSE, ...){
  is_char(trait)
  is_char(species)
  is_log(print.query)

# set the query
  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon,trait_name;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
###########################

#'Download trait data for given genus/genera and trait(s).
#'
#'BIEN_trait_traitbygenus extracts entries that contain the specified genus/genera and trait(s).
#' @param genus A single genus or a vector of genera.
#' @param trait A single trait or a vector of traits.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all data matching the specified trait(s) and genus/genera.
#' @examples \dontrun{
#' BIEN_trait_traitbygenus(trait = "Height", genus = "Carex")
#' trait_vector<-c("Height", "Leaf dry mass")
#' genus_vector<-c("Carex","Betula")
#' BIEN_trait_traitbygenus(trait=trait_vector,genus=genus_vector)}
#' @family trait functions
BIEN_trait_traitbygenus<-function(trait,genus,print.query=FALSE, ...){
  is_char(trait)
  is_char(genus)
  is_log(print.query)

# set the query
  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus,taxon,trait_name;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
###########################

#'Download trait data for given families and traits.
#'
#'BIEN_trait_traitbyfamily extracts entries that contain the specified families and trait(s).
#' @param family A single family or a vector of families.
#' @param trait A single trait or a vector of traits.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all data matching the specified trait(s) and family/families.
#' @examples \dontrun{
#' BIEN_trait_traitbyfamily(trait = "Height", family = "Poaceae")
#' trait_vector<-c("Height", "Leaf dry mass")
#' family_vector<-c("Orchidaceae","Poaceae")
#' BIEN_trait_traitbyfamily(trait=trait_vector,family=family_vector)}
#' @family trait functions
BIEN_trait_traitbyfamily<-function(trait,family,print.query=FALSE, ...){
  is_char(trait)
  is_char(family)
  is_log(print.query)

  # set the query
  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon,trait_name;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}
############################

#'Download trait data for given genera.
#'
#'BIEN_trait_genus extracts entries that contain the specified genera.
#' @param genus A single genus or a vector of genera.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all data matching the specified genera.
#' @examples \dontrun{
#' BIEN_trait_genus("Acer")
#' genus_vector<-c("Acer","Abies")
#' BIEN_trait_genus(genus_vector)}
#' @family trait funcitons
BIEN_trait_genus<-function(genus,print.query=FALSE, ...){
  is_char(genus)
  is_log(print.query)

  # set the query
  query <- paste("SELECT * FROM agg_traits WHERE genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

###########################

#'Download trait data for given families.
#'
#'BIEN_trait_family extracts all trait data for the specified families.
#' @param family A single family or a vector of families.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe of all data matching the specified families.
#' @examples \dontrun{
#' BIEN_trait_family("Poaceae")
#' family_vector<-c("Poaceae","Orchidaceae")
#' BIEN_trait_family(family_vector)}
#' @family trait functions
BIEN_trait_family<-function(family,print.query=FALSE, ...){
  is_char(family)
  is_log(print.query)

  # set the query
  query <- paste("SELECT * FROM agg_traits WHERE family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

############################

#'List all available types of trait data
#'
#'BIEN_trait_list produces a dataframe of all available types of trait data.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all currently available types of trait data and details on measurement.
#' @examples \dontrun{
#' BIEN_trait_list()}
#' @family trait functions
BIEN_trait_list<-function(print.query=FALSE, ...){
  is_log(print.query)

  # set the query
  query <- paste("SELECT DISTINCT trait_name FROM agg_traits ORDER BY trait_name;")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

#############################

#'Count the number of (geoValid) occurrence records for each species in BIEN
#'
#'BIEN_occurrence_records_per_species downloads a count of the number of geovalidated occurence records for each species in the BIEN database.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe listing the number of geovalidated occurrence records for each species in the BIEN database.
#' @examples \dontrun{
#' occurrence_counts<-BIEN_occurrence_records_per_species()}
#' @family occurrence functions
BIEN_occurrence_records_per_species<-function(print.query=FALSE, ...){
  is_log(print.query)

# set the query
  query<-paste("SELECT DISTINCT scrubbed_species_binomial,count(*) FROM view_full_occurrence_individual WHERE is_geovalid = 1 GROUP BY scrubbed_species_binomial")

  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

###############################################
#'Count the number of trait observations for each species in the BIEN database
#'
#'BIEN_trait_traits_per_species downloads a count of the number of records for each trait for each species in the BIEN database.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Returns a dataframe containing the number of trait records for each species in the BIEN database.
#' @examples \dontrun{
#' trait_observation_counts<-BIEN_trait_traits_per_species()}
#' @family trait functions
BIEN_trait_traits_per_species<-function(print.query=FALSE, ...){
  is_log(print.query)

  # set the query
  query <- paste("SELECT DISTINCT taxon, trait_name,count(*) FROM agg_traits GROUP BY trait_name,taxon ORDER BY taxon,trait_name;")
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

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



####################################
#'Run an SQL query on the BIEN database.
#'
#'BIEN_sql allows users to supply their own PostgreSQL query.
#' @param query A PostgreSQL query.
#' @param view_full_occurrence_individual Alternative value to be substituted for "view_full_occurrence_individual" in queries when not NULL.
#' @param agg_traits Alternative value to be substituted for "agg_traits" in queries when not NULL.
#' @param species_by_political_division Alternative value to be substituted for "species_by_political_division" in queries when not NULL.
#' @param bien_species_all Alternative value to be substituted for "bien_species_all" in queries when not NULL.
#' @param ranges Alternative value to be substituted for "ranges" in queries when not NULL.
#' @param bien_taxonomy Alternative value to be substituted for "bien_taxonomy" in queries when not NULL.
#' @param phylogeny Alternative value to be substituted for "phylogeny" in queries when not NULL.
#' @param bien_metadata Alternative value to be substituted for "bien_metadata" in queries when not NULL.
#' @return A dataframe returned by the query.
#' @examples \dontrun{
#' BIEN_sql("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual 
#' WHERE country in ( 'United States' );")}
BIEN_sql<-function(query,view_full_occurrence_individual=NULL,agg_traits=NULL,species_by_political_division=NULL,
                   bien_species_all=NULL,ranges=NULL,bien_taxonomy=NULL,phylogeny=NULL,bien_metadata=NULL){
  is_char(query)
  
  if(!is.null(view_full_occurrence_individual)){
    query<-gsub(pattern = "view_full_occurrence_individual",replacement = view_full_occurrence_individual,x = query)}
  
  if(!is.null(agg_traits)){
    query<-gsub(pattern = "agg_traits",replacement = agg_traits,x = query)}
  
  if(!is.null(species_by_political_division)){
    query<-gsub(pattern = "species_by_political_division",replacement = species_by_political_division,x = query)}
  
  if(!is.null(bien_species_all)){
    query<-gsub(pattern = "bien_species_all",replacement = bien_species_all,x = query)}
  
  if(!is.null(ranges)){
    query<-gsub(pattern = "ranges",replacement = ranges,x = query)}
  
  if(!is.null(bien_taxonomy)){
    query<-gsub(pattern = "bien_taxonomy",replacement = bien_taxonomy,x = query)}
  
  if(!is.null(phylogeny)){
    query<-gsub(pattern = "\\<phylogeny\\>",replacement = phylogeny,x = query)}
  
  if(!is.null(bien_metadata)){
    query<-gsub(pattern = "bien_metadata",replacement = bien_metadata,x = query)}
  
  
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- DBI::dbDriver('PostgreSQL')
  # establish connection with database
  con <- DBI::dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  # create query to retrieve
  df <- DBI::dbGetQuery(con, statement = query);
  
  DBI::dbDisconnect(con)
  return(df)
  
}

################################

################################
#Plot queries

##############################
#'Download plot data from a given datasource.
#'
#'BIEN_plot_datasource downloads all plot data from a given datasource.
#' @param datasource A datasource. See BIEN.plot.list_datasource() for options.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all data from the specified datasource.
#' @examples \dontrun{
#' BIEN_plot_datasource("SALVIAS")}
#' @family plot functions
BIEN_plot_datasource<-function(datasource,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=FALSE,all.metadata=FALSE, ...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(datasource)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)
  is_log(all.metadata)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (view_full_occurrence_individual.is_cultivated = 0 OR view_full_occurrence_individual.is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",view_full_occurrence_individual.is_cultivated,view_full_occurrence_individual.is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"view_full_occurrence_individual.verbatim_family,view_full_occurrence_individual.verbatim_scientific_name,view_full_occurrence_individual.family_matched,view_full_occurrence_individual.name_matched,view_full_occurrence_individual.name_matched_author,view_full_occurrence_individual.higher_plant_group,view_full_occurrence_individual.taxonomic_status,view_full_occurrence_individual.scrubbed_family,view_full_occurrence_individual.scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"view_full_occurrence_individual.native_status,view_full_occurrence_individual.native_status_reason,view_full_occurrence_individual.native_status_sources,view_full_occurrence_individual.isintroduced,view_full_occurrence_individual.native_status_country,view_full_occurrence_individual.native_status_state_province,view_full_occurrence_individual.native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.scrubbed_species_binomial,view_full_occurrence_individual.individual_count,",taxon_select,political_select,native_select," view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,view_full_occurrence_individual.custodial_institution_codes,view_full_occurrence_individual.collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                (SELECT * FROM view_full_occurrence_individual 
                  WHERE view_full_occurrence_individual.datasource in (", paste(shQuote(datasource, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query),  "
                  AND higher_plant_group IS NOT NULL AND (view_full_occurrence_individual.is_geovalid = 1 OR view_full_occurrence_individual.is_geovalid IS NULL) 
                  AND observation_type='plot' ORDER BY plot_name,subplot,scrubbed_species_binomial) as view_full_occurrence_individual
                  JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                 ;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}


##################################
#'List available datasources.
#'
#'BIEN_plot_list_datasource list all plot datasources in the BIEN database.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A vector of available datasources.
#' @examples \dontrun{
#' BIEN_plot_list_datasource()}
#' @family plot functions
BIEN_plot_list_datasource<-function(...){
  query <- paste("SELECT DISTINCT datasource FROM plot_metadata;")
  return(BIEN_sql(query, ...))

}

###############################
#'Download plot data from specified countries.
#'
#'BIEN_plot_country downloads all plot data from specified countries.
#' @param country A country or vector of countries.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all data from the specified countries.
#' @examples \dontrun{
#' BIEN_plot_country("Costa Rica")
#' BIEN_plot_country(c("Costa Rica","Panama"))}
#' @family plot functions
BIEN_plot_country<-function(country,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=FALSE,all.metadata=FALSE, ...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(country)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-"view_full_occurrence_individual.country,"
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  
  
  # set the query
  query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, view_full_occurrence_individual.elevation_m,
                 view_full_occurrence_individual.plot_area_ha, view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, 
                 view_full_occurrence_individual.scrubbed_species_binomial,view_full_occurrence_individual.individual_count,",paste(taxon_select),paste(native_select)," 
                 view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude, view_full_occurrence_individual.date_collected,
                 view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,
                 view_full_occurrence_individual.custodial_institution_codes,view_full_occurrence_individual.collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                 (SELECT * FROM view_full_occurrence_individual WHERE view_full_occurrence_individual.country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",
                 paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL 
                 AND (view_full_occurrence_individual.is_geovalid = 1 OR view_full_occurrence_individual.is_geovalid IS NULL) AND observation_type='plot' 
                 ORDER BY country,plot_name,subplot,scrubbed_species_binomial) as view_full_occurrence_individual 
                 LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                 ;")
  
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
  }
###############################
#'Download plot data from specified states/provinces.
#'
#'BIEN_plot_state downloads all plot data from specified states/provinces.
#' @param country A single country.
#' @param state A state or vector of states.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @note This function requires you supply either 1) a single country with one or states, or 2) vectors of equal length for each political level.
#' @return A dataframe containing all data from the specified states.
#' @examples \dontrun{
#' BIEN_plot_state("Colorado")
#' BIEN_plot_state(c("Colorado","California"))}
#' @family plot functions
BIEN_plot_state<-function(country,state,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=TRUE,all.metadata=FALSE, ...){
  is_char(country)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(state)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)
  is_log(all.metadata)
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,"
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  #state where
  if(length(country)==1){
    sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                       AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                       AND scrubbed_species_binomial IS NOT NULL")
  }else{
    
    if(length(country)==length(state)){
      
      sql_where<-"WHERE ("
      
      for(i in 1:length(country)){
        
        condition_i<- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '), " AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), ")")
        if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
        sql_where<-paste(sql_where,condition_i)
        
      }#for i  
      
      sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
      
    }else{
      stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
      
    }  
    
    
    
  }#if length(country>1)
  
  
  
  
  
  # set the query
  query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count,",paste(taxon_select),paste(native_select)," view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                 (SELECT * FROM view_full_occurrence_individual ",
                 sql_where,cultivated_query,newworld_query,  "
                 AND higher_plant_group IS NOT NULL AND (view_full_occurrence_individual.is_geovalid = 1 OR view_full_occurrence_individual.is_geovalid IS NULL) 
                 AND observation_type='plot' 
                 ORDER BY country,plot_name,subplot,scrubbed_species_binomial) as view_full_occurrence_individual 
                 JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                 ;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
  }


###############################
#'List available sampling protocols.
#'
#'BIEN_plot_list_sampling_protocols list all available sampling protocols.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A vector of available sampling protocols.
#' @examples \dontrun{
#' BIEN_plot_list_sampling_protocols()}
#' @family plot functions
BIEN_plot_list_sampling_protocols<-function(...){
  query <- paste("SELECT DISTINCT sampling_protocol FROM plot_metadata;")
  return(BIEN_sql(query, ...))

}

################################
#'Download plot data using a specified sampling protocol.
#'
#'BIEN_plot_sampling_protocol downloads all plot data using a specified sampling protocol.
#' @param sampling_protocol A sampling protocol or vector of sampling protocols. See BIEN.plot.list_sampling_protocols() for options.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all data from the specified datasource.
#' @examples \dontrun{
#' BIEN_plot_sampling_protocol("Point-intercept")}
#' @family plot functions
BIEN_plot_sampling_protocol<-function(sampling_protocol,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=FALSE,all.metadata=FALSE, ...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(sampling_protocol)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
                growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
                taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,
                  view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count,",taxon_select,native_select,political_select," 
                  view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,date_collected,view_full_occurrence_individual.datasource,
                  view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,
                                                                                                                                                       newworld_select,md_select),"
                  FROM 
                  (SELECT * FROM view_full_occurrence_individual 
                  WHERE view_full_occurrence_individual.sampling_protocol in (", paste(shQuote(sampling_protocol, type = "sh"),collapse = ', '), ")",
                 paste(cultivated_query,newworld_query),  "AND view_full_occurrence_individual.higher_plant_group IS NOT NULL 
                  AND (view_full_occurrence_individual.is_geovalid = 1 OR view_full_occurrence_individual.is_geovalid IS NULL) 
                  AND view_full_occurrence_individual.observation_type='plot' 
                  ORDER BY view_full_occurrence_individual.country,view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot,
                  view_full_occurrence_individual.scrubbed_species_binomial) as view_full_occurrence_individual
                  JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                  ;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}
#################################
#'Download plot data by plot name.
#'
#'BIEN_plot_name downloads all plot data for a set of plot names.
#' @param plot.name A plot name or vector of names. See BIEN_plot_metadata for more information on plots.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all data from the specified datasource.
#' @examples \dontrun{
#' BIEN_plot_name("SR-1")}
#' @family plot functions
BIEN_plot_name<-function(plot.name,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=FALSE,all.metadata=FALSE, ...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(plot.name)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)  
  is_log(all.metadata)
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,
                  view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.scrubbed_species_binomial,
                  view_full_occurrence_individual.individual_count,",taxon_select,native_select,political_select," 
                 view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                 view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,
                 view_full_occurrence_individual.custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                 (SELECT * FROM view_full_occurrence_individual WHERE view_full_occurrence_individual.plot_name in (", paste(shQuote(plot.name, type = "sh"),collapse = ', '), ")",
                 paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) AND observation_type='plot' 
                 ORDER BY country,plot_name,subplot,scrubbed_species_binomial) as view_full_occurrence_individual
                 LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                 ;")
  
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}

#####################
#'Download plot data by dataset.
#'
#'BIEN_plot_dataset downloads all plot data for a given dataset or datasets.
#' @param dataset A plot dataset or vector of datasets. See BIEN_plot_metadata for more information on plots.
#' @param cultivated Return cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing all data from the specified dataset.
#' @examples \dontrun{
#' BIEN_plot_dataset("Gentry Transect Dataset")}
#' @family plot functions
BIEN_plot_dataset<-function(dataset,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE,print.query=FALSE,native.status=FALSE,political.boundaries=FALSE,all.metadata=FALSE, ...){
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(dataset)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)
  is_log(all.metadata)
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",is_cultivated,is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",view_full_occurrence_individual.is_new_world"
  }else{
    newworld_query<-"AND view_full_occurrence_individual.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_family,scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,
                  view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count,",taxon_select,native_select,political_select," 
                  view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                  view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,
                  view_full_occurrence_individual.dataowner,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                  (SELECT * FROM view_full_occurrence_individual WHERE view_full_occurrence_individual.dataset in (", paste(shQuote(dataset, type = "sh"),collapse = ', '), ")",
                 cultivated_query,newworld_query,  " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) AND observation_type='plot' 
                 ORDER BY country,plot_name,subplot,scrubbed_species_binomial) as view_full_occurrence_individual
                  LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                 ;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}


##############################
#'Download plot metadata
#'
#'BIEN_plot_metadata downloads the plot metadata table. 
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A dataframe containing plot metadata.
#' @examples \dontrun{
#' BIEN_plot_metadata()}
#' @family plot functions
#' @family metadata functions
BIEN_plot_metadata<-function(print.query=FALSE, ...){
  is_log(print.query)
  #set conditions for query
  # set the query
  query <- "SELECT * FROM plot_metadata;"
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}



##############################

################################
#Taxonomy queries

#'Extract taxonomic information for species
#'
#'BIEN_taxonomy_species downloads a dataframe of all taxonomic information for given species.
#' @param species A single species or a vector of species.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing taxonomic information for the specified species.
#' @examples \dontrun{
#' BIEN_taxonomy_species("Cannabis sativa")
#' species_vector<-c("Acer nigrum","Cannabis sativa")
#' BIEN_taxonomy_species(species_vector)}
#' @family taxonomy functions
BIEN_taxonomy_species<-function(species,print.query=FALSE, ...){
  is_char(species)
  is_log(print.query)
  
  #set base query components
  sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
  sql_from <- paste(' FROM bien_taxonomy')
  sql_where <- paste(' WHERE scrubbed_species_binomial in (', paste(shQuote(species, type = "sh"),collapse = ', '), ') AND scrubbed_species_binomial IS NOT NULL')
  sql_order_by <- paste(' ORDER BY higher_plant_group,scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author ')
  
  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  #print(query)
  
  # execute the query
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}
#################
#'Extract taxonomic information for genera
#'
#'BIEN_taxonomy_genus downloads a dataframe of all taxonomic information for given genera.
#' @param genus A single genus or a vector of genera.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing taxonomic information for the specified genera.
#' @examples \dontrun{
#' BIEN_taxonomy_genus("Acer")
#' genus_vector<-c("Acer","Quercus")
#' BIEN_taxonomy_genus(genus_vector)}
#' @family taxonomy functions
BIEN_taxonomy_genus<-function(genus,print.query=FALSE, ...){
  is_char(genus)
  is_log(print.query)

  #set base query components
  sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
    sql_from <- paste(" FROM bien_taxonomy")
  sql_where <- paste(" WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")
  sql_order_by <- paste(" ORDER BY higher_plant_group,scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author ")

  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  #print(query)

  # execute the query
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}


###########
#'Extract taxonomic information for families
#'
#'BIEN_taxonomy_family downloads a dataframe of all taxonomic information for given families.
#' @param family A single family or a vector of families.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing taxonomic information for the specified families.
#' @examples \dontrun{
#' BIEN_taxonomy_family("Orchidaceae")
#' family_vector<-c("Orchidaceae","Poaceae")
#' BIEN_taxonomy_family(family_vector)}
#' @family taxonomy functions
BIEN_taxonomy_family<-function(family,print.query=FALSE, ...){
  is_char(family)
  is_log(print.query)

  #set base query components
  sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
  sql_from <- paste(" FROM bien_taxonomy")
  sql_where <- paste(" WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")
  sql_order_by <- paste(" ORDER BY higher_plant_group,scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author ")

  # form the final query
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, ";")
  #print(query)

  # execute the query
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))

}

################################

###############################
#Phylogeny fxs

#'Download the complete BIEN phylogenies
#'
#'BIEN_phylogeny_complete downloads a specified number of the BIEN phylogeny replicates.
#' @param n_phylogenies The number of phylogenies to download.  Should be an integer between 1 and 100.  Default is 1.
#' @param seed Argument passed to set.seed.  Useful for replicating work with random phylogeny sets.
#' @param replicates The specific replicated phylogenies to return.  Should be a numeric vector of integers between 1 and 100.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A phylo or multiphylo object containing the specified phylogenies
#' @examples \dontrun{
#' phylos<-BIEN_phylogeny_complete(n_phylogenies = 10,seed = 1)
#' phylos<-BIEN_phylogeny_complete(replicates = c(1,2,99,100))}
#' @family phylogeny functions
BIEN_phylogeny_complete<-function(n_phylogenies=1,seed=NULL,replicates=NULL, ...){
  is_num(n_phylogenies)  
  
  
  
  if(!is.null(replicates)){
    replicates<-replicates[which(replicates%in%1:100)]
    query<-paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_complete' AND replicate in (", paste(shQuote(replicates, type = "sh"),collapse = ', '),")"  )
    
    df<-BIEN_sql(query, ...)
    
    tree<-ape::read.tree(text = df$phylogeny,tree.names = df$replicate)
    
    return(tree)

  }

  
  set.seed(seed)  
  
  if(n_phylogenies>100){
    message("n_phylogenies must be an integer between 1 and 100.  Setting n_phylogenies to 100")  
    n_phylogenies<-100  
    
  }
  
  if(n_phylogenies<1){
    message("n_phylogenies must be an integer between 1 and 100.  Setting n_phylogenies to 1")  
    n_phylogenies<-1  
    
  }
  
  phylo_sample<-sample(x = 1:100,size = n_phylogenies,replace = FALSE)
  
  
  query<-paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_complete' AND replicate in (", paste(shQuote(phylo_sample, type = "sh"),collapse = ', '),")"  )
  
  df<-BIEN_sql(query, ...)
  
  tree<-ape::read.tree(text = df$phylogeny,tree.names = df$replicate)
  
  return(tree)
  
}

###############################
#'Download the conservative BIEN phylogeny
#'
#'BIEN_phylogeny_conservative downloads the conservative BIEN phylogeny, which only includes species with molecular data available.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A phylo object containing the BIEN conservative phylogeny
#' @examples \dontrun{
#' BIEN_phylo<-BIEN_phylogeny_conservative()}
#' @family phylogeny functions
BIEN_phylogeny_conservative<-function(...){
  
  query<-paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_conservative' ;"  )
  
  df<-BIEN_sql(query, ...)
  
  tree<-ape::read.tree(text = df$phylogeny,tree.names = df$replicate)
  
  return(tree)
  
}


#################################

#################################
#'Download the current BIEN database version and release date
#'
#'BIEN_metadata_database_version downloads the current version number and release date for the BIEN database.
#' @param ... Additional arguments passed to BIEN_sql
#' @return A data frame containing the current version number and release date for the BIEN database.
#' @family metadata functions
#' @examples \dontrun{
#' BIEN_metadata_database_version()}
BIEN_metadata_database_version<-function(...){
  query<-"SELECT db_version, db_release_date FROM bien_metadata a JOIN (SELECT MAX(bien_metadata_id) as max_id FROM bien_metadata) AS b ON a.bien_metadata_id=b.max_id;"
  BIEN_sql(query, ...)
}


################################

################################
#'Check for differing records between old and new dataframes.
#'
#'BIEN_metadata_match_data compares old and new dataframes, and can check whether they are identical or be used to select rows that are unique to the old or new versions.
#' @param old A dataframe that is to be compared to a (typically) newer dataframe.
#' @param new A dataframe that is to be compared to a (typically) older dataframe.
#' @param return What information should be returned?  Current options are: "identical" (Logical, are the two dataframes identical?), "additions" (numeric, which rows are new?), "deletions" (numeric, which rows are no longer present?), "logical" (logical, which elements of the old dataframe are in the new one?).
#' @param ... Additional arguments passed to BIEN_sql
#' @return Logical of varying length (depending on choice of "return" parameter)
#' @note Since comparisons are done by row (except when using return="identical"), this function may fail to flag additions or deletions if they are exact duplicates of existing rows.
#' @family metadata functions
#' @examples \dontrun{
#' new<-BIEN_occurrence_species("Acer nigrum")
#' old<-new[-1:-4,]#simulate having an older dataset by removing four rows
#' BIEN_metadata_match_data(old,new,return="identical")
#' BIEN_metadata_match_data(old,new,return="additions")}
BIEN_metadata_match_data<-function(old,new,return="identical"){
  if(return %in% c("identical","logical","additions","deletions")){
    
    old<-apply(old,MARGIN = 1,FUN = toString)  
    new<-apply(new,MARGIN = 1,FUN = toString)
    elements<-is.element(new,old)    
    
    if(return=="logical"){
      elements<-is.element(new,old)
      return(elements)  
    }#returns TRUE where elements are in the old set, false where they are not
    
    if(return=="additions"){
      elements<-is.element(new,old)
      return(which(elements==FALSE))  
    }#returns index of new elements
    
    if(return=="deletions"){
      elements<-is.element(old,new)
      return(which(elements==FALSE))  
    }#returns index of deleted elements
    
    if(return=="identical"){
      return(identical(old,new))
    }#returns true if identical, false otherwise
  }else{message("Please specify either 'identical','logical','additions' or 'deletions' for the value of the return argument")}
  
}


################################

###############################
#'Download occurrence points without metadata.
#'
#'.BIEN_occurrence_sdm returns occurrence points withou the recommended metadata and attribution information.
#' @param species A single species or vector of species.
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing species, latitude and longitude.
#' @note We strongly recomend BIEN_occurrence_species as an alternative to this function in most instances.
#' @examples \dontrun{
#' Abies_alba_points<-.BIEN_occurrence_sdm("Abies alba")
#' }

.BIEN_occurrence_sdm<-function(species, cultivated=FALSE, print.query=FALSE,...){
  is_log(cultivated) 
  is_char(species)
  is_log(print.query)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
  }else{
    cultivated_query<-""
  }
  
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial, latitude, longitude 
                 FROM view_full_occurrence_individual WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",paste(cultivated_query),  "
                 AND higher_plant_group IS NOT NULL 
                 AND latitude IS NOT NULL 
                 AND longitude IS NOT NULL
                 AND (is_geovalid = 1 OR is_geovalid IS NULL) 
                 ORDER BY scrubbed_species_binomial;")
  
  # create query to retrieve
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
  
}

############################

############################
#Stem functions


#'Extract stem data for specified species from BIEN
#'
#'BIEN_stem_species downloads occurrence records for specific species from the BIEN database.
#' @param species A single species, or a vector of species.  Genus and species should be separated by a space. Genus should be capitalized.
#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param only.new.world Return only records from the New World?  Default is true
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param print.query Should the PostgreSQL query be printed? The default value is FALSE.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param all.metadata Should additional plot metadata be returned?  Default is FALSE.
#' @param ... Additional arguments passed to BIEN_sql
#' @return Dataframe containing occurrence records for the specified species.
#' @note Setting either "cultivated" or "native.status" to TRUE will significantly slow the speed of a query.
#' @examples \dontrun{
#' BIEN_stem_species("Abies amabilis")
#' species_vector<-c("Abies amabilis", "Acer nigrum")
#' BIEN_stem_species(species_vector)
#' BIEN_stem_species(species_vector,all.taxonomy=TRUE)}
#' @family stem functions
BIEN_stem_species<-function(species,cultivated=FALSE,only.new.world=TRUE,all.taxonomy=FALSE, print.query = FALSE, native.status = FALSE, political.boundaries = FALSE, all.metadata = F, ...){
  is_log(all.metadata)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(all.taxonomy)
  is_char(species)
  is_log(print.query)
  is_log(native.status)
  is_log(political.boundaries)
  
  #set conditions for query
  
  if(!cultivated){
    cultivated_query<-"AND (analytical_stem.is_cultivated = 0 OR analytical_stem.is_cultivated IS NULL)"
    cultivated_select<-""
  }else{
    cultivated_query<-""
    cultivated_select<-",analytical_stem.is_cultivated,view_full_occurrence_individual.is_cultivated_in_region"
  }
  
  if(!only.new.world){
    newworld_query<-""
    newworld_select<-",analytical_stem.is_new_world"
  }else{
    newworld_query<-"AND analytical_stem.is_new_world = 1 "
    newworld_select<-""
  }
  
  if(!all.taxonomy){
    taxon_select<-""
  }else{
    taxon_select<-"analytical_stem.verbatim_family,analytical_stem.verbatim_scientific_name,analytical_stem.family_matched,analytical_stem.name_matched,analytical_stem.name_matched_author,analytical_stem.higher_plant_group,analytical_stem.taxonomic_status,analytical_stem.scrubbed_family,analytical_stem.scrubbed_author,"
  }
  
  if(!native.status){
    native_select<-""
  }else{
    native_select<-"native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  
  if(!political.boundaries){
    political_select<-""
  }else{
    political_select<-"analytical_stem.country,analytical_stem.state_province,analytical_stem.county,analytical_stem.locality,"
  }
  
  if(native.status | cultivated){
    vfoi_join<-" JOIN view_full_occurrence_individual ON (analytical_stem.taxonobservation_id  = view_full_occurrence_individual.taxonobservation_id)"}else{
      vfoi_join<-""  
    }
  
  if(!all.metadata){
    md_select<-""
  }else{
    md_select<-",plot_metadata.methodology_reference,plot_metadata.methodology_description,growth_forms_included_all, growth_forms_included_trees, growth_forms_included_shrubs, growth_forms_included_lianas,
    growth_forms_included_herbs, growth_forms_included_epiphytes, growth_forms_included_notes, taxa_included_all, taxa_included_seed_plants, taxa_included_ferns_lycophytes,
    taxa_included_bryophytes,taxa_included_exclusions"
  }
  
  # set the query
  #query <- paste("SELECT analytical_stem.scrubbed_species_binomial,",taxon_select,native_select,political_select," analytical_stem.latitude, analytical_stem.longitude,analytical_stem.date_collected,plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,analytical_stem.custodial_institution_codes,analytical_stem.collection_code",paste(cultivated_select,newworld_select),"FROM analytical_stem LEFT JOIN plot_metadata ON (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",vfoi_join ," WHERE analytical_stem.scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query),  "AND analytical_stem.higher_plant_group IS NOT NULL AND (analytical_stem.is_geovalid = 1 OR analytical_stem.is_geovalid IS NULL) ORDER BY analytical_stem.scrubbed_species_binomial;")
  
  query <- paste("SELECT analytical_stem.scrubbed_species_binomial,",taxon_select,native_select,political_select," analytical_stem.latitude, analytical_stem.longitude,analytical_stem.date_collected,
                analytical_stem.relative_x_m, analytical_stem.relative_y_m, analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, 
                 plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,analytical_stem.custodial_institution_codes,
                 analytical_stem.collection_code",paste(cultivated_select,newworld_select,md_select),"
                 FROM 
                 (SELECT * FROM analytical_stem WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                 JOIN plot_metadata ON 
                 (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                 vfoi_join ," 
                 WHERE analytical_stem.scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",
                 paste(cultivated_query,newworld_query),  "AND analytical_stem.higher_plant_group IS NOT NULL AND (analytical_stem.is_geovalid = 1 OR analytical_stem.is_geovalid IS NULL)
                 ORDER BY analytical_stem.scrubbed_species_binomial;")
  
  BIEN_sql(query)
  
  #system.time(v1<-BIEN_sql("  SELECT scrubbed_species_binomial FROM (SELECT * FROM analytical_stem WHERE scrubbed_species_binomial in ('Abies lasiocarpa')) a LEFT JOIN plot_metadata ON (a.plot_metadata_id= plot_metadata.plot_metadata_id)  "))
  
  
  
  
  # create query to retrieve
  if(print.query){
    print(query)
  }
  return(BIEN_sql(query, ...))
  
  }


