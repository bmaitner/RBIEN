
#####################

#'Extract occurrence data for specified species from BIEN
#'
#'BIEN_occurrence_species downloads occurrence records for specific species from the BIEN database.
#' @param species A single species, or a vector of species.  Genus and species should be separated by a space. Genus should be capitalized.
#' @param only.geovalid Should the returned records be limited to those with validated coordinates?  Default is TRUE
#' @template occurrence
#' @return Dataframe containing occurrence records for the specified species.
#' @examples \dontrun{
#' BIEN_occurrence_species("Abies amabilis")
#' species_vector<-c("Abies amabilis", "Acer nigrum")
#' BIEN_occurrence_species(species_vector)
#' BIEN_occurrence_species(species_vector,all.taxonomy = TRUE)}
#' @family occurrence functions
#' @export
BIEN_occurrence_species <- function(species,
                                  cultivated = FALSE,
                                  new.world = NULL,
                                  all.taxonomy = FALSE,
                                  native.status = FALSE,
                                  natives.only = TRUE,
                                  observation.type = FALSE,
                                  political.boundaries = FALSE,
                                  collection.info = FALSE,
                                  only.geovalid = TRUE,
                                  ...){
  
  #Test input
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(species)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  .is_log(only.geovalid)
  
  #set conditions for query
  cultivated_<-.cultivated_check(cultivated)  
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)  
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)  
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  geovalid_<-.geovalid_check(only.geovalid)
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial",taxonomy_$select,
                  native_$select,political_$select," ,latitude, longitude,date_collected,
                 datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                 collection_$select,
                 cultivated_$select,
                 newworld_$select,
                 observation_$select,
                 geovalid_$select,"
                 FROM view_full_occurrence_individual 
                 WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",
                 cultivated_$query,
                 newworld_$query,
                 natives_$query,
                 observation_$query,
                 geovalid_$query, "
                 AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                 AND (georef_protocol is NULL OR georef_protocol<>'county centroid') 
                 AND (is_centroid IS NULL OR is_centroid=0) 
                 AND scrubbed_species_binomial IS NOT NULL ;")
  
  return(.BIEN_sql(query, ...))
  
}

##############

#'Extract occurrence data for specified sf polygon
#'
#'BIEN_occurrence_sf downloads occurrence records falling within a user-specified sf polygon
#' @param sf An object of class sf. Note that the projection must be WGS84.
#' @param only.geovalid Should the returned records be limited to those with validated coordinates?  Default is TRUE
#' @template occurrence
#' @return Dataframe containing occurrence records falling within the polygon.
#' @examples \dontrun{
#' library(sf)  
#' 
#' # first, we download an example shapefile to use (a species range)
#' 
#' BIEN_ranges_species("Carnegiea gigantea")#saves range to the current working directory
#' 
#' # load the range map as an sf object
#' 
#' sf <- st_read(dsn = ".",layer = "Carnegiea_gigantea")
#' 
#' # get the occurrences that occur within the polygon.
#' 
#' species_occurrences <- BIEN_occurrence_sf(sf = sf)
#' }
#' @family occurrence functions
#' @importFrom sf st_geometry st_as_text st_bbox
#' @export
BIEN_occurrence_sf <- function(sf,
                               cultivated = FALSE,
                               new.world = NULL,
                               all.taxonomy = FALSE,
                               native.status = FALSE,
                               natives.only = TRUE,
                               observation.type = FALSE,
                               political.boundaries = FALSE,
                               collection.info = FALSE,
                               only.geovalid = TRUE,
                               ...){
  
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  .is_log(only.geovalid)
  
  
  # Convert the sf to wkt (needed for sql query)  
  wkt <- sf |>
    st_geometry() |>
    st_as_text()
  
  # Get bounding box of sf (used as a sort of index to make query a bit faster)
  sf_bbox <-
    sf |>
    st_bbox()
  
  long_min <- sf_bbox["xmin"]
  long_max <- sf_bbox["xmax"]
  lat_min <- sf_bbox["ymin"]
  lat_max <- sf_bbox["ymax"]
  
  
  #set conditions for query
  
  cultivated_ <- .cultivated_check(cultivated)  
  newworld_ <- .newworld_check(new.world)
  taxonomy_ <- .taxonomy_check(all.taxonomy)  
  native_ <- .native_check(native.status)
  observation_ <- .observation_check(observation.type)
  political_ <- .political_check(political.boundaries)  
  natives_ <- .natives_check(natives.only)
  collection_ <- .collection_check(collection.info)
  geovalid_<-.geovalid_check(only.geovalid)
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial",
                 taxonomy_$select,
                 native_$select,
                 political_$select," ,
                  latitude, longitude, date_collected,datasource,dataset,
                 dataowner,custodial_institution_codes,collection_code,
                 a.datasource_id",collection_$select,cultivated_$select,
                 newworld_$select,
                 observation_$select,
                 geovalid_$select,"
                    FROM 
                          (SELECT * FROM view_full_occurrence_individual 
                           WHERE higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                           AND is_geovalid = 1
                           AND (georef_protocol is NULL OR georef_protocol<>'county centroid')
                           AND (is_centroid IS NULL OR is_centroid=0)
                           AND latitude BETWEEN ",lat_min," AND ",lat_max,"AND longitude BETWEEN ",long_min," AND ",long_max,") a 
                    WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),a.geom)",
                      cultivated_$query,
                      newworld_$query,
                      natives_$query,
                      observation_$query,
                      geovalid_$query, "
                      AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')
                      AND (georef_protocol is NULL OR georef_protocol<>'county centroid') 
                      AND (is_centroid IS NULL OR is_centroid=0) 
                      AND scrubbed_species_binomial IS NOT NULL ;")
  
  # create query to retrieve
  df <- .BIEN_sql(query, ...)
  
  
  if(length(df) == 0){
    
    message("No occurrences found")
    return(invisible(NULL))
    
  }else{
    
    return(df)
    
  }
  
}



###############################

#'Extract species list by country
#'
#'BIEN_list_country downloads a list of all species within a country or countries from the BIEN database.
#' @param country A single country or a vector of countries.
#' @param country.code A single country code or a vector of country codes equal in length to the vector of states/province codes.
#' @template list
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @return Dataframe containing species list(s) for the specified country or countries.
#' @examples \dontrun{
#' BIEN_list_country("Canada")
#' country_vector<-c("Canada","United States")
#' BIEN_list_country(country_vector)}
#' @family list functions
#' @export
BIEN_list_country <- function(country = NULL,
                              country.code = NULL,
                              cultivated = FALSE,
                              new.world = NULL,
                              ...){
  
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_char(country)
  .is_char(country.code)
  if(is.null(country) & is.null(country.code)) {
    stop("Please supply either a country name or 2-digit ISO code")
    }
  
  newworld_ <- .newworld_check(new.world)
  
  
  #set base query components
    sql_select <-  paste("SELECT DISTINCT country, scrubbed_species_binomial ")
    
    sql_from <- paste(" FROM species_by_political_division ")
    
    if(is.null(country.code)){
      
      sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")
      
    }else{
      
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND scrubbed_species_binomial IS NOT NULL")  
    }
    

  # adjust for optional parameters
  
  if(!cultivated){
    
    # sql_where <- paste(sql_where, " AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) ")
    
  }else{
    
    sql_select  <- paste(sql_select, ",is_cultivated_in_region")
  }
  
  #if(!new.world){
  #  sql_select <- paste(sql_select,",is_new_world")
  #  sql_where <- paste(sql_where, "AND is_new_world = 1 ")  
  #}else{
  #  sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  #}
  
  
  
  
  # form the final query
  query <- paste(sql_select,newworld_$select, sql_from, sql_where,newworld_$query, " ;")
  
  
  return(.BIEN_sql(query, ...))
  #return(.BIEN_sql(query))
  
  }

############################


#'Extract a species list by state/province
#'
#'BIEN_list_state produces a list of all species with geovalidated occurrences falling within specified state(s) or province(s).
#' @param state A state or vector of states (or other primary political divisions, e.g. provinces).
#' @param country A single country or a vector of countries equal in length to the vector of states/provinces.
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country code or a vector of country codes equal in length to the vector of states/province codes.
#' @template list
#' @return Dataframe containing species list(s) for the specified states/provinces.
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @examples \dontrun{
#' BIEN_list_state("United States","Michigan")
#' state_vector<-c("Michigan","Arizona")
#' BIEN_list_state(country="United States", state= state_vector)}
#' @family list functions
#' @export
BIEN_list_state <- function(country = NULL,
                            country.code = NULL,
                            state = NULL,
                            state.code = NULL,
                            cultivated = FALSE,
                            new.world = NULL,
                            ...){
  .is_char(country)
  .is_char(country.code)
  .is_char(state)
  .is_char(state.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  
  if(is.null(country)& is.null(country.code)) {
    
    stop("Please supply either a country name or 2-digit ISO code")
    
    }  
  
  # set base query components
  
    sql_select <-  paste("SELECT DISTINCT country, state_province, scrubbed_species_binomial ")
    sql_from <- paste(" FROM species_by_political_division ")
  
  #if supplying country names  
  if(is.null(country.code) & is.null(state.code)){  
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
  }else{
    
    if(length(country.code)==1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), ")) 
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country.code)==length(state.code)){
        
        sql_where<-"WHERE ("
        
        for(i in 1:length(country.code)){
          
          condition_i<- paste("country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code[i], type = "sh"),collapse = ', '), ")) 
                              AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code[i], type = "sh"),collapse = ', '), "))")
          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)
          
        }#for i  
        
        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{
        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
        
      }  
      
    }#if length(country>1)

  }  

  # adjust for optional parameters
  if(!cultivated){
    
    #sql_where <- paste(sql_where, " AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) ")
    
  }else{
    
    sql_select  <- paste(sql_select, ",is_cultivated_in_region")

  }
  
  newworld_ <- .newworld_check(new.world)
  
  # form the final query
  query <- paste(sql_select,newworld_$select, sql_from, sql_where,newworld_$query, " ;")
  
  return(.BIEN_sql(query, ...))
  
  }



###########################

#'Extract a species list by county.
#'
#'BIEN_list_county produces a list of all species with geovalidated occurrences falling within specified county or counties.
#' @param country A single country or vector of countries
#' @param state A state or vector of states (or other primary political divisions, e.g. provinces).
#' @param county A single county (or other secondary administrative boundary)or vector of counties.
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country (or other primary administrative boundary) code or a vector of country codes equal in length to the vector of states/province codes.
#' @param county.code A single county (or other secondary administrative boundary) code or a vector of county codes equal in length to the vectors of states/province codes and country codes.
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @note We recommend using country, state, and county rather than codes, since county names have not been fully standardized.
#' @template list
#' @return Dataframe containing species list(s) for the specified states/provinces.
#' @note This function requires you supply either 1) a single state and country with one or more counties, or 2) vectors of equal length for each political level.
#' @examples \dontrun{
#' BIEN_list_county("United States", "Michigan", "Kent")
#' BIEN_list_county(country = "United States", state = "Michigan", county = "Kent")
#' county_vector<-c("Kent","Kalamazoo")
#' BIEN_list_county(country = "United States", state = "Michigan", county = county_vector)}
#' @family list functions
#' @export
BIEN_list_county <- function(country = NULL,
                             state = NULL,
                             county = NULL,
                             country.code = NULL,
                             state.code = NULL,
                             county.code = NULL,
                             cultivated = FALSE,
                             new.world = NULL,
                             ...){

  .is_char(country.code)
  .is_char(state.code)
  .is_char(county.code)
  .is_char(country)
  .is_char(state)
  .is_char(county)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  
  # set base query components
    sql_select <-  paste("SELECT DISTINCT country, state_province, county,  scrubbed_species_binomial ")
    sql_from <- paste(" FROM species_by_political_division ")
  
  if(is.null(country.code) & is.null(state.code) & is.null(county.code)){  
    
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
  }else{ 
    
    #sql where
    if(length(country.code)==1 & length(state.code)==1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), "))
                         AND county in (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country)==length(state) & length(country)==length(county)){
        
        sql_where<-"WHERE ("
        
        for(i in 1:length(country)){
          
          condition_i<- paste("(country = (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                              AND state_province = (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), ")) 
                              AND county = (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))" )
          
          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)
          
        }#for i  
        
        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{
        stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")  
        
      }  
      
      
      
    }#if length(country>1)
    
    
  }
  
  
  # adjust for optional parameters
  if(!cultivated){
    
    #sql_where <- paste(sql_where, " AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) ")
    
  }else{
    
    sql_select  <- paste(sql_select, ",is_cultivated_in_region")
    
  }
  
  #if(!new.world){
  #  sql_select <- paste(sql_select,",is_new_world")
  #}else{
  #  sql_where <- paste(sql_where, "AND is_new_world = 1 ")
  #}
  
  newworld_ <- .newworld_check(new.world)
  
  # form the final query
  query <- paste(sql_select,newworld_$select, sql_from, sql_where,newworld_$query,  " ;")
  
  return(.BIEN_sql(query, ...))
  
  }


###########################

#'Extract a list of all species in the BIEN database.
#'
#'BIEN_list_all produces a list of all species in the BIEN database.
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing a list of all species in the BIEN database.
#' @examples \dontrun{
#' species_list<-BIEN_list_all()}
#' @family list functions
#' @export
BIEN_list_all<-function( ...){
  query <- paste("SELECT species FROM bien_species_all ;")
  
  return(.BIEN_sql(query, ...))
  
}
###########################

#'Extract a list of species within a given sf polygon.
#'
#'BIEN_list_sf produces a list of all species with occurrence records falling within a user-supplied sf object.
#' @param sf An object of class ff.  Note that the object must be in WGS84.
#' @template list
#' @return Dataframe containing a list of all species with occurrences in the supplied sf object.
#' @examples \dontrun{
#' library(sf)
#' 
#' BIEN_ranges_species("Carnegiea gigantea") # saves ranges to the current working directory
#' 
#' sf <- st_read(dsn = ".",
#'               layer = "Carnegiea_gigantea")
#' 
#' species_list <- BIEN_list_sf(sf = sf)
#' }
#' @family list functions
#' @importFrom sf st_geometry st_as_text st_bbox
#' @export
BIEN_list_sf <- function(sf,
                         cultivated = FALSE,
                         new.world = NULL,
                         ...){
  
  .is_log(cultivated)
  .is_log_or_null(new.world)
  
  # Convert the sf to wkt (needed for sql query)  
  wkt <- sf |>
    st_geometry() |>
    st_as_text()
  
  # Get bounding box of sf (used as a sort of index to make query a bit faster)
  sf_bbox <-
    sf |>
    st_bbox()
  
  long_min <- sf_bbox["xmin"]
  long_max <- sf_bbox["xmax"]
  lat_min <- sf_bbox["ymin"]
  lat_max <- sf_bbox["ymax"]  
  
  # adjust for optional parameters
  
  if(!cultivated){
    
    cultivated_query <- "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL)"
    cultivated_select <- ""
  }else{
    
    cultivated_query <- ""
    cultivated_select <- ",is_cultivated_observation,is_cultivated_in_region"
    
  }
  
  newworld_ <- .newworld_check(new.world)
  
  query <- paste("SELECT DISTINCT scrubbed_species_binomial",cultivated_select,newworld_$select ,"
                FROM  
                  (SELECT * FROM view_full_occurrence_individual WHERE higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                  AND is_geovalid = 1 AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
                  AND observation_type IN ('plot','specimen','literature','checklist') 
                  AND scrubbed_species_binomial IS NOT NULL 
                  AND latitude BETWEEN ",lat_min," AND ",lat_max,"AND longitude BETWEEN ",long_min," AND ",long_max,") a
               WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),a.geom)",cultivated_query,newworld_$query ," ;")
  
  # create query to retrieve
  
  df <- .BIEN_sql(query, ...)
  #df <- .BIEN_sql(query)
  
  if(length(df) == 0){
    
    message("No species found")
    return(invisible(NULL))
    
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
#' @template occurrence
#' @return Dataframe containing occurrence records for the specified genera.
#' @examples \dontrun{
#' BIEN_occurrence_genus("Abutilon")
#' genus_vector<-c("Abutilon","Abronia")
#' BIEN_occurrence_genus(genus_vector)
#' BIEN_occurrence_genus(genus = "Abutilon",cultivated = TRUE,new.world = FALSE)}
#' @family occurrence functions
#' @export
BIEN_occurrence_genus <- function(genus,
                                  cultivated = FALSE,
                                  new.world = NULL,
                                  all.taxonomy = FALSE,
                                  native.status = FALSE,
                                  natives.only = TRUE,
                                  observation.type = FALSE,
                                  political.boundaries = FALSE,
                                  collection.info = FALSE,
                                  ...){
  .is_char(genus)
  .is_log(cultivated)
  .is_log(all.taxonomy)
  .is_log_or_null(new.world)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  
  cultivated_<-.cultivated_check(cultivated)  
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)  
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)  
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  
  # set the query
  query <-
    paste("SELECT scrubbed_genus, scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," ,latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,observation_$select, "
          FROM view_full_occurrence_individual 
          WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",cultivated_$query,newworld_$query,natives_$query," 
            AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
            AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
            AND observation_type IN ('plot','specimen','literature','checklist') 
            AND scrubbed_species_binomial IS NOT NULL ;")
  
  return(.BIEN_sql(query, ...))
  
}
############################

#'Extract species occurrences by family.
#'
#'BIEN_occurrence_family extracts all occurrences for a given family (or families) from the BIEN database.
#' @param family A single family or a vector of families.
#' @param only.geovalid Should the returned records be limited to those with validated coordinates?  Default is TRUE
#' @template occurrence
#' @return Dataframe containing occurrence records for the specified family/families.
#' @examples \dontrun{
#' BIEN_occurrence_family("Theaceae")
#' family_vector<-c("Theaceae","Ericaceae")
#' BIEN_occurrence_family(family_vector)}
#' @family occurrence functions
#' @export
BIEN_occurrence_family <- function(family,
                                   cultivated = FALSE,
                                   new.world = NULL,
                                   observation.type = FALSE,
                                   all.taxonomy = FALSE,
                                   native.status = FALSE,
                                   natives.only = TRUE,
                                   political.boundaries = FALSE,
                                   collection.info = FALSE,
                                   only.geovalid = TRUE,
                                   ...){
  
  .is_char(family)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(observation.type)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(only.geovalid)
  
  
  
  #set conditions for query
  cultivated_<-.cultivated_check(cultivated)  
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)  
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)  
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  observation_<-.observation_check(observation.type)
  geovalid_<-.geovalid_check(only.geovalid)
  
  
  
  # set the query
  query <- paste("SELECT scrubbed_family",taxonomy_$select,native_$select,political_$select,",
                    scrubbed_species_binomial, latitude, longitude, date_collected,
                    datasource,dataset, dataowner, custodial_institution_codes,
                    collection_code, view_full_occurrence_individual.datasource_id",
                    collection_$select, cultivated_$select, newworld_$select,
                    observation_$select, geovalid_$select,"
                 FROM view_full_occurrence_individual 
                 WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")",
                 cultivated_$query,newworld_$query,natives_$query,observation_$query, geovalid_$query, " 
                    AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')  
                    AND (georef_protocol is NULL OR georef_protocol<>'county centroid')
                    AND (is_centroid IS NULL OR is_centroid=0) 
                    AND scrubbed_species_binomial IS NOT NULL ;")
  
  return(.BIEN_sql(query, ...))
  
}


#######################

#'Extract species occurrence records by state.
#'
#'BIEN_occurrence_state extracts occurrences records for the specified state(s).
#' @param state A state or vector of states (or other primary political divisions, e.g. provinces).
#' @param country A single country or vector of countries.
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country (or other primary administrative boundary) code or a vector of country codes equal in length to the vector of states/province codes.
#' @template occurrence
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @note This function requires you supply either 1) a single country with one or more states, or 2) vectors of equal length for each political level.
#' @return Dataframe containing occurrence records for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_occurrence_state("United States","Rhode Island")
#' state_vector<-c("Rhode Island","Maryland")
#' BIEN_occurrence_state(country="United States",state=state_vector)}
#' @family occurrence functions
#' @export
BIEN_occurrence_state <- function(country = NULL,
                                  state = NULL,
                                  country.code = NULL,
                                  state.code = NULL,
                                  cultivated = FALSE,
                                  new.world = NULL,
                                  all.taxonomy = FALSE,
                                  native.status = FALSE,
                                  natives.only = TRUE,
                                  observation.type = FALSE,
                                  political.boundaries = FALSE,
                                  collection.info = FALSE,
                                  ...){
  
  .is_char(country)
  .is_char(state)
  .is_char(country.code)
  .is_char(state.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  
  #set conditions for query
    cultivated_ <- .cultivated_check(cultivated)  
    newworld_ <- .newworld_check(new.world)
    taxonomy_ <- .taxonomy_check(all.taxonomy)  
    native_ <- .native_check(native.status)
    observation_ <- .observation_check(observation.type)
    political_ <- .political_check(political.boundaries)  
    natives_ <- .natives_check(natives.only)
    collection_ <- .collection_check(collection.info)
  
  if(is.null(country.code) & is.null(state.code)){  
    
    ##state where
    if(length(country) == 1){
      sql_where <- paste(" WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") 
                         AND state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") 
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country) == length(state)){
        
        sql_where <- "WHERE ("
        
        for(i in 1:length(country)){
          
          condition_i <- paste("(country = ", paste(shQuote(country[i], type = "sh"),collapse = ', '),
                               " AND state_province = ", paste(shQuote(state[i], type = "sh"),collapse = ', '), ")")
          
          if(i != 1){condition_i <- paste("OR ", condition_i)} #stick OR onto the condition where needed
          sql_where <- paste(sql_where, condition_i)
          
        }#for i  
        
        sql_where <- paste(sql_where, ") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{
        
        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
        
      }  
      
    }#if length(country>1)
  }else{
    
    ##state where
    if(length(country.code) == 1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), ")) 
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country.code) == length(state.code)){
        
        sql_where <- "WHERE ("
        
        for(i in 1:length(country.code)){
          
          condition_i <- paste("country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code[i], type = "sh"),collapse = ', '), ")) 
                                AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code[i], type = "sh"),collapse = ', '), "))")
          if(i != 1){condition_i <- paste("OR ",condition_i)}#stick OR onto the condition where needed
          
            sql_where <- paste(sql_where, condition_i)
          
        }#for i  
        
        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{

        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  

      }  
      
    }#if length(country>1)  

  }  

  # set the query
  query <- paste("SELECT scrubbed_species_binomial" ,taxonomy_$select,political_$select, ", latitude, longitude,date_collected,datasource,
                  dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                  collection_$select,cultivated_$select,newworld_$select,native_$select,observation_$select,"
                 FROM view_full_occurrence_individual ",
                  sql_where,cultivated_$query,newworld_$query,natives_$query," 
                  AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
                  AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
                  AND observation_type IN ('plot','specimen','literature','checklist')  
                  AND scrubbed_species_binomial IS NOT NULL ;")
  
  
  return(.BIEN_sql(query, ...))
  
  }


#############################

#'Extract species occurrence records by country.
#'
#'BIEN_occurrence_country extracts occurrences records for the specified country/countries.
#' @param country A single country or a vector of country.
#' @param country.code A single country code or a vector of country codes equal in length to the vector of states/province codes.
#' @template occurrence
#' @param only.geovalid Should the returned records be limited to those with validated coordinates?  Default is TRUE
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @return Dataframe containing occurrence records for the specified country.
#' @examples \dontrun{
#' BIEN_occurrence_country("Cuba")
#' country_vector<-c("Cuba","Bahamas")
#' BIEN_occurrence_country(country_vector)}
#' @family occurrence functions
#' @export
BIEN_occurrence_country <- function(country = NULL,
                                    country.code = NULL,
                                    cultivated = FALSE,
                                    new.world = NULL,
                                    all.taxonomy = FALSE,
                                    native.status = FALSE,
                                    natives.only = TRUE,
                                    observation.type = FALSE,
                                    political.boundaries = FALSE,
                                    collection.info = FALSE,
                                    only.geovalid = TRUE,
                                    ...){

  .is_char(country)
  .is_char(country.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(only.geovalid)
  
  if(is.null(country)& is.null(country.code)) {
    stop("Please supply either a country or 2-digit ISO code")
    }
  
  #set conditions for query
    
    cultivated_ <- .cultivated_check(cultivated)  
    newworld_ <- .newworld_check(new.world)
    taxonomy_ <- .taxonomy_check(all.taxonomy)  
    native_ <- .native_check(native.status)
    observation_ <- .observation_check(observation.type)
    political_ <- .political_check(political.boundaries)  
    natives_ <- .natives_check(natives.only)
    collection_ <- .collection_check(collection.info)
    geovalid_<-.geovalid_check(only.geovalid)
    
  
  
  # set the query
  
  
  if(is.null(country.code)){
      query <- paste("SELECT scrubbed_species_binomial",taxonomy_$select,
                      political_$select, native_$select,", latitude,
                      longitude,date_collected, datasource, dataset ,dataowner,
                      custodial_institution_codes,collection_code,
                      view_full_occurrence_individual.datasource_id",
                      collection_$select, cultivated_$select,
                      newworld_$select, observation_$select, geovalid_$select, "
                    FROM view_full_occurrence_individual
                    WHERE country in
                    (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",
                    cultivated_$query,newworld_$query,natives_$query,
                    geovalid_$query," 
                    AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')
                    AND (georef_protocol is NULL OR georef_protocol<>'county centroid')
                    AND (is_centroid IS NULL OR is_centroid=0)",
                    observation_$query,";")
  
  }else{
    query <- paste("SELECT scrubbed_species_binomial",taxonomy_$select,political_$select,native_$select,", latitude, longitude, 
                      date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,
                      view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,observation_$select,"
                   FROM view_full_occurrence_individual 
                   WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                      ",cultivated_$query,newworld_$query,natives_$query," 
                      AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
                      AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0)",
                      observation_$query, 
                      "AND scrubbed_species_binomial IS NOT NULL ;")
    
    
  }
  
  
  return(.BIEN_sql(query, ...))
  
}

##############################

#'Extract species occurrence records by county.
#'
#'BIEN_occurrence_county extracts occurrences records for the specified county or counties.
#' @param country A single country or vector of countries.
#' @param state A state or vector of states (or other primary political divisions, e.g. provinces).
#' @param county A single county or a vector of counties (or other secondary political division, e.g. parish).
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country (or other primary administrative boundary) code or a vector of country codes equal in length to the vector of states/province codes.
#' @param county.code A single county (or other secondary administrative boundary) code or a vector of county codes equal in length to the vectors of states/province codes and country codes.
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @template occurrence
#' @note This function requires you supply either 1) a single country with one or more states, or 2) vectors of equal length for each political level.
#' @return Dataframe containing occurrence records for the specified states/provinces.
#' @examples \dontrun{
#' BIEN_occurrence_county("United States","Arizona","Pima")
#' country_vector<-c("United States","United States")
#' state_vector<-c("Arizona","Michigan")
#' county_vector<-c("Pima","Kent")
#' BIEN_occurrence_county(country=country_vector, state = state_vector, county = county_vector)}
#' @family occurrence functions
#' @export
BIEN_occurrence_county <- function(country = NULL,
                                   state = NULL,
                                   county = NULL,
                                   country.code = NULL,
                                   state.code = NULL,
                                   county.code = NULL,
                                   cultivated = FALSE,
                                   new.world = NULL,
                                   all.taxonomy = FALSE,
                                   native.status = FALSE,
                                   natives.only = TRUE,
                                   observation.type = FALSE,
                                   political.boundaries = FALSE,
                                   collection.info = FALSE,
                                   ...){

  .is_char(country)
  .is_char(state)
  .is_char(county)
  .is_char(country.code)
  .is_char(state.code)
  .is_char(county.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(collection.info)
  
  #set conditions for query
  cultivated_<-.cultivated_check(cultivated)  
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)  
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)  
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  
  if(is.null(country.code) & is.null(state.code) & is.null(county.code)){    
    
    #sql where
    if(length(country) ==1 & length(state) == 1){
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
    
  }else{
    
    #sql where
    if(length(country.code) == 1 & length(state.code) == 1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), "))
                         AND county in (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country) == length(state) & length(country) == length(county)){
        
        sql_where <- "WHERE ("
        
        for(i in 1:length(country)){
          
          condition_i <- paste("(country = (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                              AND state_province = (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), ")) 
                              AND county = (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", paste(shQuote(county.code, type = "sh"),collapse = ', '), "))" )
          
          if(i != 1){condition_i <- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)
          
        }#for i  
        
        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{
        stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")  
        
      }  

    }#if length(country>1)

  }#if codes are not null  

  # set the query
  query <- paste("SELECT scrubbed_species_binomial" ,taxonomy_$select,political_$select , ",latitude, longitude,date_collected,datasource,
                    dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                    collection_$select,cultivated_$select,newworld_$select,native_$select,observation_$select,"
                  FROM view_full_occurrence_individual ",
                    sql_where,cultivated_$query,newworld_$query,natives_$query," 
                    AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
                    AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
                    AND observation_type IN ('plot','specimen','literature','checklist')  
                    AND scrubbed_species_binomial IS NOT NULL ;")

  return(.BIEN_sql(query, ...))
  
  }


############################
#'Extract species occurrence records by a latitude/longitude bounding box.
#'
#'BIEN_occurrence_box extracts occurrences records falling within the specific area.
#' @param min.lat Minimum latitude
#' @param max.lat Maximum latitude
#' @param min.long Minimum longitude
#' @param max.long Maximum longitude
#' @param species Optional.  A single species or a vector of species.
#' @param genus Optional. A single genus or a vector of genera.
#' @template occurrence
#' @return Dataframe containing occurrence records for the specified area.
#' @note Specifying species and/or genera will limit records returned to that set of taxa.
#' @examples \dontrun{
#' output_test<-
#' BIEN_occurrence_box(min.lat = 32,max.lat = 33,min.long = -114,max.long = -113,
#' cultivated = TRUE, new.world = FALSE)}
#' @family occurrence functions
#' @export
BIEN_occurrence_box<-function(min.lat,
                              max.lat,
                              min.long,
                              max.long,
                              species = NULL,
                              genus = NULL,
                              cultivated = FALSE,
                              new.world = NULL,
                              all.taxonomy = FALSE,
                              native.status = FALSE,
                              natives.only = TRUE,
                              observation.type = FALSE,
                              political.boundaries = TRUE,
                              collection.info = FALSE,
                              ...){

  .is_num(min.lat)
  .is_num(max.lat)
  .is_num(min.long)
  .is_num(max.long)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(observation.type)
  .is_log(collection.info)
  .is_char(species)
  .is_char(genus)
  
  #set conditions for query
  cultivated_ <- .cultivated_check(cultivated)  
  newworld_ <- .newworld_check(new.world)
  taxonomy_ <- .taxonomy_check(all.taxonomy)  
  native_ <- .native_check(native.status)
  observation_ <- .observation_check(observation.type)
  political_ <- .political_check(political.boundaries)  
  natives_ <- .natives_check(natives.only)
  collection_ <- .collection_check(collection.info)
  species_ <- .species_check(species)
  genus_ <- .genus_check(genus)  
  
  
  # set the query
  query <- paste("SELECT scrubbed_species_binomial", taxonomy_$select,political_$select,native_$select,",latitude, longitude, 
                    date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                    collection_$select,cultivated_$select,newworld_$select,observation_$select,"
                 FROM view_full_occurrence_individual 
                 WHERE latitude between " , paste(shQuote(min.lat, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.lat, type = "sh"),collapse = ', '),"
                  AND longitude between ", paste(shQuote(min.long, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.long, type = "sh"),collapse = ', '), 
                  cultivated_$query,newworld_$query,natives_$query, species_$query, genus_$query  ,  "
                  AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 AND (georef_protocol is NULL OR georef_protocol<>'county centroid') 
                  AND (is_centroid IS NULL OR is_centroid=0) AND observation_type IN ('plot','specimen','literature','checklist') 
                  AND scrubbed_species_binomial IS NOT NULL ;")
  
  return(.BIEN_sql(query, ...))
  
}

#####

#'Download range maps for given species.
#'
#'BIEN_ranges_species extracts range maps for the specified species.
#' @param species A single species or a vector of species.
#' @template ranges
#' @return Range maps for specified species.
#' @examples \dontrun{
#' library(sf)
#' library(maps) #a convenient source of maps
#' species_vector <- c("Abies_lasiocarpa","Abies_amabilis")
#' BIEN_ranges_species(species_vector)
#' BIEN_ranges_species(species_vector, match_names_only = TRUE)
#' temp_dir <- file.path(tempdir(), "BIEN_temp")#Set a working directory
#' BIEN_ranges_species(species = species_vector,
#'                     directory = temp_dir)#saves ranges to a temporary directory
#' BIEN_ranges_species("Abies_lasiocarpa")
#' BIEN_ranges_species("Abies_lasiocarpa",
#'                     directory = temp_dir)
#' 
#' #Reading files
#' 
#' Abies_poly <- st_read(dsn = temp_dir,
#'                       layer = "Abies_lasiocarpa")
#' 
#' #Plotting files
#' plot(Abies_poly[1])#plots the range, but doesn't mean much without any reference
#' map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#' 
#' plot(Abies_poly[1],
#'      col = "forest green",
#'      add = TRUE) #adds the range of Abies lasiocarpa to the map
#' 
#' # Getting data from the files (currently only species names and a BIEN ID field)
#' Abies_poly$species#gives the species name associated with "Abies_poly"}#'
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
BIEN_ranges_species <- function(species,
                                directory = NULL,
                                matched = TRUE,
                                match_names_only = FALSE,
                                include.gid = FALSE,
                                ...){
  
  .is_char(species)
  .is_log(matched)
  .is_log(match_names_only)
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  if(match_names_only == FALSE){
    
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory <- getwd()
    }
    
    
    # set the query
    query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ;")
    
    # create query to retrieve
    df <- .BIEN_sql(query, ...)
    #df <- .BIEN_sql(query)
    
    if(length(df) == 0){
      
      message("No species matched")
      
    }else{
      
      for(l in 1:length(df$species)){
        
        sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                                 wkt = "st_astext",
                                 crs = "epsg:4326")
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        # if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
        #   directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        # }
        
        if(include.gid == TRUE){
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l],"_",df$gid[l],sep=""),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
          
        }else{
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l]),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
        }
        
        #save output
        
      }#for species in df loop
    }#else
    
    #list matched species
    if(matched == TRUE){
      found <- as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found) <- c("Species","Range_map_downloaded?")
      found$`Range_map_downloaded?` <- as.character(found$`Range_map_downloaded?`)
      found$`Range_map_downloaded?`[which(species%in%df$species)] <- "Yes"
      return(found)
    }#matched = true
  }#match names only if statement
  
  if(match_names_only == TRUE){
    
    rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ;")
    
    # create query to retrieve
    df <- .BIEN_sql(rangeQuery, ...)
    #df <- .BIEN_sql(rangeQuery)
    
    if(length(df) == 0){
      
      message("No species matched")
      
    }else{
      found <- as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found) <- c("Species","Range_map_available?")
      found$`Range_map_available?` <- as.character(found$`Range_map_available?`)
      found$`Range_map_available?`[which(species%in%df$species)] <- "Yes"
      return(found)
      
    }
    
  } #matched_names_only == TRUE
}

####################################

#'Extract range data for large numbers of species
#'
#'BIEN_ranges_species_bulk downloads ranges for a large number of species using parallel processing.
#' @param species A vector of species or NULL (the default).  If NULL, all available ranges will be used.
#' @param directory The directory where range shapefiles will be stored.  If NULL, a temporary directory will be used.
#' @param batch_size The number of ranges to download at once.
#' @param return_directory Should the directory be returned? Default is TRUE
#' @param use_parallel Logical.  Should batches be downloaded in parallel?  If set to TRUE, AND if parallel and foreach are available, parallel processing of downloads will use n-1 clusters.
#' @return Optionally, the directory to which the files were saved.
#' @note This function may take a long time (hours) to run depending on the number of cores, download speed, etc.
#' @examples \dontrun{
#' #To download all BIEN ranges maps:
#' BIEN_ranges_species_bulk()
#' }
#' @family range functions
#' @export
#' @import foreach
#' @import doParallel
BIEN_ranges_species_bulk <- function(species = NULL,
                                     directory = NULL,
                                     batch_size = 1000,
                                     return_directory = TRUE,
                                     use_parallel = FALSE){
  
  #Set species list and directory if NULL
  
  if(is.null(species)){ species <- BIEN_ranges_list()$species }  
  
  if(is.null(directory)){directory <- file.path(tempdir(), "BIEN_temp")
  print(paste("Files will be saved to ",directory))}  
  
  if(!file.exists(directory)){
    dir.create(directory)
  }
  
  
  
  if(nzchar(system.file(package = "doParallel"))  & nzchar(system.file(package = "foreach")) & use_parallel){
    
    
    #Download range maps
    cl <- parallel::makePSOCKcluster(parallel::detectCores())
    
    doParallel::registerDoParallel(cl = cl,
                                   cores = parallel::detectCores() - 1)
    
    foreach::foreach(i = 1:ceiling(length(species)/batch_size  )) %dopar%
      
      BIEN_ranges_species(species = species[(((i-1)*batch_size)+1):(i*batch_size)],
                          directory = file.path(directory,i),
                          matched = FALSE)
    
    parallel::stopCluster(cl)
    rm(cl)
    
  }else{
    
    
    for(i in 1:ceiling(length(species)/batch_size  )){
      
      BIEN_ranges_species(species = species[(((i-1)*batch_size)+1):(i*batch_size)],
                          directory = file.path(directory,i),
                          matched = FALSE)
      
    }
    
  }
  
  
  
  
  if(return_directory){return(directory)}
  
}#end fx



####################################
#'Download range maps for given genus.
#'
#'BIEN_ranges_genus extracts range maps for the specified genera.
#' @param genus A single genus or a vector of genera.
#' @template ranges
#' @return Range maps for all available species within the specified genera.
#' @examples \dontrun{
#' library(maps)
#' library(sf)
#' 
#' genus_vector <- c("Abies","Acer")
#' 
#' temp_dir <- file.path(tempdir(), "BIEN_temp")#Set a working directory
#' 
#' BIEN_ranges_genus(genus_vector)
#' 
#' BIEN_ranges_genus(genus = genus_vector,
#'                   match_names_only = TRUE)
#' 
#' BIEN_ranges_genus(genus = genus_vector,
#'                   directory = temp_dir) #saves ranges to a specified working directory
#' 
#' BIEN_ranges_genus("Abies")
#' 
#' BIEN_ranges_genus(genus = "Abies",
#'                   directory = temp_dir)
#' 
#' #Reading files
#' 
#' Abies_poly <- read_sf(dsn = temp_dir,layer = "Abies_lasiocarpa")
#' 
#' #Plotting files
#' 
#' plot(Abies_poly[1]) #plots the range, but doesn't mean much without any reference
#' 
#' map('world', fill = TRUE, col = "grey") #plots a world map (WGS84 projection), in grey
#' 
#' plot(Abies_poly[1],
#'      col="forest green",
#'      add = TRUE) #adds the range of Abies lasiocarpa to the map
#' 
#' # Getting data from the files (currently only species names)
#' 
#' Abies_poly$species#gives the species name associated with "Abies_poly"
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
BIEN_ranges_genus <- function(genus,
                              directory = NULL,
                              matched = TRUE,
                              match_names_only = FALSE,
                              include.gid = FALSE,
                              ...){
  
  .is_char(genus)
  .is_log(matched)
  .is_log(match_names_only)
  .is_log(include.gid)
  
  #modify the genus list to make searching easier
    genus <- paste("(",genus,"_",")",sep = "")
  
  if(match_names_only == FALSE){
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory <- getwd()
    }
    
    
    
    # set the query
    query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species ~ '",paste(genus,collapse="|"),"' ;",sep="")
    
    # create query to retrieve
    df <- .BIEN_sql(query, ...)
    #df <- .BIEN_sql(query)
    
    if(length(df) == 0){
      message("No species matched")
    }else{
      
      for(l in 1:length(df$species)){
        
        sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                             wkt = "st_astext",
                             crs = "epsg:4326")
        
        #Make sure that the directory doesn't have a "/" at the end-this confuses rgdal.  Probably a more eloquent way to do this with regex...
        # if(unlist(strsplit(directory,""))[length(unlist(strsplit(directory,"")))]=="/"){
        #   directory<-paste(unlist(strsplit(directory,""))[-length(unlist(strsplit(directory,"")))],collapse = "")
        # }
        
        if(include.gid == TRUE){
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l],"_",df$gid[l],sep=""),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
          
        }else{
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l]),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
        }
        
        #save output
        
      }#for species in df loop
    }#else
    
    #list matched species
    if(matched == TRUE){
      
      found <- as.data.frame(df$species)
      return(found)
      
    }#matched = true
    
  }#match names only if statement
  
  if(match_names_only == TRUE){
    
    query <- paste("SELECT species FROM ranges WHERE species ~ '",paste(genus,collapse="|"),"' ;",sep="")
    
    # create query to retrieve
    df <- .BIEN_sql(query, ...)
    
    if(length(df) == 0){
      
      message("No species matched")
      
    }else{
      
      found<-as.data.frame(df$species)
      return(found)
      
    }
    
  } #matched_names_only == TRUE
  
}

#######################################
#'Download range maps that intersect a specified bounding box.
#'
#'BIEN_ranges_box extracts range maps for a specified bounding box.
#' @param min.lat Minimum latitude of the ranges included.
#' @param max.lat Maximum latitude of the ranges included.
#' @param min.long Minimum longitude of the ranges included.
#' @param max.long Maximum longitude of the ranges included.
#' @param crop.ranges Should the ranges be cropped to the focal area? Default is FALSE.
#' @template ranges_spatial
#' @return Range maps for all available species within the specified bounding box.
#' @examples \dontrun{
#' temp_dir <- file.path(tempdir(), "BIEN_temp") #Set a working directory
#' BIEN_ranges_box(42,43,-85,-84,species.names.only = TRUE)
#' BIEN_ranges_box(42,43,-85,-84,directory = temp_dir)}
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
BIEN_ranges_box <- function(min.lat,
                            max.lat,
                            min.long,
                            max.long,
                            directory = NULL,
                            species.names.only = FALSE,
                            return.species.list = TRUE ,
                            crop.ranges = FALSE,
                            include.gid = FALSE,
                            ...){
  
  .is_num(min.lat)
  .is_num(max.lat)
  .is_num(min.long)
  .is_num(max.long)
  .is_log(include.gid)
  .is_log(return.species.list)
  .is_log(species.names.only)
  
  if(species.names.only == FALSE){
    
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory <- getwd()
    }    
    
    # set the query
    
    if(crop.ranges){
      
      query <- paste("SELECT ST_AsText(ST_intersection(geom,ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326))),species,gid FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)") 
      
    }else{
      
      query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)")
      
    }
    
    # create query to retrieve
    
    df <- .BIEN_sql(query, ...)
    #df <- .BIEN_sql(query)
    
    if(nrow(df) == 0){
      
      message("No species matched")
      
    }else{
      
      for(l in 1:length(df$species)){
        
        sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                             wkt = "st_astext",
                             crs = "epsg:4326")
        
        if(include.gid == TRUE){
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l],"_",df$gid[l],sep=""),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
          
        }else{
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l]),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
        }
        
        #save output
        
      }#for species in df loop
      
      if(return.species.list){
        return(df$species)  
      }#if return.species.list
      
    }#else
    
  }#species names only if statement
  
  if(species.names.only == TRUE){
    
    # create query to retrieve
    query<-paste("SELECT species FROM ranges WHERE st_intersects(ST_MakeEnvelope(",min.long, ",",min.lat,",",max.long,",",max.lat,",4326),geom)")  
    
    df <- .BIEN_sql(query, ...)
    
    if(length(df) == 0){
      
      message("No species found")
      
    }else{
      
      return(df)
      
    }
    
  } #species.names.only == TRUE
}


#######################################
#'Download range maps that intersect the range of a given species.
#'
#'BIEN_ranges_intersect_species extracts range maps for a specified bounding box.
#' @param species Focal species (or a vector of species) for which to extract intersecting ranges.
#' @param include.focal Should a range for the focal species be downloaded? Default is TRUE.
#' @template ranges_spatial
#' @return Range maps for all available species that intersect the range of the focal species.
#' @examples \dontrun{
#' temp_dir <- file.path(tempdir(), "BIEN_temp") #Set a working directory
#' BIEN_ranges_intersect_species(species = "Carnegiea_gigantea",
#' directory = temp_dir,include.focal = TRUE)
#' species_vector<-c("Carnegiea_gigantea","Echinocereus coccineus")
#' BIEN_ranges_intersect_species(species = species_vector,species.names.only = TRUE)
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
BIEN_ranges_intersect_species <- function(species,
                                          directory = NULL,
                                          species.names.only = FALSE,
                                          include.focal = TRUE,
                                          return.species.list = TRUE,
                                          include.gid = FALSE,
                                          ...){
  
  .is_char(species)
  .is_log(species.names.only)
  .is_log(include.focal)
  .is_log(include.gid)
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  #set query chunk to include focal species
  
  if(include.focal){
    focal.query <- ""  
  }else{
    focal.query <- "a.species != b.species AND" 
  }
  
  if(species.names.only == FALSE){
    
    #set directory for saving
    
    if(is.null(directory)){
      directory <- getwd()
    }  
    
    # set the query
    query <- paste("SELECT b.species AS focal_species, a.species 
                   AS intersecting_species,a.species,a.gid, ST_AsText(a.geom) 
                   AS geom 
                   FROM ranges AS a, (SELECT species, geom 
                                      FROM ranges 
                                      WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE", focal.query," ST_Intersects(a.geom, b.geom) ;")  
    
    # create query to retrieve
    df <- .BIEN_sql(query, ...)
    #df <- .BIEN_sql(query)
    #df <- .BIEN_sql(query,limit = limit)
    
    if(length(df) == 0){
      
      message("No species matched")
      
    }else{
      
      for(l in 1:nrow(df)){
        
        sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                             wkt = "geom",
                             crs = "epsg:4326")
        
        if(include.gid == TRUE){
          
          suppressWarnings(
            st_write(obj = sp_range,
                     dsn = directory,
                     layer = paste(df$species[l],"_",df$gid[l],sep=""),
                     driver = "ESRI Shapefile",
                     append = FALSE,
                     quiet=TRUE))
          
          
        }else{
          
          suppressWarnings(
            st_write(obj = sp_range,
                     dsn = directory,
                     layer = paste(df$species[l]),
                     driver = "ESRI Shapefile",
                     append = FALSE,
                     quiet=TRUE))
          
        }
        
        #save output
        
      }#for species in df loop
      
      if(return.species.list){
        
        return(df$species)
        
      }#if return.species.list
      
    }#else
    
  }#species names only if statement
  
  if(species.names.only == TRUE){
    
    query <- paste("SELECT b.species AS focal_species, a.species AS intersecting_species FROM ranges AS a, (SELECT species, geom FROM ranges WHERE species in (",paste(shQuote(species, type = "sh"),collapse = ', '),")) b WHERE", focal.query," ST_Intersects(a.geom, b.geom) ;")  
    
    
    # create query to retrieve
    
    df <- .BIEN_sql(query, ...)
    
    if(length(df) == 0){
      
      message("No species found")
      
    }else{
      return(df)
      
    }
    
  } #species.names.only == TRUE
}

#######################################
#'Download range maps that intersect a user-supplied sf object.
#'
#'BIEN_ranges_sf extracts range maps that intersect a specified simple features (sf) object.
#' @param sf An object of class sf.
#' @param crop.ranges Should the ranges be cropped to the focal area? Default is FALSE.
#' @template ranges_spatial
#' @return All range maps that intersect the user-supplied sf object.
#' @examples \dontrun{
#' 
#' # Here we use a range map as our example polygon
#' 
#' BIEN_ranges_species("Carnegiea gigantea") #saves ranges to the current working directory
#' 
#' # Read in the polygon  with sf   
#' sf <- sf::st_read(dsn = ".",
#'                   layer = "Carnegiea_gigantea")
#' 
#' BIEN_ranges_sf(sf = sf,
#'                limit = 10) 
#'                # We use the limit argument to return only 10 range maps.
#'                # Omit the limit argument to get all ranges
#' 
#' #Note that this will save many shapefiles to the working directory.
#' }
#' @family range functions
#' @importFrom sf st_geometry st_as_text st_as_sf st_write
#' @export
BIEN_ranges_sf <- function(sf,
                           directory = NULL,
                           species.names.only = FALSE,
                           return.species.list = TRUE,
                           crop.ranges = FALSE,
                           include.gid = FALSE,
                           ...){
  
  .is_log(return.species.list)
  .is_log(species.names.only)
  .is_log(crop.ranges)
  .is_log(include.gid)
  
  wkt <- sf |>
    st_geometry() |>
    st_as_text()
  
  if(species.names.only == FALSE){
    
    #set directory for saving
    if(is.null(directory)){
      directory <- getwd()
    }  
    
    # set the query
    if(crop.ranges){
      
      query <- paste("SELECT ST_AsText(ST_intersection(geom,ST_GeographyFromText('SRID=4326;",paste(wkt),"'))),species,gid FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom) ;") 
      
    }else{
      
      query <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom) ;")  
      
    }
    
    # create query to retrieve
    
    df <- .BIEN_sql(query, ...)
    
    
    if(length(df) == 0){
      
      message("No species matched")
      
    }else{
      
      for(l in 1:length(df$species)){
        
        sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                             wkt = "st_astext",
                             crs = "epsg:4326")
        
        if(include.gid == TRUE){
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l],"_",df$gid[l],sep=""),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
          
        }else{
          
          st_write(obj = sp_range,
                   dsn = directory,
                   layer = paste(df$species[l]),
                   driver = "ESRI Shapefile",
                   append = FALSE,
                   quiet=TRUE)
          
        }
        
        #save output
        
      }#for species in df loop
    }#else
    
  }#species names only if statement
  
  if(species.names.only == TRUE){
    
    query <- paste("SELECT species FROM ranges WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom) ;")  
    
    # create query to retrieve
    df <- .BIEN_sql(query, ...)
    
    if(length(df) == 0){
      message("No species found")
    }else{
      return(df)
      
    }
    
  } #species.names.only == TRUE
}

#######################################
#'Load range maps for specified species.
#'
#'BIEN_ranges_load_species returns spatial data for the specified species.
#' @param species A single species or a vector of species.
#' @param ... Additional arguments passed to internal functions.
#' @return A sf containing range maps for the specified species.
#' @examples \dontrun{
#' library(maps)
#' species_vector<-c("Abies_lasiocarpa","Abies_amabilis")
#' abies_maps <- BIEN_ranges_load_species(species = species_vector)
#' xanthium_strumarium <- BIEN_ranges_load_species(species = "Xanthium strumarium")
#' 
#' #Plotting files
#' plot(abies_maps) # plots the sf, but doesn't mean much without any reference
#' map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#' plot(xanthium_strumarium,col="forest green",add = TRUE) #adds the range of X. strumarium
#' plot(abies_maps[1,], add = TRUE, col ="light green")
#' }
#' @family range functions
#' @importFrom sf st_as_sf
#' @export
BIEN_ranges_load_species <- function(species,
                                     ...){
  
  .is_char(species)
  
  #make sure there are no spaces in the species names
  
  species <- gsub(" ","_",species)
  
  # set the query
  
  query <- paste("SELECT ST_AsText(geom) as geometry,species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ;")
  
  # create query to retrieve
  df <- .BIEN_sql(query, ...)
  #df <- .BIEN_sql(query)
  
  if(length(df) == 0){
    
    message("No species matched")
    return(invisible(NULL))
    
  }else{
    
    poly <- st_as_sf(x = df,
                     wkt = "geometry",
                     crs = "epsg:4326")
    
    return(poly) 
    
  }#else
  
  
}


###############################
#'List available range maps
#'
#'BIEN_ranges_list a data.frame containing listing all range maps currently available.
#' @param ... Additional arguments passed to internal functions.
#' @return A data.frame containing the available species and their associated GIDs.
#' @examples \dontrun{
#' available_maps<-BIEN_ranges_list()}
#' @family range functions
#' @family metadata functions
#' @export
BIEN_ranges_list <- function( ...){
  
  # set the query
  query <- paste("SELECT species,gid FROM ranges ;")
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
}

########################################

#'Extract range data and convert to smaller "skinny" format
#'
#'BIEN_ranges_shapefile_to_skinny converts ranges to a "skinny" format to save space.
#' @param directory The directory where range shapefiles will be stored.  If NULL, a temporary directory will be used.
#' @param raster A raster (which must have a CRS specified) to be used for rasterizing the ranges.
#' @param skinny_ranges_file A filename that will be used to write the skinny ranges will be written to (RDS format).  If NULL, this will not be written.
#' @return Matrix containing 2 columns: 1) Species name; and 2) the raster cell number it occurs within.
#' @examples \dontrun{
#' BIEN_ranges_shapefile_to_skinny(directory = BIEN_ranges_species_bulk(species = c("Acer rubrum")),
#' raster = terra::rast(crs = "+proj=laea +lat_0=15 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 
#' +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
#'            extent = terra::ext(c(-5261554,5038446,-7434988,7165012 )),
#'            resolution =c(100000,100000)
#'            )
#' )
#' }
#' @family range functions
#' @importFrom sf read_sf st_transform st_crs
#' @importFrom terra rasterize
#' @importFrom terra rast
#' @importFrom terra values
#' @export
BIEN_ranges_shapefile_to_skinny <- function(directory,
                                            raster,
                                            skinny_ranges_file = NULL){
  
  
  range_maps <- list.files(path = directory,
                           pattern = ".shp",
                           full.names = TRUE,
                           recursive = TRUE)
  
  skinny_occurrences <- NULL
  
  raster <- rast(raster)
  
  for(i in range_maps){
    
    #print(i)
    raster_i <- read_sf(i) |>
      st_transform(crs = st_crs(raster)) |>
      rasterize(y = raster)
    
    if(length(which(values(raster_i) > 0)) > 0){
      skinny_occurrences <- rbind(skinny_occurrences,
                                  cbind(read_sf(i)$species,
                                        which(values(raster_i) > 0)))
    }#end if statement
  }#end i loop
  
  
  
  #Save skinny occurrences if filename specified
  
  if(!is.null(skinny_ranges_file)){
    saveRDS(object = skinny_occurrences,
            file = skinny_ranges_file)  
  }
  
  #return skinny occurrences
  return(skinny_occurrences)
  
}#end fx

########################################

#'Build a richness raster from a skinny range file
#'
#'BIEN_ranges_skinny_ranges_to_richness_raster takes in "skinny" range data and converts it to a richness raster.
#' @param skinny_ranges A matrix output by the function "BIEN_ranges_skinny" or equivalent methods.
#' @param raster The raster that was used in building the skinny_ranges matrix.
#' @return Raster
#' @examples \dontrun{
#' 
#' 
#' template_raster <- terra::rast(
#'   crs = "+proj=laea +lat_0=15 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 
#'   +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
#'   ext = ext(c(-5261554, 5038446, -7434988, 7165012 )),
#'   resolution =  c(100000, 100000))
#' 
#' #Download ranges and convert to a "skinny" format
#' skinny_ranges <- BIEN_ranges_shapefile_to_skinny(
#' directory = BIEN_ranges_species_bulk(species = c("Acer rubrum"), 
#' raster = template_raster)
#' 
#' #Convert from skinny format to richness raster                                                 
#' richness_raster<- BIEN_ranges_skinny_ranges_to_richness_raster(
#'  skinny_ranges = skinny_ranges,raster = template_raster)
#'  
#'  plot(richness_raster)
#' }
#' @family range functions
#' @export
#' @importFrom terra values
BIEN_ranges_skinny_ranges_to_richness_raster <- function(skinny_ranges,
                                                         raster){
  
  #Create empty output raster
    output_raster <- raster
    terra::values(output_raster) <- NA #note that if this "terra::" is omitted, cran checks raise a note
  
  #iterate through all cells with at least one occurrence, record 
  
  output_raster[as.numeric(unique(skinny_ranges[,2]))] <-
    sapply(X = unique(skinny_ranges[,2]),
           FUN = function(x){ length(unique(skinny_ranges[which(skinny_ranges[,2] == x), 1]))} )
  
  #return output raster  
  
  return(output_raster)
  
  
}


########################################
########################################

#'Download trait data for given species.
#'
#'BIEN_trait_species extracts trait data for the species specified.
#' @param species A single species or a vector of species.
#' @template trait
#' @return A dataframe of all available trait data for the given species.
#' @examples \dontrun{
#' BIEN_trait_species("Poa annua")
#' species_vector<-c("Poa annua","Juncus trifidus")
#' BIEN_trait_species(species_vector)}
#' @family trait functions
#' @export
BIEN_trait_species <- function(species,
                               all.taxonomy = FALSE,
                               political.boundaries = FALSE,
                               source.citation = FALSE,
                               ...){

  .is_char(species)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  
  # set the query
  taxonomy_ <- .taxonomy_check_traits(all.taxonomy)  
  political_ <- .political_check_traits(political.boundaries)  
  source_ <- .source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id 
                 FROM agg_traits WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ;")
  
  return(.BIEN_sql(query, ...))
  
}

############################

#'Calculates species mean values for a given trait, using Genus or Family level data where Species level data is lacking.
#'
#'BIEN_trait_mean Estimates species mean values for a given trait, using Genus or Family level data where Species level data is absent.
#' @param species A single species or a vector of species.
#' @param trait A single trait.
#' @param ... Additional arguments passed to internal functions.
#' @note Trait spelling needs to be exact and case-sensitive, see \code{\link{BIEN_trait_list}} for a list of traits.
#' @return A dataframe of estimated trait means and associated metadata for the given species.
#' @examples \dontrun{
#' BIEN_trait_mean(species=c("Poa annua","Juncus trifidus"),trait="leaf dry mass per leaf fresh mass") }
#' @family trait functions
#' @export
BIEN_trait_mean <- function(species,
                            trait,
                            ...){
  
  
  #first, get taxonomic info for the species
  .is_char(trait)
  .is_char(species)
  
  #make sure there is only one trait
  if( length(trait) > 1){stop("Multiple traits submitted. This function only handles one trait at a time.")}
  
  #make sure trait exists
  traits_available <- BIEN_trait_list(...)
  if(!trait %in% traits_available$trait_name){stop("Trait not found.")}
  
  
  # create query to retreive taxonomic info
  genera <- unlist(lapply(X = strsplit(species," "),
                          FUN = function(x){x[1]}))
  
  query <- paste("SELECT DISTINCT scrubbed_family,scrubbed_genus,scrubbed_species_binomial FROM bien_taxonomy WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") or scrubbed_genus in (", paste(shQuote(genera, type = "sh"),collapse = ', '), ") ;")
  
  
  taxonomy_for_traits <- .BIEN_sql(query, ...)
  #taxonomy_for_traits <- .BIEN_sql(query)
  if(length(taxonomy_for_traits) == 0){stop("Taxonomic data missing, check species name(s)")}
  
  
  #then, query the various taxonomic levels to get trait data

  query <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND (scrubbed_family in (", paste(shQuote(unique(taxonomy_for_traits$scrubbed_family)  , type = "sh"),collapse = ', '), ") or  scrubbed_genus in (", paste(shQuote(unique(taxonomy_for_traits$scrubbed_genus)  , type = "sh"),collapse = ', '), ")) ;")
  
  traits_df <- suppressWarnings(.BIEN_sql(query, ...)) #suppress warnings to avoid the geom message
  #traits_df <- suppressWarnings(.BIEN_sql(query))
  
  if(length(traits_df) == 0){stop("No matching trait data for these taxa.")}
  
  #finally, choose the best available trait data
  
  output_data <- NULL
  for(i in 1:length(species)){
    
    species_i_data<-list()
    species_i_data[[1]]<-cbind(traits_df$trait_value[which(traits_df$scrubbed_species_binomial==species[i])],traits_df$id[which(traits_df$scrubbed_species_binomial==species[i])]  )
    species_i_data[[2]]<-cbind(traits_df$trait_value[which(traits_df$scrubbed_genus==taxonomy_for_traits$scrubbed_genus[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])],traits_df$id[which(traits_df$scrubbed_genus==taxonomy_for_traits$scrubbed_genus[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])])
    if(length(species_i_data[[2]])==0){
      species_i_data[[2]]<-cbind(traits_df$trait_value[which(traits_df$scrubbed_genus==strsplit(species[i]," ")[[1]][1])],traits_df$id[which(traits_df$scrubbed_genus==strsplit(species[i]," ")[[1]][1])])
    }
    #species_i_data[[3]]<-traits_df$trait_value[which(traits_df$family==taxonomy_for_traits$scrubbed_family[i])]
    species_i_data[[3]]<-cbind(traits_df$trait_value[which(traits_df$scrubbed_family==taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])],traits_df$id[which(traits_df$scrubbed_family==taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_species_binomial==species[i])])])
    if(length(species_i_data[[3]])==0){
      species_i_data[[3]]<-cbind(traits_df$trait_value[which(traits_df$scrubbed_family==unique(taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_genus==strsplit(species[i]," ")[[1]][1])]))],traits_df$id[which(traits_df$scrubbed_family==unique(taxonomy_for_traits$scrubbed_family[which(taxonomy_for_traits$scrubbed_genus==strsplit(species[i]," ")[[1]][1])]))])
    }
    species_i_data[[4]]<-"NA"
    
    names(species_i_data)<-c("Species","Genus","Family","NA")
    
    species_i_data<-species_i_data[which(lengths(species_i_data)>0)]#prunes list to include only taxonomic levels with data
    
    #trait_mean<- species_i_data[1]
    
    if(length(species_i_data)>0){
      level_used<-names(species_i_data[1])
      if(species_i_data[[1]][1]=="NA"){sample_size<-0}else{sample_size<-nrow(species_i_data[[1]])}
      if(species_i_data[[1]][1]=="NA"){mean_value<-"NA"}else{mean_value<-mean(as.numeric(species_i_data[[1]][,1]), na.rm = TRUE)}
      if(species_i_data[[1]][1]=="NA"){ids <-"NA"}else{ids<-paste(as.numeric(species_i_data[[1]][,2]),collapse = ",")}
      unit<-unique(traits_df$unit)
      
      output_data<-rbind(output_data,cbind(species[i],mean_value,trait,unit,level_used,sample_size,ids))
    }#if data is available
    
    
    
  }#i loop
  
  colnames(output_data)[1] <- "species"
  output_data <- as.data.frame(output_data)
  return(output_data)
  
}

############################

#'Download all measurements of a specific trait(s).
#'
#'BIEN_trait_trait downloads all measurements of the trait(s) specified.
#' @param trait A single trait or a vector of traits.
#' @template trait
#' @note Trait spelling needs to be exact and case-sensitive, see \code{\link{BIEN_trait_list}} for a list of traits.
#' @return A dataframe of all available trait data for the given trait(s).
#' @examples \dontrun{
#' BIEN_trait_trait("whole plant height")
#' trait_vector<-c("whole plant height", "leaf dry mass per leaf fresh mass")
#' BIEN_trait_trait(trait_vector)}
#' @family trait functions
#' @export
BIEN_trait_trait <- function(trait,
                             all.taxonomy = FALSE,
                             political.boundaries = FALSE,
                             source.citation = FALSE,...){

  .is_char(trait)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  
  # set the query
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)
  source_<-.source_check_traits(source.citation)
  
  
  query <- paste("SELECT 
                 scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,",project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id 
                 FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ;")

  return(.BIEN_sql(query, ...))

}
############################

#'Download trait data for given species and trait.
#'
#'BIEN_trait_traitbyspecies extracts entries that contain the specified species and trait(s).
#' @param species A single species or a vector of species.
#' @param trait A single trait or a vector of traits.
#' @template trait
#' @note Trait spelling needs to be exact and case-sensitive, see \code{\link{BIEN_trait_list}} for a list of traits.
#' @return A dataframe of all data matching the specified trait(s) and species.
#' @examples \dontrun{
#' BIEN_trait_traitbyspecies(trait = "whole plant height", species = "Carex capitata")
#' trait_vector<-c("whole plant height", "leaf area")
#' species_vector<-c("Carex capitata","Betula nana")
#' BIEN_trait_traitbyspecies(trait=trait_vector,species=species_vector)}
#' @family trait functions
#' @export
BIEN_trait_traitbyspecies <- function(species,
                                      trait,
                                      all.taxonomy = FALSE,
                                      political.boundaries = FALSE,
                                      source.citation = FALSE,
                                      ...){
  .is_char(species)
  .is_char(trait)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  
  # set the query
  taxonomy_ <- .taxonomy_check_traits(all.taxonomy)  
  political_ <- .political_check_traits(political.boundaries)
  source_ <- .source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id 
                 FROM agg_traits 
                 WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") 
                 AND trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ;")
  
  return(.BIEN_sql(query, ...))
  
}
###########################

#'Download trait data for given genus/genera and trait(s).
#'
#'BIEN_trait_traitbygenus extracts entries that contain the specified genus/genera and trait(s).
#' @param genus A single genus or a vector of genera.
#' @param trait A single trait or a vector of traits.
#' @template trait
#' @note Trait spelling needs to be exact and case-sensitive, see \code{\link{BIEN_trait_list}} for a list of traits.
#' @return A dataframe of all data matching the specified trait(s) and genus/genera.
#' @examples \dontrun{
#' BIEN_trait_traitbygenus(trait = "whole plant height", genus = "Carex")
#' trait_vector<-c("whole plant height", "leaf area")
#' genus_vector<-c("Carex","Betula")
#' BIEN_trait_traitbygenus(trait=trait_vector,genus=genus_vector)}
#' @family trait functions
#' @export
BIEN_trait_traitbygenus <- function(genus,
                                    trait,
                                    all.taxonomy = FALSE,
                                    political.boundaries = FALSE,
                                    source.citation = FALSE,
                                    ...){

  .is_char(genus)
  .is_char(trait)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  # set the query
  
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)
  source_<-.source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_genus, scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id 
                 FROM agg_traits 
                 WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") 
                 AND trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ;")

  return(.BIEN_sql(query, ...))

}
###########################

#'Download trait data for given families and traits.
#'
#'BIEN_trait_traitbyfamily extracts entries that contain the specified families and trait(s).
#' @param family A single family or a vector of families.
#' @param trait A single trait or a vector of traits.
#' @template trait
#' @note Trait spelling needs to be exact and case-sensitive, see \code{\link{BIEN_trait_list}} for a list of traits.
#' @return A dataframe of all data matching the specified trait(s) and family/families.
#' @examples \dontrun{
#' BIEN_trait_traitbyfamily(trait = "whole plant height", family = "Poaceae")
#' trait_vector <- c("whole plant height", "leaf fresh mass")
#' family_vector < -c("Orchidaceae","Poaceae")
#' BIEN_trait_traitbyfamily(trait = trait_vector, family = family_vector)}
#' @family trait functions
#' @export
BIEN_trait_traitbyfamily <- function(family,
                                     trait,
                                     all.taxonomy = FALSE,
                                     political.boundaries = FALSE,
                                     source.citation = FALSE,
                                     ...){

  .is_char(family)
  .is_char(trait)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  
  # set the query
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)
  source_<-.source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_family, scrubbed_genus, scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id
                 FROM agg_traits 
                 WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") 
                 AND trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ;")

  return(.BIEN_sql(query, ...))

}

############################

#'Download trait data for given genera.
#'
#'BIEN_trait_genus extracts entries that contain the specified genera.
#' @param genus A single genus or a vector of genera.
#' @template trait
#' @return A dataframe of all data matching the specified genera.
#' @examples \dontrun{
#' BIEN_trait_genus("Acer")
#' genus_vector <- c("Acer","Abies")
#' BIEN_trait_genus(genus_vector)}
#' @family trait functions
#' @export
BIEN_trait_genus <- function(genus,
                             all.taxonomy = FALSE,
                             political.boundaries = FALSE,
                             source.citation = FALSE,
                             ...){

  .is_char(genus)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  
  # set the query
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)
  source_<-.source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_genus, scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access,id 
                 FROM agg_traits WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ;")
  
  return(.BIEN_sql(query, ...))

}


###########################

#'Download trait data for given families.
#'
#'BIEN_trait_family extracts all trait data for the specified families.
#' @param family A single family or a vector of families.
#' @template trait
#' @return A dataframe of all data matching the specified families.
#' @examples \dontrun{
#' BIEN_trait_family("Poaceae")
#' family_vector<-c("Poaceae","Orchidaceae")
#' BIEN_trait_family(family_vector)}
#' @family trait functions
#' @export
BIEN_trait_family <- function(family,
                              all.taxonomy = FALSE,
                              political.boundaries = FALSE,
                              source.citation = FALSE,
                              ...){

  .is_char(family)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)

  # set the query
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)
  source_<-.source_check_traits(source.citation)
  
  query <- paste("SELECT 
                 scrubbed_family, scrubbed_genus, scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access,id 
                 FROM agg_traits WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ;")
  
  return(.BIEN_sql(query, ...))
  
}

############################

#'List all available types of trait data
#'
#'BIEN_trait_list produces a dataframe of all available types of trait data.
#' @param ... Additional arguments passed to internal functions.
#' @return A dataframe containing all currently available types of trait data and details on measurement.
#' @examples \dontrun{
#' BIEN_trait_list()}
#' @family trait functions
#' @export
BIEN_trait_list <- function( ...){
  
  # set the query
  query <- paste("SELECT DISTINCT trait_name FROM agg_traits ;")
  
  return(.BIEN_sql(query, ...))
  
}


###################################

#'Download trait data for given country.
#'
#'BIEN_trait_species extracts trait data for the species country.
#' @param country A single country or a vector of countries.
#' @param trait.name Optional.  The trait or traits you want returned.  If left blank, all traits will be returned.
#' @template trait
#' @return A dataframe of all available trait data for the given country.
#' @examples \dontrun{
#' BIEN_trait_country("South Africa")
#' BIEN_trait_country(country="South Africa",trait="whole plant growth form")}
#' @family trait functions
#' @export
BIEN_trait_country <- function(country,
                               trait.name = NULL,
                               all.taxonomy = FALSE,
                               political.boundaries = TRUE,
                               source.citation = FALSE,
                               ...){

  .is_char(country)
  .is_log(all.taxonomy)
  .is_log(political.boundaries)
  .is_log(source.citation)
  .is_char(trait.name)
  
  # set the query
  taxonomy_<-.taxonomy_check_traits(all.taxonomy)  
  political_<-.political_check_traits(political.boundaries)  
  source_<-.source_check_traits(source.citation)
  
  if(!is.null(trait.name)){
    trait_select<-paste(" AND", "trait_name in (", paste(shQuote(trait.name, type = "sh"),collapse = ', '), ") ")  
  }else{trait_select <- ""}
  
  
  query <- paste("SELECT 
                 scrubbed_species_binomial, trait_name, trait_value, unit, method, latitude, longitude, elevation_m, url_source",source_$select ,", project_pi, project_pi_contact",
                 political_$select, taxonomy_$select,", access, id 
                 FROM agg_traits WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",trait_select," ;")
  
  return(.BIEN_sql(query, ...))
  
}



#############################

#'Count the number of (geoValid) occurrence records for each species in BIEN
#'
#'BIEN_occurrence_records_per_species downloads a count of the number of geovalidated occurrence records for each species in the BIEN database.
#' @param species A single species, or vector of species.  If NULL, the default, it will return counts for all species.
#' @param ... Additional arguments passed to internal functions.
#' @return A dataframe listing the number of geovalidated occurrence records for each species in the BIEN database.
#' @examples \dontrun{
#' occurrence_counts<-BIEN_occurrence_records_per_species()}
#' @family occurrence functions
#' @export
BIEN_occurrence_records_per_species <- function(species = NULL,
                                                ...){
  
  if(is.null(species)){    
    # set the query
    query <- paste("SELECT DISTINCT scrubbed_species_binomial,count(*)
                   FROM view_full_occurrence_individual
                   WHERE is_geovalid = 1
                    AND latitude IS NOT NULL
                    AND LONGITUDE IS NOT NULL
                   GROUP BY scrubbed_species_binomial ;")
  }
  
  if(is.character(species)){
    query <- paste("SELECT scrubbed_species_binomial,count(*) 
                 FROM view_full_occurrence_individual 
                 WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") 
                  AND is_geovalid = 1 AND (georef_protocol is NULL OR georef_protocol<>'county centroid') 
                  AND (is_centroid IS NULL OR is_centroid=0) 
                  AND observation_type IN ('plot','specimen','literature','checklist')    
                 GROUP BY scrubbed_species_binomial ;")
  }
  
  return(.BIEN_sql(query, ...))
  
  }

###############################################
#'Count the number of trait observations for each species in the BIEN database
#'
#'BIEN_trait_traits_per_species downloads a count of the number of records for each trait for each species in the BIEN database.
#' @param species Optional species or vector of species.  If left blank, returns counts for all species.
#' @param ... Additional arguments passed to internal functions.
#' @return Returns a dataframe containing the number of trait records for each species in the BIEN database.
#' @examples \dontrun{
#' trait_observation_counts<-BIEN_trait_traits_per_species()}
#' @family trait functions
#' @export
BIEN_trait_traits_per_species <- function(species = NULL,
                                          ...){

  if(!is.null(species)){
    
    species_query<-paste("WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")")  
    
  }else{
    
    species_query <- ""  

  }
  
  # set the query
  query <- paste("SELECT DISTINCT scrubbed_species_binomial, trait_name,count(*) 
                 FROM agg_traits",
                 species_query,
                 "GROUP BY trait_name,scrubbed_species_binomial ;")
  
  return(.BIEN_sql(query, ...))
  
}

################################
#Plot queries

##############################
#'Download plot data from a given datasource.
#'
#'BIEN_plot_datasource downloads all plot data from a given datasource.
#' @param datasource A datasource. See \code{\link{BIEN_plot_list_datasource}} for options.
#' @template plot
#' @return A dataframe containing all data from the specified datasource.
#' @examples \dontrun{
#' BIEN_plot_datasource("SALVIAS")}
#' @family plot functions
#' @export
BIEN_plot_datasource <- function(datasource,
                                 cultivated = FALSE,
                                 new.world = NULL,
                                 all.taxonomy = FALSE,
                                 native.status = FALSE,
                                 natives.only = TRUE,
                                 political.boundaries = FALSE,
                                 collection.info = FALSE,
                                 all.metadata = FALSE,
                                 ...){

  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(datasource)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)

  #set conditions for query
  cultivated_ <- .cultivated_check_plot(cultivated)
  newworld_ <- .newworld_check_plot(new.world)
  taxonomy_ <- .taxonomy_check_plot(all.taxonomy)
  native_ <- .native_check_plot(native.status)
  natives_ <- .natives_check_plot(natives.only)
  political_ <- .political_check_plot(political.boundaries)
  collection_ <- .collection_check_plot(collection.info)
  md_ <- .md_check_plot(all.metadata)
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, view_full_occurrence_individual.elevation_m,
                 view_full_occurrence_individual.plot_area_ha,view_full_occurrence_individual.sampling_protocol,
                 view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.scrubbed_species_binomial,
                 view_full_occurrence_individual.individual_count",taxonomy_$select,political_$select,native_$select," ,view_full_occurrence_individual.latitude, 
                 view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,view_full_occurrence_individual.datasource,
                 view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,view_full_occurrence_individual.custodial_institution_codes,
                 view_full_occurrence_individual.collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                  (SELECT * FROM view_full_occurrence_individual 
                   WHERE view_full_occurrence_individual.datasource in (", paste(shQuote(datasource, type = "sh"),collapse = ', '), ")",cultivated_$query,newworld_$query,natives_$query,  "
                    AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                    AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                    AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0)  
                    AND observation_type='plot'
                    AND scrubbed_species_binomial IS NOT NULL ) as view_full_occurrence_individual
                 JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                  ;")
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
  }


##################################
#'List available datasources.
#'
#'BIEN_plot_list_datasource list all plot datasources in the BIEN database.
#' @param ... Additional arguments passed to internal functions.
#' @return A vector of available datasources.
#' @examples \dontrun{
#' BIEN_plot_list_datasource()}
#' @family plot functions
#' @export
BIEN_plot_list_datasource <- function(...){
  
  query <- paste("SELECT DISTINCT plot_metadata.datasource FROM plot_metadata ;")
  return(.BIEN_sql(query, ...))
  
}

###############################
#'Download plot data from specified countries.
#'
#'BIEN_plot_country downloads all plot data from specified countries.
#' @param country A country or vector of countries.
#' @param country.code A single country code or a vector of country codes equal in length to the vector of states/province codes.
#' @template plot
#' @return A dataframe containing all data from the specified countries.
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @examples \dontrun{
#' BIEN_plot_country("Costa Rica")
#' BIEN_plot_country(c("Costa Rica","Panama"))}
#' @family plot functions
#' @export
BIEN_plot_country <- function(country = NULL,
                              country.code = NULL,
                              cultivated = FALSE,
                              new.world = NULL,
                              all.taxonomy = FALSE,
                              native.status = FALSE,
                              natives.only = TRUE,
                              political.boundaries = FALSE,
                              collection.info = FALSE,
                              all.metadata = FALSE,
                              ...){

  .is_char(country.code)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(country)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)
  if(is.null(country)& is.null(country.code))  {stop("Please supply either a country name or 2-digit ISO code")}
  
  #set conditions for query
    cultivated_ <- .cultivated_check_plot(cultivated)
    newworld_ <- .newworld_check_plot(new.world)
    taxonomy_ <- .taxonomy_check_plot(all.taxonomy)
    native_ <- .native_check_plot(native.status)
    natives_ <- .natives_check_plot(natives.only)
    collection_ <- .collection_check_plot(collection.info)
    md_ <- .md_check_plot(all.metadata)
  
  if(!political.boundaries){
    political_select <- "view_full_occurrence_individual.country,"
  }else{
    political_select <- "view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  # set the query
  if(is.null(country.code)){
    query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, view_full_occurrence_individual.elevation_m,
                      view_full_occurrence_individual.plot_area_ha, view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, 
                      view_full_occurrence_individual.scrubbed_species_binomial,view_full_occurrence_individual.individual_count",taxonomy_$select,native_$select,", 
                      view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude, view_full_occurrence_individual.date_collected,
                      view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,
                      view_full_occurrence_individual.custodial_institution_codes,view_full_occurrence_individual.collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                   FROM 
                      (SELECT * FROM view_full_occurrence_individual 
                       WHERE view_full_occurrence_individual.country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",
                          cultivated_$query,newworld_$query,natives_$query,  "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                          AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                          AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                          AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0) AND observation_type='plot' 
                          AND scrubbed_species_binomial IS NOT NULL) as view_full_occurrence_individual 
                   LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                    ;")
  }else{
    
    query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, view_full_occurrence_individual.elevation_m,
                      view_full_occurrence_individual.plot_area_ha, view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, 
                      view_full_occurrence_individual.scrubbed_species_binomial,view_full_occurrence_individual.individual_count",taxonomy_$select,native_$select,", 
                      view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude, view_full_occurrence_individual.date_collected,
                      view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,
                      view_full_occurrence_individual.custodial_institution_codes,view_full_occurrence_individual.collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                    FROM 
                      (SELECT * FROM view_full_occurrence_individual WHERE view_full_occurrence_individual.country in 
                        (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), "))",
                        cultivated_$query,newworld_$query,natives_$query,  "
                        AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                        AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                        AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                        AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0) AND observation_type='plot'
                        AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL) as view_full_occurrence_individual 
                   LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                    ;")  
    
    
  }
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
}
###############################
#'Download plot data from specified states/provinces.
#'
#'BIEN_plot_state downloads all plot data from specified states/provinces.
#' @param country A single country.
#' @param state A state or vector of states (or other primary political divisions).
#' @param state.code A single state/province code, or a vector of states/province codes.
#' @param country.code A single country code or a vector of country codes equal in length to the vector of states/province codes.
#' @template plot
#' @note Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
#' @note This function requires you supply either 1) a single country with one or states, or 2) vectors of equal length for each political level.
#' @return A dataframe containing all data from the specified states.
#' @examples \dontrun{
#' BIEN_plot_state(country="United States", state="Colorado")
#' BIEN_plot_state(country="United States",state= c("Colorado","California"))}
#' @family plot functions
#' @export
BIEN_plot_state <- function(country = NULL,
                            state = NULL,
                            country.code = NULL,
                            state.code = NULL,
                            cultivated = FALSE,
                            new.world = NULL,
                            all.taxonomy = FALSE,
                            native.status = FALSE,
                            natives.only = TRUE,
                            political.boundaries = TRUE,
                            collection.info = FALSE,
                            all.metadata = FALSE,
                            ...){

  .is_char(country)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(state)
  .is_char(state.code)
  .is_char(country.code)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)
  
  #set conditions for query
  cultivated_<-.cultivated_check_plot(cultivated)
  newworld_<-.newworld_check_plot(new.world)
  taxonomy_<-.taxonomy_check_plot(all.taxonomy)
  native_<-.native_check_plot(native.status)
  natives_<-.natives_check_plot(natives.only)
  collection_<-.collection_check_plot(collection.info)
  md_<-.md_check_plot(all.metadata)
  
  if(!political.boundaries){
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,"
  }else{
    political_select<-"view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
  }
  
  if(is.null(country.code) & is.null(state.code) ){  

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
    
  }else{
    
    
    if(length(country.code)==1){
      sql_where <- paste(" WHERE country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code, type = "sh"),collapse = ', '), ")) 
                         AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code, type = "sh"),collapse = ', '), ")) 
                         AND scrubbed_species_binomial IS NOT NULL")
    }else{
      
      if(length(country.code)==length(state.code)){
        
        sql_where<-"WHERE ("
        
        for(i in 1:length(country.code)){
          
          condition_i<- paste("country in (SELECT country FROM country WHERE iso in (", paste(shQuote(country.code[i], type = "sh"),collapse = ', '), ")) 
                              AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", paste(shQuote(state.code[i], type = "sh"),collapse = ', '), "))")
          if(i!=1){condition_i<- paste("OR ",condition_i)}#stick OR onto the condition where needed
          sql_where<-paste(sql_where,condition_i)
          
        }#for i  
        
        sql_where<-paste(sql_where,") AND scrubbed_species_binomial IS NOT NULL")  
        
      }else{
        stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")  
        
      }  
      
    }#if length(country>1)
    
  }    
  
  # set the query
  query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, 
                    view_full_occurrence_individual.plot_area_ha,view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count",
                    taxonomy_$select,native_$select," ,view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                    view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,custodial_institution_codes,
                    collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM view_full_occurrence_individual ",
                      sql_where,cultivated_$query,newworld_$query,natives_$query,  "
                      AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                      AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                      AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0) 
                      AND observation_type='plot' 
                      AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL) as view_full_occurrence_individual 
                 JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                  ;")
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
  }
###############################

#'Download plot data from specified sf object.
#'
#'BIEN_plot_sf downloads all plot data falling within a supplied sf polygon.
#' @param sf An object of class sf.  Note that the projection must be WGS84.
#' @template plot
#' @return A dataframe containing all plot data from within the specified sf polygon.
#' @examples \dontrun{
#' library(sf)
#' 
#' BIEN_ranges_species("Carnegiea gigantea") #saves ranges to the current working directory
#' 
#' sf <- st_read(dsn = ".",
#'               layer = "Carnegiea_gigantea")
#' 
#' saguaro_plot_data <- BIEN_plot_sf(sf = sf)
#' }
#' @family plot functions
#' @importFrom sf st_geometry st_as_text
#' @export
BIEN_plot_sf <- function(sf,
                         cultivated = FALSE,
                         new.world = NULL,
                         all.taxonomy = FALSE,
                         native.status = FALSE,
                         natives.only = TRUE,
                         political.boundaries = TRUE,
                         collection.info = FALSE,
                         all.metadata = FALSE,
                         ...){
  
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)
  
  
  # Convert the sf to wkt (needed for sql query)
  
  wkt <- sf |>
    st_geometry() |>
    st_as_text()
  
  #set conditions for query
  
  cultivated_ <- .cultivated_check_plot(cultivated)
  newworld_ <- .newworld_check_plot(new.world)
  taxonomy_ <- .taxonomy_check_plot(all.taxonomy)
  native_ <- .native_check_plot(native.status)
  natives_ <- .natives_check_plot(natives.only)
  collection_ <- .collection_check_plot(collection.info)
  md_ <- .md_check_plot(all.metadata)
  
  if(!political.boundaries){
    
    political_select <- "view_full_occurrence_individual.country,"
    
  }else{
    
    political_select <- "view_full_occurrence_individual.country,view_full_occurrence_individual.state_province,view_full_occurrence_individual.county,view_full_occurrence_individual.locality,"
    
  }
  
  
  # set the query
  
  query <- paste("SELECT ",political_select," view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, 
                      view_full_occurrence_individual.plot_area_ha,view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count",
                 taxonomy_$select,native_$select," ,view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                      view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,custodial_institution_codes,
                      collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                   FROM 
                      (SELECT * FROM view_full_occurrence_individual ",
                 "WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom) ",cultivated_$query,newworld_$query,natives_$query,  "
                          AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                          AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                          AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0) 
                          AND observation_type='plot' 
                          AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL ) as view_full_occurrence_individual 
                   JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                   ;")
  
  # create query to retrieve
  
  return(.BIEN_sql(query, ...))
  #return(.BIEN_sql(query))
  
}




###############################
#'List available sampling protocols.
#'
#'BIEN_plot_list_sampling_protocols list all available sampling protocols.
#' @param ... Additional arguments passed to internal functions.
#' @return A vector of available sampling protocols.
#' @examples \dontrun{
#' BIEN_plot_list_sampling_protocols()}
#' @family plot functions
#' @export
BIEN_plot_list_sampling_protocols <- function(...){
  
  query <- paste("SELECT DISTINCT sampling_protocol FROM plot_metadata ;")
  return(.BIEN_sql(query, ...))
  
}

################################
#'Download plot data using a specified sampling protocol.
#'
#'BIEN_plot_sampling_protocol downloads all plot data using a specified sampling protocol.
#' @param sampling_protocol A sampling protocol or vector of sampling protocols. See \code{\link{BIEN_plot_list_sampling_protocols}} for options.
#' @template plot
#' @return A dataframe containing all data from the specified datasource.
#' @examples \dontrun{
#' BIEN_plot_sampling_protocol("Point-intercept")}
#' @family plot functions
#' @export
BIEN_plot_sampling_protocol <- function (sampling_protocol,
                                         cultivated = FALSE,
                                         new.world = FALSE,
                                         all.taxonomy = FALSE,
                                         native.status = FALSE,
                                         natives.only = TRUE, 
                                         political.boundaries = FALSE,
                                         collection.info = FALSE,
                                         all.metadata = FALSE,
                                         ...){
  
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(sampling_protocol)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)
  
  cultivated_ <- .cultivated_check_plot(cultivated)
  newworld_ <- .newworld_check_plot(new.world)
  taxonomy_ <- .taxonomy_check_plot(all.taxonomy)
  native_ <- .native_check_plot(native.status)
  natives_ <- .natives_check_plot(natives.only)
  political_ <- .political_check_plot(political.boundaries)
  collection_ <- .collection_check_plot(collection.info)
  md_ <- .md_check_plot(all.metadata)
  
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha, 
                    view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count", 
                    taxonomy_$select, native_$select, political_$select,",
                    view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,date_collected,view_full_occurrence_individual.datasource,
                    view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,custodial_institution_codes,collection_code,
                    view_full_occurrence_individual.datasource_id", collection_$select, cultivated_$select, newworld_$select, md_$select, " 
                 FROM 
                    (SELECT * FROM view_full_occurrence_individual 
                    WHERE view_full_occurrence_individual.sampling_protocol in (", paste(shQuote(sampling_protocol, type = "sh"), collapse = ", "), ")",
                        cultivated_$query, newworld_$query, natives_$query, 
                        "AND view_full_occurrence_individual.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                        AND (view_full_occurrence_individual.is_geovalid = 1 ) 
                        AND (view_full_occurrence_individual.georef_protocol is NULL OR view_full_occurrence_individual.georef_protocol<>'county centroid') 
                        AND (view_full_occurrence_individual.is_centroid IS NULL OR view_full_occurrence_individual.is_centroid=0) 
                        AND view_full_occurrence_individual.observation_type='plot' 
                        AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL) as view_full_occurrence_individual
                 JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id) ;")
  return(.BIEN_sql(query, ...))
}


#################################
#'Download plot data by plot name.
#'
#'BIEN_plot_name downloads all plot data for a set of plot names.
#' @param plot.name A plot name or vector of names. See BIEN_plot_metadata for more information on plots.
#' @template plot
#' @note Plot names can be looked up with \code{\link{BIEN_plot_metadata}}.
#' @return A dataframe containing all data from the specified plot(s).
#' @examples \dontrun{
#' BIEN_plot_name("SR-1")}
#' @family plot functions
#' @export
BIEN_plot_name <- function(plot.name,
                           cultivated = FALSE,
                           new.world = NULL,
                           all.taxonomy = FALSE,
                           native.status = FALSE,
                           natives.only = TRUE,
                           political.boundaries = FALSE,
                           collection.info = FALSE,
                           all.metadata = FALSE,
                           ...){

  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(plot.name)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)  
  .is_log(collection.info)
  .is_log(all.metadata)
  
  #set conditions for query
  cultivated_<-.cultivated_check_plot(cultivated)
  newworld_<-.newworld_check_plot(new.world)
  taxonomy_<-.taxonomy_check_plot(all.taxonomy)
  native_<-.native_check_plot(native.status)
  natives_<-.natives_check_plot(natives.only)
  political_<-.political_check_plot(political.boundaries)
  collection_<-.collection_check_plot(collection.info)
  md_<-.md_check_plot(all.metadata)
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,
                    view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.scrubbed_species_binomial,
                    view_full_occurrence_individual.individual_count",taxonomy_$select,native_$select,political_$select,", 
                    view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                    view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,view_full_occurrence_individual.dataowner,
                    view_full_occurrence_individual.custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM view_full_occurrence_individual WHERE view_full_occurrence_individual.plot_name in (", paste(shQuote(plot.name, type = "sh"),collapse = ', '), ")",
                      cultivated_$query,newworld_$query,natives_$query,  "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
                      AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) AND observation_type='plot' 
                      AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL ) as view_full_occurrence_individual
                 LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                  ;")
  
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
  }

#####################
#'Download plot data by dataset.
#'
#'BIEN_plot_dataset downloads all plot data for a given dataset or datasets.
#' @param dataset A plot dataset or vector of datasets. See BIEN_plot_metadata for more information on plots.
#' @template plot
#' @return A dataframe containing all data from the specified dataset.
#' @note Datasets and related information can be looked up with \code{\link{BIEN_plot_metadata}}
#' @examples \dontrun{
#' BIEN_plot_dataset("Gentry Transect Dataset")}
#' @family plot functions
#' @export
BIEN_plot_dataset <- function(dataset,
                              cultivated = FALSE,
                              new.world = NULL,
                              all.taxonomy = FALSE,
                              native.status = FALSE,
                              natives.only = TRUE,
                              political.boundaries = FALSE,
                              collection.info = FALSE,
                              all.metadata = FALSE,
                              ...){

  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(dataset)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  .is_log(all.metadata)
  
  #set conditions for query
  cultivated_<-.cultivated_check_plot(cultivated)
  newworld_<-.newworld_check_plot(new.world)
  taxonomy_<-.taxonomy_check_plot(all.taxonomy)
  native_<-.native_check_plot(native.status)
  natives_<-.natives_check_plot(natives.only)
  political_<-.political_check_plot(political.boundaries)
  collection_<-.collection_check_plot(collection.info)
  md_<-.md_check_plot(all.metadata)
  
  # set the query
  query <- paste("SELECT view_full_occurrence_individual.plot_name,subplot, view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha,
                    view_full_occurrence_individual.sampling_protocol,recorded_by, scrubbed_species_binomial,individual_count",taxonomy_$select,native_$select,political_$select,", 
                    view_full_occurrence_individual.latitude, view_full_occurrence_individual.longitude,view_full_occurrence_individual.date_collected,
                    view_full_occurrence_individual.datasource,view_full_occurrence_individual.dataset,
                    view_full_occurrence_individual.dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                    collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM view_full_occurrence_individual 
                    WHERE view_full_occurrence_individual.dataset in (", paste(shQuote(dataset, type = "sh"),collapse = ', '), ")",
                        cultivated_$query,newworld_$query,natives_$query,  " AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                        AND is_geovalid = 1 AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
                        AND observation_type='plot' AND view_full_occurrence_individual.scrubbed_species_binomial IS NOT NULL ) as view_full_occurrence_individual
                 LEFT JOIN plot_metadata ON (view_full_occurrence_individual.plot_metadata_id=plot_metadata.plot_metadata_id)
                  ;")
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
  }


##############################
#'Download plot metadata
#'
#'BIEN_plot_metadata downloads the plot metadata table. 
#' @param ... Additional arguments passed to internal functions.
#' @return A dataframe containing plot metadata.
#' @examples \dontrun{
#' BIEN_plot_metadata()}
#' @family plot functions
#' @family metadata functions
#' @export
BIEN_plot_metadata <- function( ...){
  
  # set the query
  query <- "SELECT * FROM plot_metadata ;"
  
  # create query to retrieve
  return(.BIEN_sql(query, ...))
  
}



##############################

################################
#Taxonomy queries

#'Extract taxonomic information for species
#'
#'BIEN_taxonomy_species downloads a dataframe of all taxonomic information for given species.
#' @param species A single species or a vector of species.
#' @template taxonomy
#' @return Dataframe containing taxonomic information for the specified species.
#' @examples \dontrun{
#' BIEN_taxonomy_species("Cannabis sativa")
#' species_vector<-c("Acer nigrum","Cannabis sativa")
#' BIEN_taxonomy_species(species_vector)}
#' @family taxonomy functions
#' @export
BIEN_taxonomy_species <- function(species,
                                  ...){

  .is_char(species)
  
  #set base query components
    sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
    sql_from <- paste(' FROM bien_taxonomy')
    sql_where <- paste(' WHERE scrubbed_species_binomial in (', paste(shQuote(species, type = "sh"),collapse = ', '), ') AND scrubbed_species_binomial IS NOT NULL')

  # form the final query
    query <- paste(sql_select, sql_from, sql_where,  " ;")
  
  # execute the query
  
    return(.BIEN_sql(query, ...))
  
}

#################
#'Extract taxonomic information for genera
#'
#'BIEN_taxonomy_genus downloads a dataframe of all taxonomic information for given genera.
#' @param genus A single genus or a vector of genera.
#' @template taxonomy
#' @return Dataframe containing taxonomic information for the specified genera.
#' @examples \dontrun{
#' BIEN_taxonomy_genus("Acer")
#' genus_vector<-c("Acer","Quercus")
#' BIEN_taxonomy_genus(genus_vector)}
#' @family taxonomy functions
#' @export
BIEN_taxonomy_genus <- function(genus,
                                ...){

  .is_char(genus)
  
  #set base query components
    sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
    sql_from <- paste(" FROM bien_taxonomy")
    sql_where <- paste(" WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")

  # form the final query
    query <- paste(sql_select, sql_from, sql_where, " ;")
  
  # execute the query
    return(.BIEN_sql(query, ...))

}


###########
#'Extract taxonomic information for families
#'
#'BIEN_taxonomy_family downloads a dataframe of all taxonomic information for given families.
#' @param family A single family or a vector of families.
#' @template taxonomy
#' @return Dataframe containing taxonomic information for the specified families.
#' @examples \dontrun{
#' BIEN_taxonomy_family("Orchidaceae")
#' family_vector<-c("Orchidaceae","Poaceae")
#' BIEN_taxonomy_family(family_vector)}
#' @family taxonomy functions
#' @export
BIEN_taxonomy_family <- function(family,
                                 ...){

  .is_char(family)
  
  #set base query components
    sql_select <-  paste('SELECT DISTINCT higher_plant_group, "class", superorder, "order", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status')
    sql_from <- paste(" FROM bien_taxonomy")
    sql_where <- paste(" WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") AND scrubbed_species_binomial IS NOT NULL")

  # form the final query
    query <- paste(sql_select, sql_from, sql_where, " ;")
    #print(query)
  
  # execute the query
    return(.BIEN_sql(query, ...))
  
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
#' @template phylogeny
#' @return A phylo or multiphylo object containing the specified phylogenies
#' @examples \dontrun{
#' phylos<-BIEN_phylogeny_complete(n_phylogenies = 10,seed = 1)
#' phylos<-BIEN_phylogeny_complete(replicates = c(1,2,99,100))}
#' @family phylogeny functions
#' @importFrom ape read.tree
#' @export
BIEN_phylogeny_complete<-function(n_phylogenies = 1,
                                  seed = NULL,
                                  replicates = NULL,
                                  ...){

  .is_num(n_phylogenies)  
  
  if(!is.null(replicates)){
    replicates <- replicates[which(replicates%in%1:100)]
    query <- paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_complete' AND replicate in (", paste(shQuote(replicates, type = "sh"),collapse = ', '),")"  )
    
    df <- .BIEN_sql(query, ...)
    
    tree <- read.tree(text = df$phylogeny, tree.names = df$replicate)
    
    return(tree)
    
  }
  
  
  set.seed(seed)  
  
  if(n_phylogenies > 100){
    message("n_phylogenies must be an integer between 1 and 100.  Setting n_phylogenies to 100")  
    n_phylogenies <- 100  
    
  }
  
  if(n_phylogenies < 1){
    message("n_phylogenies must be an integer between 1 and 100.  Setting n_phylogenies to 1")  
    n_phylogenies <- 1  
    
  }
  
  phylo_sample <- sample(x = 1:100, size = n_phylogenies, replace = FALSE)
  
  
  query <- paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_complete' AND replicate in (", paste(shQuote(phylo_sample, type = "sh"),collapse = ', '),")"  )
  
  df <- .BIEN_sql(query, ...)
  
  tree <- read.tree(text = df$phylogeny, tree.names = df$replicate)
  
  return(tree)
  
}

###############################
#'Download the conservative BIEN phylogeny
#'
#'BIEN_phylogeny_conservative downloads the conservative BIEN phylogeny, which only includes species with molecular data available.
#' @template phylogeny
#' @return A phylo object containing the BIEN conservative phylogeny
#' @examples \dontrun{
#' BIEN_phylo<-BIEN_phylogeny_conservative()}
#' @family phylogeny functions
#' @importFrom ape read.tree
#' @export
BIEN_phylogeny_conservative <- function(...){
  
  query <- paste("SELECT * FROM phylogeny WHERE phylogeny_version = 'BIEN_2016_conservative' ;"  )
  
  df <- .BIEN_sql(query, ...)
  
  tree <- read.tree(text = df$phylogeny,tree.names = df$replicate)
  
  return(tree)
  
}

#################################


#'Label nodes on a phylogeny
#'
#'BIEN_phylogeny_label_nodes will label the nodes on a phylogeny based on either the BIEN taxonomy or user-supplied taxa.
#' @param phylogeny A single phylogeny.
#' @param family Should family-level nodes be labeled?  Default is TRUE.
#' @param genus Should genus-level nodes be labeled?  Default is FALSE.  Overwrites family-level nodes where a family contains a single genera.
#' @param other_taxa A dataframe containing two columns: 1) the taxa to be labelled; 2) the species associated with each taxon.
#' @template phylogeny
#' @return Input phylogeny with labeled nodes.
#' @examples \dontrun{
#' phylogeny<-BIEN_phylogeny_conservative()
#'
#'phylogeny<-drop.tip(phy = phylogeny,tip = 101:length(phylogeny$tip.label))
#'plot.phylo(x = phylogeny,show.tip.label = FALSE)
#'
#'fam_nodes<-BIEN_phylogeny_label_nodes(phylogeny = phylogeny,family = TRUE)
#'plot.phylo(x = fam_nodes,show.tip.label = FALSE, show.node.label = TRUE)
#'
#'gen_nodes<-BIEN_phylogeny_label_nodes(phylogeny = phylogeny, family = FALSE, genus = TRUE)
#'plot.phylo(x = gen_nodes, show.tip.label = FALSE, show.node.label = TRUE)
#'
#'other_taxa <- as.data.frame(matrix(nrow = 10,ncol = 2))
#'colnames(other_taxa)<-c("taxon","species")
#'other_taxa$taxon[1:5]<-"A" #Randomly assign a few species to taxon A
#'other_taxa$taxon[6:10]<-"B" #Randomly assign a few species to taxon B
#'tax_nodes <- 
#'  BIEN_phylogeny_label_nodes(phylogeny = phylogeny,
#'                             family = FALSE, genus = FALSE, other_taxa = other_taxa)
#'plot.phylo(x = tax_nodes,show.tip.label = FALSE,show.node.label = TRUE)}
#' @family phylogeny functions
#' @importFrom ape getMRCA
#' @export
BIEN_phylogeny_label_nodes <- function(phylogeny,
                                       family = TRUE,
                                       genus = FALSE,
                                       other_taxa = NULL,
                                       ...){
  
  if(is.null(phylogeny$node.label)){
    phylogeny$node.label[1:phylogeny$Nnode] <- NA
  }
  
  taxonomy <- BIEN_taxonomy_species(species = gsub(pattern = "_",replacement = " ",x = phylogeny$tip.label))
  
  if(family == TRUE){
    for(i in 1:length(unique(taxonomy$scrubbed_family))){
      
      fam_i <- unique(taxonomy$scrubbed_family)[i]  
      spp_i <- taxonomy$scrubbed_species_binomial[which(taxonomy$scrubbed_family == fam_i)]
      mrca_i <- getMRCA(phy = phylogeny,
                             tip = which(phylogeny$tip.label %in% gsub(pattern = " ",replacement = "_", x = spp_i   ) )) 
      phylogeny$node.label[mrca_i-length(phylogeny$tip.label)] <- fam_i  
      
    }}
  
  if(genus == TRUE){
    for(i in 1:length(unique(taxonomy$scrubbed_genus))){
      
      gen_i <- unique(taxonomy$scrubbed_genus)[i]  
      spp_i <- taxonomy$scrubbed_species_binomial[which(taxonomy$scrubbed_genus == gen_i)]
      mrca_i <- getMRCA(phy = phylogeny,
                             tip = which(phylogeny$tip.label %in% gsub(pattern = " ",replacement = "_", x = spp_i   ) )) 
      phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]<-gen_i  
      
    }}
  
  
  if(!is.null(other_taxa)){
    for(i in 1:length(unique(other_taxa[,1]))){
      
      tax_i <- unique(other_taxa[,1])[i]  
      spp_i <- other_taxa[,2][which(other_taxa[,1]==tax_i)]
      mrca_i <- getMRCA(phy = phylogeny,
                             tip = which(phylogeny$tip.label %in% gsub(pattern = " ",replacement = "_", x = spp_i   ) )) 
      phylogeny$node.label[mrca_i-length(phylogeny$tip.label)]<-tax_i  
      
    }}
  
  
  return(phylogeny)  
  
  
  
}#end fx


#################################

#################################
#'Download the current BIEN database version and release date
#'
#'BIEN_metadata_database_version downloads the current version number and release date for the BIEN database.
#' @param ... Additional arguments passed to internal functions.
#' @return A data frame containing the current version number and release date for the BIEN database.
#' @family metadata functions
#' @examples \dontrun{
#' BIEN_metadata_database_version()}
#' @export
BIEN_metadata_database_version <- function(...){
  
  query <- "SELECT db_version, db_release_date FROM bien_metadata a JOIN (SELECT MAX(bien_metadata_id) as max_id FROM bien_metadata) AS b ON a.bien_metadata_id=b.max_id ;"
  .BIEN_sql(query, ...)
  
}


################################

################################
#'Check for differing records between old and new dataframes.
#'
#'BIEN_metadata_match_data compares old and new dataframes, and can check whether they are identical or be used to select rows that are unique to the old or new versions.
#' @param old A dataframe that is to be compared to a (typically) newer dataframe.
#' @param new A dataframe that is to be compared to a (typically) older dataframe.
#' @param return What information should be returned?  Current options are: "identical" (Logical, are the two dataframes identical?), "additions" (numeric, which rows are new?), "deletions" (numeric, which rows are no longer present?), "logical" (logical, which elements of the old dataframe are in the new one?).
#' @return Logical of varying length (depending on choice of "return" parameter)
#' @note Since comparisons are done by row (except when using return="identical"), this function may fail to flag additions or deletions if they are exact duplicates of existing rows.
#' @family metadata functions
#' @examples \dontrun{
#' new<-BIEN_occurrence_species("Acer nigrum")
#' old<-new[-1:-4,]#simulate having an older dataset by removing four rows
#' BIEN_metadata_match_data(old,new,return="identical")
#' BIEN_metadata_match_data(old,new,return="additions")}
#' @export
BIEN_metadata_match_data <- function(old,
                                     new,
                                     return = "identical"){
  
  if(return %in% c("identical","logical","additions","deletions")){
    
    old <- apply(old,MARGIN = 1,FUN = toString)  
    new <- apply(new,MARGIN = 1,FUN = toString)
    elements <- is.element(new,old)    
    
    if(return == "logical"){
      elements <- is.element(new,old)
      return(elements)  
    }#returns TRUE where elements are in the old set, false where they are not
    
    if(return == "additions"){
      elements <- is.element(new,old)
      return(which(elements == FALSE))  
    }#returns index of new elements
    
    if(return == "deletions"){
      elements <- is.element(old,new)
      return(which(elements == FALSE))  
    }#returns index of deleted elements
    
    if(return == "identical"){
      return(identical(old,new))
    }#returns true if identical, false otherwise
  }else{message("Please specify either 'identical','logical','additions' or 'deletions' for the value of the return argument")}
  
}

################################


#'Generate citations for data extracted from BIEN.
#'
#'BIEN_metadata_citation guides a user through the proper documentation for data downloaded from the BIEN database.
#' @param dataframe A data.frame of occurrence data downloaded from the BIEN R package.
#' @param trait.dataframe A data.frame of trait data downloaded from the BIEN R package.
#' @param trait.mean.dataframe A data.frame of species mean trait data from the function BIEN_trait_mean.
#' @param bibtex_file Output file for writing bibtex citations.
#' @param acknowledgement_file Output file for writing acknowledgements.
#' @param ... Additional arguments passed to internal functions.
#' @return A list object containing information needed for data attribution.  Full information for herbaria is available at http://sweetgum.nybg.org/science/ih/
#' @examples \dontrun{
#' BIEN_metadata_citation()#If you are referencing the phylogeny or range maps.
#' Xanthium_data<-BIEN_occurrence_species("Xanthium strumarium")
#' citations<-BIEN_metadata_citation(dataframe=Xanthium_data)#If you are referencing occurrence data}
#' @family metadata functions
#' @export
BIEN_metadata_citation <- function(dataframe = NULL,
                                   trait.dataframe = NULL,
                                   trait.mean.dataframe = NULL,
                                   bibtex_file = NULL,
                                   acknowledgement_file = NULL,
                                   ...){
  
  
  BIEN_cite <- '@ARTICLE{Enquist_undated-aw, title  = "Botanical big data shows that plant diversity in the New World is driven by climatic-linked differences in evolutionary rates and 
  biotic exclusion", author = "Enquist, B J and Sandel, B and Boyle, B and Svenning, J-C and McGill, B J and Donoghue, J C and Hinchliff, C E and Jorgensen, P M and Kraft, N J B and Marcuse-Kubitza, A and Merow, C and Morueta-Holme, N and Peet, R K and Schildhauer, M and Spencer, N and Regetz, J and Simova, I and Smith, S A and Thiers, B and Violle, C and Wiser, S K and Andelman, S and Casler, N and Condit, R and Dolins, S and Guaderrama, D and Maitner, B S and Narro, M L and Ott, J E and Phillips, O and Sloat, L L and ter   Steege, H"}'
  BIEN_cite <- gsub(pattern = "\n", replacement = "", BIEN_cite)
  
  R_package_cite <- '@article{doi:10.1111/2041-210X.12861,
  author = {Maitner Brian S. and Boyle Brad and Casler Nathan and Condit Rick and Donoghue John and Duran Sandra M. and Guaderrama Daniel and Hinchliff Cody E. and Jorgensen Peter M. and Kraft Nathan J.B. and McGill Brian and Merow Cory and Morueta-Holme Naia and Peet Robert K. and Sandel Brody and Schildhauer Mark and Smith Stephen A. and Svenning Jens-Christian and Thiers Barbara and Violle Cyrille and Wiser Susan and Enquist Brian J.},
  title = {The bien r package: A tool to access the Botanical Information and Ecology Network (BIEN) database},
  journal = {Methods in Ecology and Evolution},
  volume = {9},
  number = {2},
  pages = {373-379},
  keywords = {biodiversity, community plot, ecoinformatics, functional traits, herbarium records, occurrence, phylogeny, plants, presence, R, range maps},
  doi = {10.1111/2041-210X.12861},
  url = {https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.12861},
  eprint = {https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210X.12861},
  abstract = {Abstract There is an urgent need for large-scale botanical data to improve our understanding of community assembly, coexistence, biogeography, evolution, and many other fundamental biological processes. Understanding these processes is critical for predicting and handling human-biodiversity interactions and global change dynamics such as food and energy security, ecosystem services, climate change, and species invasions. The Botanical Information and Ecology Network (BIEN) database comprises an unprecedented wealth of cleaned and standardised botanical data, containing roughly 81 million occurrence records from c. 375,000 species, c. 915,000 trait observations across 28 traits from c. 93,000 species, and co-occurrence records from 110,000 ecological plots globally, as well as 100,000 range maps and 100 replicated phylogenies (each containing 81,274 species) for New World species. Here, we describe an r package that provides easy access to these data. The bien r package allows users to access the multiple types of data in the BIEN database. Functions in this package query the BIEN database by turning user inputs into optimised PostgreSQL functions. Function names follow a convention designed to make it easy to understand what each function does. We have also developed a protocol for providing customised citations and herbarium acknowledgements for data downloaded through the bien r package. The development of the BIEN database represents a significant achievement in biological data integration, cleaning and standardization. Likewise, the bien r package represents an important tool for open science that makes the BIEN database freely and easily accessible to everyone.}
}' 
  
  R_package_cite <- gsub(pattern = "\n",replacement = "",R_package_cite)
  
  
  
  
  
  if(!is.null(trait.dataframe)){
    trait.query<-paste("SELECT DISTINCT citation_bibtex,source_citation,source, url_source, access, project_pi, project_pi_contact FROM agg_traits 
                       WHERE id in (", paste(shQuote(as.integer(trait.dataframe$id), type = "sh"),collapse = ', '),") ;")
    trait.sources<-.BIEN_sql(trait.query, ...)}
  
  
  if(!is.null(trait.mean.dataframe)){
    
    ids<-paste(trait.mean.dataframe$ids,collapse = ",")
    ids<-unique(unlist(strsplit(x = ids,split = ",")))
    ids<-ids[which(ids!="NA")]
    
    trait.mean.query<-paste("SELECT DISTINCT citation_bibtex,source_citation,source, url_source, access, project_pi, project_pi_contact FROM agg_traits 
                            WHERE id in (", paste(shQuote(as.integer(ids), type = "sh"),collapse = ', '),") ;")
    
    trait.mean.sources <- .BIEN_sql(trait.mean.query, ...)
    #trait.mean.sources <- .BIEN_sql(trait.mean.query)
  }
  
  
  if(!is.null(trait.dataframe) & !is.null(trait.mean.dataframe)){
    trait.sources<- rbind(trait.sources,trait.mean.sources)
    trait.sources<-unique(trait.sources)
    
  }
  
  if(is.null(trait.dataframe) & !is.null(trait.mean.dataframe)){
    trait.sources <- trait.mean.sources
    
  }
  
  
  
  #########
  ##########
  #If an occurrence dataframe is supplied:
  
  if(!is.null(dataframe)){  
    
    datasources<-unique(dataframe$datasource_id[!is.na(dataframe$datasource_id)])
    
    query<-paste("WITH a AS (SELECT * FROM datasource where datasource_id in (", paste(shQuote(datasources, type = "sh"),collapse = ', '),")) 
                 SELECT * FROM datasource where datasource_id in (SELECT proximate_provider_datasource_id FROM a) OR datasource_id in (SELECT datasource_id FROM a) ;")
    
    sources<-.BIEN_sql(query, ...)
    
    citation<-list()
    citation[[1]]<-general<-"Public BIEN data are licensed via a CC-BY license.  Please see BIENdata.org for more information.
    The references in this list should be added to any publication using these data.  This is most easily done by specifying a bibtex_file and importing the bibtex formatted references into a reference manager.
    The acknowledgements in this list should be pasted into the acknowledgements of any resulting publications.
    Be sure to check for a 'data owners to contact' section in this list, as any authors listed there need to be contacted prior to publishing with their data."
    citation[[1]]<-gsub(pattern = "\n",replacement = "",citation[[1]])
    
    
    #Cleaning up the bibtex so that it loads properly into reference managers.  Better too many new lines than not enough...for some reason...
    
    dl_cites<-unique(sources$source_citation[which(!is.na(sources$source_citation))])
    if(!is.null(trait.dataframe)){dl_cites<-c(dl_cites,trait.sources$citation_bibtex)}
    dl_cites<-gsub(dl_cites,pattern = '"@',replacement = '@')
    dl_cites<-gsub(dl_cites,pattern = '" @',replacement = '@')
    dl_cites<-unique(dl_cites[which(!is.na(dl_cites))])
    
    citation[[2]]<-c(BIEN_cite,R_package_cite,dl_cites)
    citation[[2]]<-gsub(citation[[2]],pattern = "author", replacement = "\nauthor")
    citation[[2]]<-gsub(citation[[2]],pattern = "title", replacement = "\ntitle")
    citation[[2]]<-gsub(citation[[2]],pattern = "year", replacement = "\nyear")
    citation[[2]]<-gsub(citation[[2]],pattern = "organization", replacement = "\norganization")
    citation[[2]]<-gsub(citation[[2]],pattern = "address", replacement = "\naddress")
    citation[[2]]<-gsub(citation[[2]],pattern = "url", replacement = "\nurl")
    citation[[2]]<-gsub(citation[[2]],pattern = "journal", replacement = "\njournal")
    citation[[2]]<-gsub(citation[[2]],pattern = "note", replacement = "\nnote")
    citation[[2]]<-iconv(citation[[2]],to="ASCII//TRANSLIT")
    citation[[2]]<-gsub(citation[[2]],pattern = '\n}\"', replacement = '\n}')
    citation[[2]]<-gsub(citation[[2]],pattern = '\"\\\nurl', replacement = '\"\\url', fixed = TRUE)
    
    if(length(unique(sources$source_name[which(sources$is_herbarium==1)]))>0){
      citation[[3]]<-paste("We acknowledge the herbaria that contributed data to this work: ",
                           paste(unique(sources$source_name[which(sources$is_herbarium==1)])|>sort(),
                                 collapse = ", "),".",collapse = "",sep="")
    }
    if(length(unique(sources$source_name[which(sources$is_herbarium==1)]))==0){
      citation[[3]]<-data.frame()
    }
    
    citation[[4]]<-sources[which(sources$access_conditions=="contact authors"),]
    citation[[4]]<-citation[[4]][c('primary_contact_fullname','primary_contact_email','access_conditions','source_fullname','source_citation')]
    
    if(!is.null(trait.dataframe)){
      if(length(which(trait.sources$access=='public (notify the PIs)'))>0){
        
      ack_trait_sources<-trait.sources[which(trait.sources$access=='public (notify the PIs)'),]
      ack_trait_sources<-ack_trait_sources[c('project_pi','project_pi_contact','access','source_citation','citation_bibtex')]
      colnames(ack_trait_sources)<-c('primary_contact_fullname','primary_contact_email','access_conditions','source_fullname','source_citation')
      citation[[4]]<-rbind(citation[[4]],ack_trait_sources)
      }
    }
    
    
    names(citation)<-c("general information","references","acknowledgements","data owners to contact")
    
    
    #Write acknowledgements    
    if(nrow(citation[[4]])==0){citation[[4]]<-NULL}
    
    if(!is.null(acknowledgement_file)){
      
      if(length(unique(sources$source_name[which(sources$is_herbarium==1)]))>0){
        writeLines(text = citation$acknowledgements,con = acknowledgement_file)}else{
          message("No herbarium records found, not generating an herbarium acknowledgement file.")
          
        }  
      
    }
    
    #Write author contact warning and info        
    
    if("contact authors"%in%sources$access_conditions & is.null(trait.dataframe)){
      affected_datasource_id<-sources$datasource_id[which(sources$access_conditions=='contact authors')]
      n_affected_records<-length(which(dataframe$datasource_id%in%affected_datasource_id))
      pct_affected_records<-round(x =( n_affected_records/(length(which(!dataframe$datasource_id%in%affected_datasource_id))+n_affected_records))*100,digits = 2)
      
      n_affected_sources<-nrow(citation$`data owners to contact`)
      pct_affected_sources<-round(x = (n_affected_sources/nrow(sources))*100,digits = 2)
      
      message(paste("NOTE: You have references that require you to contact the data owners before publication.  This applies to ",
                    n_affected_records, " records (",pct_affected_records,"%) from ",n_affected_sources," sources (",pct_affected_sources,"%).",sep=""))
      
    }#if need to contact authors of a study
    
    if("contact authors"%in%sources$access_conditions & !is.null(trait.dataframe)){
      affected_datasource_id<-sources$datasource_id[which(sources$access_conditions=='contact authors')]
      #using author to identify datasource here.  Not perfect, but should generally work
      affected_trait__datasource_id<-trait.sources$project_pi_contact[which(trait.sources$access=='public (notify the PIs)')]
      
      n_affected_records<-length(which(dataframe$datasource_id%in%affected_datasource_id))+length(which(trait.dataframe$access%in%'public (notify the PIs)'))
      
      pct_affected_records<-round(x =( n_affected_records/(nrow(dataframe)+nrow(trait.dataframe) ))*100,digits = 2)
      
      n_affected_sources<-nrow(citation$`data owners to contact`)
      
      pct_affected_sources<-round(x = (n_affected_sources/(nrow(sources)+nrow(trait.sources)))*100,digits = 2)
      
      message(paste("NOTE: You have references that require you to contact the data owners before publication.  This applies to ",
                    n_affected_records, " records (",pct_affected_records,"%) from ",n_affected_sources," sources (",pct_affected_sources,"%).",sep=""))
      
    }#if need to contact authors of a study
    
  }  #if a dataframe is supplied
  
  ##########  
  
  #########  
  #If no dataframe or trait dataframe supplied  
  
  
  if(is.null(dataframe) & is.null(trait.dataframe)){
    
    citation<-list()
    citation[[1]]<-general<-"Public BIEN data are licensed via a CC-BY license.  Please see BIENdata.org for more information.
    The references in this list should be added to any publication using these data.  This is most easily done by specifying a bibtex_file and importing the bibtex formatted references into a reference manager.
    The acknowledgements in this list should be pasted into the acknowledgements of any resulting publications.
    Be sure to check for a 'data owners to contact' section in this list, as any authors listed there need to be contacted prior to publishing with their data."
    citation[[1]]<-gsub(pattern = "\n",replacement = "",citation[[1]])
    
    
    #Cleaning up the bibtex so that it loads properly into reference managers.  Better too many new lines than not enough...for some reason...
    citation[[2]]<-c(BIEN_cite,R_package_cite)
    citation[[2]]<-gsub(citation[[2]],pattern = "author", replacement = "\nauthor")
    citation[[2]]<-gsub(citation[[2]],pattern = "title", replacement = "\ntitle")
    citation[[2]]<-gsub(citation[[2]],pattern = "year", replacement = "\nyear")
    citation[[2]]<-gsub(citation[[2]],pattern = "organization", replacement = "\norganization")
    citation[[2]]<-gsub(citation[[2]],pattern = "address", replacement = "\naddress")
    citation[[2]]<-gsub(citation[[2]],pattern = "url", replacement = "\nurl")
    citation[[2]]<-gsub(citation[[2]],pattern = "journal", replacement = "\njournal")
    citation[[2]]<-gsub(citation[[2]],pattern = "note", replacement = "\nnote")
    citation[[2]]<-iconv(citation[[2]],to="ASCII//TRANSLIT")
    names(citation)<-c("general information","references")
    
  }#if dataframe is null
  
  #######
  #####  
  
  
  
  if((!is.null(trait.dataframe) |!is.null(trait.mean.dataframe)) & is.null(dataframe)){  
    citation<-list()
    citation[[1]]<-general<-"Public BIEN data are licensed via a CC-BY license.  Please see BIENdata.org for more information.
    The references in this list should be added to any publication using these data.  This is most easily done by specifying a bibtex_file and importing the bibtex formatted references into a reference manager.
    The acknowledgements in this list should be pasted into the acknowledgements of any resulting publications.
    Be sure to check for a 'data owners to contact' section in this list, as any authors listed there need to be contacted prior to publishing with their data."
    citation[[1]]<-gsub(pattern = "\n",replacement = "",citation[[1]])
    
    
    #Cleaning up the bibtex so that it loads properly into reference managers.  Better too many new lines than not enough...for some reason...
    if(!is.null(trait.dataframe)| !is.null(trait.mean.dataframe)){dl_cites<-c(trait.sources$citation_bibtex)}
    dl_cites<-gsub(dl_cites,pattern = '"@',replacement = '@')
    dl_cites<-gsub(dl_cites,pattern = '" @',replacement = '@')
    dl_cites<-unique(dl_cites[which(!is.na(dl_cites))])
    
    citation[[2]]<-c(BIEN_cite,R_package_cite,dl_cites)
    citation[[2]]<-gsub(citation[[2]],pattern = "author", replacement = "\nauthor")
    citation[[2]]<-gsub(citation[[2]],pattern = "title", replacement = "\ntitle")
    citation[[2]]<-gsub(citation[[2]],pattern = "year", replacement = "\nyear")
    citation[[2]]<-gsub(citation[[2]],pattern = "organization", replacement = "\norganization")
    citation[[2]]<-gsub(citation[[2]],pattern = "address", replacement = "\naddress")
    citation[[2]]<-gsub(citation[[2]],pattern = "url", replacement = "\nurl")
    citation[[2]]<-gsub(citation[[2]],pattern = "journal", replacement = "\njournal")
    citation[[2]]<-gsub(citation[[2]],pattern = "note", replacement = "\nnote")
    citation[[2]]<-iconv(citation[[2]],to="ASCII//TRANSLIT")
    citation[[2]]<-gsub(citation[[2]],pattern = '\n}\"', replacement = '\n}')
    citation[[2]]<-gsub(citation[[2]],pattern = '\"\\\nurl', replacement = '\"\\url', fixed = TRUE)
    citation[[3]]<-data.frame()
    ack_trait_sources<-trait.sources[which(trait.sources$access=='public (notify the PIs)'),]
    ack_trait_sources<-ack_trait_sources[c('project_pi','project_pi_contact','access','source_citation','citation_bibtex')]
    citation[[4]]<-ack_trait_sources
    colnames(citation[[4]])<-c('primary_contact_fullname','primary_contact_email','access_conditions','source_fullname','source_citation')
    
    
    names(citation)<-c("general information","references","acknowledgements","data owners to contact")
    
    
    #Write acknowledgements    
    
    #add code here if we decide to do trait acknowledgements
    
    
    if(nrow(citation[[4]])==0){citation[[4]]<-NULL}
    
    #Write author contact warning and info        
    
    if('public (notify the PIs)'%in%trait.sources$access){
      affected_trait__datasource_id<-trait.sources$project_pi_contact[which(trait.sources$access=='public (notify the PIs)')]
      
      n_affected_records<-length(which(trait.dataframe$access%in%'public (notify the PIs)'))
      
      pct_affected_records<-round(x =( n_affected_records/(nrow(trait.dataframe) ))*100,digits = 2)
      
      n_affected_sources<-nrow(citation$`data owners to contact`)
      
      pct_affected_sources<-round(x = (n_affected_sources/(nrow(trait.sources)))*100,digits = 2)
      
      message(paste("NOTE: You have references that require you to contact the data owners before publication.  This applies to ",
                    n_affected_records, " records (",pct_affected_records,"%) from ",n_affected_sources," sources (",pct_affected_sources,"%).",sep=""))
      
    }#if need to contact authors of a study
    
  }  #if only a trait dataframe is supplied  
  
  
  
  
  #######    
  ######  
  #Write bibtex output  
  if(!is.null(bibtex_file)){
    
    writeLines(text = citation[[2]],con=bibtex_file)  
    
    
  }
  
  #Return the citation list  
  
  return(citation)    
  
  }

#####################


#'List political divisions and associated geonames codes.
#'
#'BIEN_metadata_list_political_names downloads country, state, and county names and associated codes used by BIEN.
#' @param ... Additional arguments passed to internal functions.
#' @return A dataframe containing political division names and their associated codes.
#' @note Political names and codes follow http://www.geonames.org/
#' @examples \dontrun{
#' BIEN_metadata_list_political_names()}
#' @family metadata functions
#' @export
BIEN_metadata_list_political_names <- function(...){
  
  query<-'SELECT country,country_iso, state_province, state_province_ascii,state_province_code AS "state_code",
  county_parish,county_parish_ascii,county_parish_code AS "county_code" FROM county_parish ;'
  
  .BIEN_sql(query, ...)
  
}

####################

#'Download data dictionaries
#'
#'BIEN_metadata_data_dictionaries downloads the data dictionaries for the BIEN database.
#' @param ... Additional arguments passed to internal functions.
#' @return A list containing data.frames containing different data dictionaries.
#' "Tables" contains information on the contents of tables.
#' "Columns" contains information on the contents of columns within tables.
#' "Values" contains information on some of the values that are used to populated columns, particularly where these may be unclear.
#' "rbien" contains information on some of the values returned by the BIEN R package.
#' @family metadata functions
#' @examples \dontrun{
#' BIEN_metadata_data_dictionaries()}
#' @keywords internal
BIEN_metadata_data_dictionaries <- function(...){
  
  tables <- .BIEN_sql("SELECT * from data_dictionary_tables",...)
  columns <- .BIEN_sql("SELECT * from data_dictionary_columns",...)
  values <- .BIEN_sql("SELECT * from data_dictionary_values",...)
  rbien <- .BIEN_sql("SELECT * from data_dictionary_rbien",...)
  
  
  
  data_dictionary <- list(tables =tables,
                          columns = columns,
                          values = values,
                          rbien = rbien)
  
  
  return(data_dictionary)
  
}



################################

############################
#Stem functions

#'Extract stem data for specified species from BIEN
#'
#'BIEN_stem_species downloads occurrence records for specific species from the BIEN database.
#' @param species A single species, or a vector of species.  Genus and species should be separated by a space. Genus should be capitalized.
#' @template stem
#' @return Dataframe containing stem data for the specified species.
#' @note Setting either "cultivated" or "native.status" to TRUE will significantly slow the speed of a query.
#' @examples \dontrun{
#' BIEN_stem_species("Abies amabilis")
#' species_vector<-c("Abies amabilis", "Acer nigrum")
#' BIEN_stem_species(species_vector)
#' BIEN_stem_species(species_vector,all.taxonomy = TRUE)}
#' @family stem functions
#' @export
BIEN_stem_species <- function(species,
                              cultivated = FALSE,
                              new.world = NULL,
                              all.taxonomy = FALSE,
                              native.status = FALSE,
                              natives.only = TRUE,
                              political.boundaries = FALSE,
                              collection.info = FALSE,
                              all.metadata = FALSE,
                              ...){

  .is_log(all.metadata)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(species)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  
  #set conditions for query
    cultivated_ <- .cultivated_check_stem(cultivated)
    newworld_ <- .newworld_check_stem(new.world)
    taxonomy_ <- .taxonomy_check_stem(all.taxonomy)
    native_ <- .native_check_stem(native.status)
    natives_ <- .natives_check_stem(natives.only)
    political_ <- .political_check_stem(political.boundaries)
    collection_ <- .collection_check_stem(collection.info)
    vfoi_ <- .vfoi_check_stem(native.status,cultivated,natives.only,collection.info)
    md_ <- .md_check_stem(all.metadata)
  
  # set the query
    query <- paste("SELECT analytical_stem.scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," ,analytical_stem.latitude, 
                        analytical_stem.longitude,analytical_stem.date_collected, analytical_stem.relative_x_m, analytical_stem.relative_y_m, 
                        analytical_stem.taxonobservation_id,analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, 
                        plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,analytical_stem.custodial_institution_codes,
                        analytical_stem.collection_code,analytical_stem.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                   FROM 
                      (SELECT * FROM analytical_stem WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                      JOIN plot_metadata ON 
                      (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                      vfoi_$join ," 
                   WHERE analytical_stem.scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",
                      cultivated_$query,newworld_$query,natives_$query,  "AND analytical_stem.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                      AND (analytical_stem.is_geovalid = 1) AND (analytical_stem.georef_protocol is NULL OR analytical_stem.georef_protocol<>'county centroid') 
                      AND (analytical_stem.is_centroid IS NULL OR analytical_stem.is_centroid=0) ;")
  
  return(.BIEN_sql(query, ...))
  
  }


#######################

#'Extract stem data for specified families from BIEN
#'
#'BIEN_stem_family downloads occurrence records for specific families from the BIEN database.
#' @param family A single family, or a vector of families. Families should be capitalized.
#' @template stem
#' @return Dataframe containing stem data for the specified families.
#' @note Setting either "cultivated" or "native.status" to TRUE will significantly slow the speed of a query.
#' @examples \dontrun{
#' BIEN_stem_family(family = "Marantaceae")
#' family_vector<-c("Marantaceae", "Buxaceae")
#' BIEN_stem_family(family = family_vector)
#' BIEN_stem_family(family = family_vector, all.taxonomy = TRUE, native.status = TRUE)}
#' @family stem functions
#' @export
BIEN_stem_family <- function(family,
                             cultivated = FALSE,
                             new.world = NULL,
                             all.taxonomy = FALSE,
                             native.status = FALSE,
                             natives.only = TRUE,
                             political.boundaries = FALSE,
                             collection.info = FALSE,
                             all.metadata = FALSE,
                             ...){

  .is_log(all.metadata)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(family)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)

  #set conditions for query
  cultivated_<-.cultivated_check_stem(cultivated)
  newworld_<-.newworld_check_stem(new.world)
  taxonomy_<-.taxonomy_check_stem(all.taxonomy)
  native_<-.native_check_stem(native.status)
  natives_<-.natives_check_stem(natives.only)
  political_<-.political_check_stem(political.boundaries)
  collection_<-.collection_check_stem(collection.info)
  vfoi_<-.vfoi_check_stem(native.status,cultivated,natives.only,collection.info)
  md_<-.md_check_stem(all.metadata)
  
  # set the query
  query <- paste("SELECT analytical_stem.scrubbed_family, analytical_stem.scrubbed_genus,analytical_stem.scrubbed_species_binomial",taxonomy_$select,native_$select,
                      political_$select,", analytical_stem.latitude, analytical_stem.longitude,analytical_stem.date_collected,analytical_stem.relative_x_m, 
                      analytical_stem.relative_y_m, analytical_stem.taxonobservation_id, analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, 
                      plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,analytical_stem.custodial_institution_codes,
                      analytical_stem.collection_code,analytical_stem.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM analytical_stem WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                 JOIN plot_metadata ON 
                 (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                 vfoi_$join ," 
                 WHERE analytical_stem.scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")",
                    cultivated_$query,newworld_$query,natives_$query,  "AND analytical_stem.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                    AND (analytical_stem.is_geovalid = 1) AND (analytical_stem.georef_protocol is NULL OR analytical_stem.georef_protocol<>'county centroid') 
                    AND (analytical_stem.is_centroid IS NULL OR analytical_stem.is_centroid=0) ;")
  
  return(.BIEN_sql(query, ...))
  
  }

#######################################

#'Extract stem data for specified genera from BIEN
#'
#'BIEN_stem_genus downloads occurrence records for specific genera from the BIEN database.
#' @param genus A single genus, or a vector of genera. Genera should be capitalized.
#' @template stem
#' @return Dataframe containing stem data for the specified genera.
#' @note Setting either "cultivated" or "native.status" to TRUE will significantly slow the speed of a query.
#' @examples \dontrun{
#' BIEN_stem_genus(genus = "Tovomita")
#' genus_vector<-c("Tovomita", "Myrcia")
#' BIEN_stem_genus(genus = genus_vector)
#' BIEN_stem_genus(genus = genus_vector, all.taxonomy = TRUE)}
#' @family stem functions
#' @export
BIEN_stem_genus <- function(genus,
                           cultivated = FALSE,
                            new.world = NULL,
                            all.taxonomy = FALSE,
                            native.status = FALSE,
                            natives.only = TRUE,
                            political.boundaries = FALSE,
                            collection.info = FALSE,
                            all.metadata = FALSE,
                            ...){

  .is_log(all.metadata)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(genus)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  
  #set conditions for query
    cultivated_ <- .cultivated_check_stem(cultivated)
    newworld_ <- .newworld_check_stem(new.world)
    taxonomy_ <- .taxonomy_check_stem(all.taxonomy)
    native_ <- .native_check_stem(native.status)
    natives_ <- .natives_check_stem(natives.only)
    political_ <- .political_check_stem(political.boundaries)
    collection_ <- .collection_check_stem(collection.info)
    vfoi_ <- .vfoi_check_stem(native.status,cultivated,natives.only,collection.info)
    md_ <- .md_check_stem(all.metadata)
  
  # set the query
  query <- paste("SELECT analytical_stem.scrubbed_genus,analytical_stem.scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," , 
                      analytical_stem.latitude, analytical_stem.longitude,analytical_stem.date_collected, analytical_stem.relative_x_m, analytical_stem.relative_y_m, 
                      analytical_stem.taxonobservation_id, analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, plot_metadata.dataset,
                      plot_metadata.datasource,plot_metadata.dataowner, analytical_stem.custodial_institution_codes, analytical_stem.collection_code, 
                      analytical_stem.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM analytical_stem WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                 JOIN plot_metadata ON 
                    (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                 vfoi_$join ," 
                 WHERE analytical_stem.scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",
                    cultivated_$query,newworld_$query,natives_$query, "AND analytical_stem.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                    AND (analytical_stem.is_geovalid = 1) AND (analytical_stem.georef_protocol is NULL OR analytical_stem.georef_protocol<>'county centroid') 
                    AND (analytical_stem.is_centroid IS NULL OR analytical_stem.is_centroid=0) ;")
  
  return(.BIEN_sql(query, ...))
  
  }

###########

#'Extract stem data for a given datasource from BIEN
#'
#'BIEN_stem_datasource downloads occurrence records for specific datasources from the BIEN database.
#' @param datasource A single datasource, or a vector of datasources.
#' @template stem
#' @return Dataframe containing stem data for the specified datasource.
#' @note Setting either "cultivated" or "native.status" to TRUE will significantly slow the speed of a query.
#' @note #' @note For a list of available datasources, use \code{\link{BIEN_plot_list_datasource}}.
#' @examples \dontrun{
#' BIEN_stem_datasource(datasource = "SALVIAS")}
#' @family stem functions
#' @export
BIEN_stem_datasource <- function(datasource,
                                 cultivated = FALSE,
                                 new.world = NULL,
                                 all.taxonomy = FALSE,
                                 native.status = FALSE,
                                 natives.only = TRUE,
                                 political.boundaries = FALSE,
                                 collection.info = FALSE,
                                 all.metadata = FALSE,
                                 ...){

  .is_log(all.metadata)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(datasource)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
    
  #set conditions for query
    cultivated_<-.cultivated_check_stem(cultivated)
    newworld_<-.newworld_check_stem(new.world)
    taxonomy_<-.taxonomy_check_stem(all.taxonomy)
    native_<-.native_check_stem(native.status)
    natives_<-.natives_check_stem(natives.only)
    political_<-.political_check_stem(political.boundaries)
    collection_<-.collection_check_stem(collection.info)
    vfoi_<-.vfoi_check_stem(native.status,cultivated,natives.only,collection.info)
    md_<-.md_check_stem(all.metadata)
  
  # set the query
  query <- paste("SELECT analytical_stem.plot_name,analytical_stem.subplot, analytical_stem.elevation_m, analytical_stem.plot_area_ha,analytical_stem.sampling_protocol,
                      analytical_stem.recorded_by,analytical_stem.scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," ,analytical_stem.latitude, 
                      analytical_stem.longitude,analytical_stem.date_collected,analytical_stem.relative_x_m, analytical_stem.relative_y_m, analytical_stem.taxonobservation_id, 
                      analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,
                      analytical_stem.custodial_institution_codes, analytical_stem.collection_code,analytical_stem.datasource_id",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM analytical_stem WHERE datasource in (", paste(shQuote(datasource, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                 JOIN plot_metadata ON 
                    (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                 vfoi_$join ," 
                 WHERE analytical_stem.datasource in (", paste(shQuote(datasource, type = "sh"),collapse = ', '), ")",
                    cultivated_$query,newworld_$query,native_$query,  "AND analytical_stem.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                    AND (analytical_stem.is_geovalid = 1) AND (analytical_stem.georef_protocol is NULL OR analytical_stem.georef_protocol<>'county centroid') 
                    AND (analytical_stem.is_centroid IS NULL OR analytical_stem.is_centroid=0) ;")
  
  return(.BIEN_sql(query, ...))
  
  }


##########################


#'Download stem data using a specified sampling protocol.
#'
#'BIEN_stem_sampling_protocol downloads plot-based stem data using a specified sampling protocol.
#' @param sampling_protocol A sampling protocol or vector of sampling protocols. See \code{\link{BIEN_plot_list_sampling_protocols}} for options.
#' @template stem
#' @return A dataframe containing all data from the specified sampling protocol.
#' @examples \dontrun{
#' BIEN_stem_sampling_protocol("Point-intercept")}
#' @family stem functions
#' @export
BIEN_stem_sampling_protocol <- function(sampling_protocol,
                                        cultivated = FALSE,
                                        new.world = NULL,
                                        all.taxonomy = FALSE,
                                        native.status = FALSE,
                                        natives.only = TRUE,
                                        political.boundaries = FALSE,
                                        collection.info = FALSE,
                                        all.metadata = FALSE,
                                        ...){

  .is_log(all.metadata)
  .is_log(cultivated)
  .is_log_or_null(new.world)
  .is_log(all.taxonomy)
  .is_char(sampling_protocol)
  .is_log(native.status)
  .is_log(natives.only)
  .is_log(political.boundaries)
  .is_log(collection.info)
  
  #set conditions for query
    cultivated_<-.cultivated_check_stem(cultivated)
    newworld_<-.newworld_check_stem(new.world)
    taxonomy_<-.taxonomy_check_stem(all.taxonomy)
    native_<-.native_check_stem(native.status)
    natives_<-.natives_check_stem(natives.only)
    political_<-.political_check_stem(political.boundaries)
    collection_<-.collection_check_stem(collection.info)
    vfoi_<-.vfoi_check_stem(native.status,cultivated,natives.only,collection.info)
    md_<-.md_check_stem(all.metadata)
  
  # set the query
  query <- paste("SELECT analytical_stem.scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," ,analytical_stem.latitude, analytical_stem.longitude,analytical_stem.date_collected,
                    analytical_stem.relative_x_m, analytical_stem.relative_y_m, analytical_stem.taxonobservation_id,analytical_stem.stem_code, analytical_stem.stem_dbh_cm, analytical_stem.stem_height_m, 
                    plot_metadata.dataset,plot_metadata.datasource,plot_metadata.dataowner,analytical_stem.custodial_institution_codes,
                    analytical_stem.collection_code,analytical_stem.datasource_id,view_full_occurrence_individual.plot_name,view_full_occurrence_individual.subplot, 
                    view_full_occurrence_individual.elevation_m, view_full_occurrence_individual.plot_area_ha, 
                    view_full_occurrence_individual.sampling_protocol,view_full_occurrence_individual.recorded_by, view_full_occurrence_individual.individual_count",collection_$select,cultivated_$select,newworld_$select,md_$select,"
                 FROM 
                    (SELECT * FROM analytical_stem WHERE sampling_protocol in (", paste(shQuote(sampling_protocol, type = "sh"),collapse = ', '), ")) AS analytical_stem 
                 JOIN plot_metadata ON 
                    (analytical_stem.plot_metadata_id= plot_metadata.plot_metadata_id)",
                 vfoi_$join ," 
                 WHERE analytical_stem.sampling_protocol in (", paste(shQuote(sampling_protocol, type = "sh"),collapse = ', '), ")",
                    cultivated_$query,newworld_$query,natives_$query,  "AND analytical_stem.higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
                    AND (analytical_stem.is_geovalid = 1) AND (analytical_stem.georef_protocol is NULL OR analytical_stem.georef_protocol<>'county centroid') 
                    AND (analytical_stem.is_centroid IS NULL OR analytical_stem.is_centroid=0) ;")
  
  return(.BIEN_sql(query, ...))
  
}

#######################################