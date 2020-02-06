#'Run an SQL query on the BIEN database.
#'
#'.BIEN_sql is an internal function used to submit SQL queries.
#' @param query A PostgreSQL query.
#' @param view_full_occurrence_individual Alternative value to be substituted for "view_full_occurrence_individual" in queries when not NULL.
#' @param agg_traits Alternative value to be substituted for "agg_traits" in queries when not NULL.
#' @param species_by_political_division Alternative value to be substituted for "species_by_political_division" in queries when not NULL.
#' @param bien_species_all Alternative value to be substituted for "bien_species_all" in queries when not NULL.
#' @param ranges Alternative value to be substituted for "ranges" in queries when not NULL.
#' @param bien_taxonomy Alternative value to be substituted for "bien_taxonomy" in queries when not NULL.
#' @param phylogeny Alternative value to be substituted for "phylogeny" in queries when not NULL.
#' @param bien_metadata Alternative value to be substituted for "bien_metadata" in queries when not NULL.
#' @param plot_metadata Alternative value to be substituted for "plot_metadata" in queries when not NULL.
#' @param analytical_stem Alternative value to be substituted for "analytical_stem" in queries when not NULL.
#' @param datasource Alternative value to be substituted for "datasource" in queries when not NULL.
#' @param centroid Alternative value to be substituted for "centroid" in queries when not NULL.
#' @param limit A limit on the number of records to be returned.  Should be a single number or NULL (the default).
#' @param return.query Should  the query used be returned rather than executed?  Default is FALSE
#' @param schema An alternative schema to be accessed.  Used for testing purposes.
#' @param print.query Should  the query used be printed?  Default is FALSE
#' @import RPostgreSQL
#' @importFrom DBI dbDriver
#' @return A dataframe returned by the query.
#' @keywords internal
#' @examples \dontrun{
#' .BIEN_sql("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual 
#' WHERE country in ( 'United States' );")}
.BIEN_sql<-function(query,view_full_occurrence_individual=NULL,agg_traits=NULL,species_by_political_division=NULL,
                    bien_species_all=NULL,ranges=NULL,bien_taxonomy=NULL,phylogeny=NULL,bien_metadata=NULL,plot_metadata=NULL,
                    analytical_stem=NULL,datasource=NULL,centroid=NULL,limit=NULL,return.query=FALSE,schema=NULL,print.query=FALSE){
  .is_char(query)

  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  if(!is.null(schema)){
    view_full_occurrence_individual<-paste(schema,"view_full_occurrence_individual",sep = ".")
    agg_traits <- paste(schema,"agg_traits",sep = ".")
    species_by_political_division <- paste(schema,"species_by_political_division",sep = ".")
    bien_species_all <- paste(schema,"bien_species_all",sep = ".")
    ranges <- paste(schema,"ranges",sep = ".")
    bien_taxonomy <- paste(schema,"bien_taxonomy",sep = ".")
    phylogeny <- paste(schema,"phylogeny",sep = ".")
    bien_metadata <- paste(schema,"bien_metadata",sep = ".")
    plot_metadata <- paste(schema,"plot_metadata",sep = ".")
    analytical_stem <- paste(schema,"analytical_stem",sep = ".")
    datasource <- paste(schema,"datasource",sep = ".")
    centroid <- paste(schema,"centroid",sep = ".")
  }
  
  
  if(!is.null(view_full_occurrence_individual)){
    #query<-gsub(pattern = "\\<view_full_occurrence_individual\\>",replacement = view_full_occurrence_individual,x = query)}
    query<-gsub(pattern = "(?<!as |AS )(?<!\\S)view_full_occurrence_individual(?!\\S)",replacement = view_full_occurrence_individual,x = query,perl = T)}
  
  
  if(!is.null(plot_metadata)){
    #query<-gsub(pattern = "\\<view_full_occurrence_individual\\>",replacement = view_full_occurrence_individual,x = query)}
    query<-gsub(pattern = "(?<!as |AS )(?<!\\S)plot_metadata(?!\\S)",replacement = plot_metadata,x = query,perl = T)}
  
  if(!is.null(analytical_stem)){
    #query<-gsub(pattern = "\\<view_full_occurrence_individual\\>",replacement = view_full_occurrence_individual,x = query)}
    query<-gsub(pattern = "(?<!as |AS )(?<!\\S)analytical_stem(?!\\S)",replacement = analytical_stem,x = query,perl = T)}
  
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
    query<-gsub(pattern = "\\<bien_metadata\\>",replacement = bien_metadata,x = query)}
  
  
  if(!is.null(datasource)){
    query<-gsub(pattern = "(?<=\\s)datasource(?=\\s)",replacement = datasource,x = query,perl = T)}  
  
  if(!is.null(centroid)){
    query<-gsub(pattern = "(?<=\\s)centroid(?=\\s)",replacement = datasource,x = query,perl = T)}  
  
  
  if(!is.null(limit)){
    query<-gsub(pattern = " ;",replacement = paste(" LIMIT ",limit,";"),x = query)}
 
  
  
  
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  
  if(return.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    dbDisconnect(con)
    return(query)
  }
  
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  dbDisconnect(con)
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  return(df)
  
}
