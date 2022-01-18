#' @param cultivated Return known cultivated records as well?  Default is FALSE.
#' @param new.world NULL (The default) returns global records, TRUE returns only New World, and FALSE only Old World.
#' @param all.taxonomy Return all taxonomic information?  This includes the raw data as well as the "scrubbed" data.
#' @param native.status Return information on introduction status?  The default value is FALSE. A value of TRUE also returns additional information on introduction status.
#' @param natives.only Exclude detected introduced species?  Default is TRUE.
#' @param observation.type Return information on type of observation (i.e. specimen vs. plot)?  The default value is FALSE.
#' @param political.boundaries Return information on political boundaries for an observation? The default value is FALSE.
#' @param collection.info Return additional information about collection and identification? The default value is FALSE.
#' @param ... Additional arguments passed to internal functions.
#' @note US FIA coordinates have been fuzzed and swapped, for more details see: https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php
