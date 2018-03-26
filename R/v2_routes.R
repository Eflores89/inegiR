#' Returns INEGI destiny id's with matching searches
#'
#' Returns data.frame with id's and coordinates that match with the API names. 
#'
#' @param search Character vector to search for
#' @param token API token supplied by INEGI
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' # All id's in Monterrey, Mex.
#' \dontrun{
#' token <- "webservice_token"
#' dest_ids <- inegi_destiny("monterrey", token)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom XML xmlToList
#' @importFrom plyr ldply
#' @export
inegi_destiny <- function(search, token){
  dest <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=SD&buscar=", search,
              "&type=xml&key=", token)
  s <- XML::xmlToList(dest)
  d <- plyr::ldply(s$destinos, as.data.frame)
  names(d) <- c("ID", "ID_DEST", "STATE", 
                "NAME", "GEO_STRING")
  
  geolist <- apply(X = d['GEO_STRING'], 
                        MARGIN = 1, inegiR::ext_geo)
  
  d$GEO_TYPE <- ldply(geolist, "[")['TYPE']
  d$LAT <- ldply(geolist, "[")['LAT']
  d$LONG <- ldply(geolist, "[")['LONG']
  
  return(d)
}
#' Extracts INEGI GeoJSON 
#'
#' Helper function
#'
#' @param x GeoJSON description
#' 
#' @return Data.frame
#'
#' @importFrom jsonlite fromJSON
#' @export
ext_geo <- function(x){
  l <- jsonlite::fromJSON(as.character(x))
  data.frame("TYPE" = as.character(l$type), 
             "LAT" = l$coordinates[1],
             "LONG" = l$coordinates[2])
}
