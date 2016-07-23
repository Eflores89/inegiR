

inegi_route <- function(from, to, route_type = 2, vehicle = 1, axis = 0, type = 1, token){
  if(type == 1){
    # linea a linea
    if(length(from)==2){}else{stop("Length of from parameter must be 2. A vector with source id and target id, in that order")}
    if(length(to)==2){}else{stop("Length of to parameter must be 2. A vector with source id and target id, in that order")}
    
    q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR
                &id_i=1
                &source_i=", 
                from[1], 
                "&target_i=",
                from[2],
                "&id_f=", from[2],
                "&source_f=", to[1],
                "&target_f=", to[2],
                "&p=", route_type,
                "&v=", vehicle,
                "&e=", axis,
                "&type=xml
                  &key=", token)
    m <- "line to line"
  }else{
    if(type == 2){
      q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR",
                  "&dest_i=",from, 
                  "&dest_f=", to, 
                  "&p=", route_type, 
                  "&v=", vehicle, 
                  "&e=", axis,
                  "&type=xml&key=",token)
      m <- "destiny to destiny"
    }else{
      
      if(type == 3){
        
      }else{
        if(type == 4){
          
        }else{stop("Type of query unrecognized. Use 1 through 4.")}
      }
    }
  }
  d <- XML::xmlToList(q)
  df <- data.frame("ROUTE_STRING" = paste0("Route, ", m, 
                                           " id: ", from[1], 
                                           " to id: ", to[1]),
                   "SQL_STRING" = ifelse(is.null(d$ruta$sql), NA, as.character(d$ruta$sql)), 
                   "ROUTE_LENGTH" = ifelse(is.null(d$ruta$long_km), NA, as.numeric(d$ruta$long_km)), 
                   "ROUTE_TIME" = ifelse(is.null(d$ruta$tiempo_min), NA, as.numeric(d$ruta$tiempo_min)),
                   "ROUTE_TOLL" = ifelse(is.null(d$ruta$peaje), NA, 
                                         ifelse(as.character(d$ruta$peaje)=="t", TRUE,FALSE)),
                   "ROUTE_COST" = ifelse(is.null(d$ruta$costo_caseta), NA, as.numeric(d$ruta$costo_caseta)),
                   "ROUTE_EXCEED" = ifelse(is.null(d$ruta$eje_excedente), NA, as.numeric(d$ruta$eje_excedente))
                   )
  l <- list("METADATA" = df, 
            "GEO" = jsonlite::fromJSON(as.character(d$ruta$geojson)))
  return(l)
}
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
#' Returns INEGI line ID
#'
#' Returns data.frame with id's that match coordinates. 
#'
#' @param latitud latitud vector in decimals
#' @param longitud longitud vector in decimals
#' @param scale scale of map visualization (1:scale)
#' @param keep_trying if TRUE the function will start from scale downwards by a degree of 10 until a line is reached
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
#' line_ids <- inegi_lines(latitud, longitud, token = token)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom XML xmlToList
#' @importFrom plyr ldply
#' @export
inegi_lines <- function(latitud, longitud, scale = 100000, keep_trying = TRUE, token){
  
  if(keep_trying){
  d <- NULL
   while(is.null(d$linea)){
    print(paste0("Using scale: ", scale))
     if(scale<1){d$linea <- NA # if already extended all attempts
     }else{ 
    q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=IL",
                "&x=", latitud,
                "&y=", longitud, 
                "&escala=", scale, 
                "&type=xml&key=", token)
    
    d <- XML::xmlToList(q)
    scale <- scale/10
     }
   }
  }else{
    print(paste0("Using scale: ", scale))
    q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=IL",
                "&x=", latitud,
                "&y=", longitud, 
                "&escala=", scale, 
                "&type=xml&key=", token)
    
    d <- XML::xmlToList(q)
  }
  
  if(is.null(d$linea)){stop("Nothing returned. Try with different scale or keep_trying = true")}
  if(suppressWarnings(is.na(d$linea))){stop("Nothing returned. Try with different scale or keep_trying = true")}
  
  df <- as.data.frame(d$linea)
  print(df)
  #geolist <- apply(X = df$geojson, 
   #                MARGIN = 1, inegiR::ext_geo)
  #df$GEO_TYPE <- ldply(geolist, "[")['TYPE']
  #df$LAT <- ldply(geolist, "[")['LAT']
  #df$LONG <- ldply(geolist, "[")['LONG']
  
  df
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
