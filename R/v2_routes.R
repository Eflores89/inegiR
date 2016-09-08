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

#' Returns INEGI closest line id's with matching searches
#'
#' Returns data.frame with id's, source, target, name and geojson
#' adjunsted to the clostest line to that lat/lon point
#'
#' @param latitud Latitud value to search a near line
#' @param longitud Longitud value to search a near line
#' @param scale Reference visualization map scale
#' @param keep_trying Exaustive search by decreacing the scale
#' @param token Sakbe Token obtained from INEGI
#' 
#' @return Data.frame
#'
#' @author Arturo Cardenas 
#' 
#' @examples
#' # Line closest to the Mexican History Museum in Monterrey, Mexico
#' \dontrun{
#' token <- "webservice_token"
#' from <- inegi_lines(25.671528, -100.306492, scale=10000000,
#'                    keep_trying = TRUE, token = token)
#' }
#'
#' @importFrom XML xmlToList
#' @export
inegi_lines <- function(latitud, longitud, scale = 100000, keep_trying = TRUE, token){
  options(scipen = 999)
  if(keep_trying){
    d <- NULL
    while(is.null(d$linea)){
      print(paste0("Using scale: ", scale))
      if(scale<1){d$linea <- NA # if already extended all attempts
      }else{ 
        q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=IL",
                    "&x=", longitud,
                    "&y=", latitud, 
                    "&escala=", scale, 
                    "&type=xml&key=", token)
        d <- XML::xmlToList(q)
        scale <- scale/10
      }
    }
  }else{
    print(paste0("Using scale: ", scale))
    q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=IL",
                "&x=", longitud,
                "&y=", latitud, 
                "&escala=", scale, 
                "&type=xml&key=", token)
    d <- XML::xmlToList(q)
  }
  if(is.null(d$linea)){stop("Nothing returned. Try with different scale or keep_trying = true")}
  if(suppressWarnings(is.na(d$linea))){stop("Nothing returned. Try with different scale or keep_trying = true")}
  df <- as.data.frame(d$linea)
  print(df)
  return(df)
}

#' Returns route between two INEGI lines obtained using inegi_lines()
#'
#' Returns list with route geometry (geojson) and metadata
#' of the route like:  
#' ROUTE_LENGTH: Route lenght in KMs
#' ROUTE_TIME: Route time lenght in min
#' ROUTE_TOLL: Logical value indicating if there is a toll to pay or not   
#' ROUTE_COST: If ROUTE_TOLL = TRUE it returns the cost of the tolls
#' ROUTE_EXCEED: Returns the extra cost of driving vehicles with more than 2 axis  
#'
#' @param from_io Latitud/Longitud of initial route point (it will look for the closest line)
#' @param to_io Latitud/Longitud of final route point (it will look for the closest line)
#' @param route_type 0 for Toll Free Route, 1 for Preferiably Toll Route and 2 for suggested route (default value = 2)
#' @param vehicle default value is 1 for regular vehicle
#' @param axis number of extra axis being pull by the vehicle
#' @param type Type of initial and final points format: 
#' 1: line to line (using lat/long), 
#' 2: destiny to destiny (using landmark's names)
#' 3: line to destiny (using lat/long for first point and landmark's name for second point)
#' 4: destiny to line (using landmark's name for first point and lat/long for second point )
#' @param token Sakbe Token
#' 
#' @return List
#'
#' @author Arturo Cardenas 
#' 
#' @examples
#' # Line closest to the Mexican History Museum in Monterrey, Mexico
#' \dontrun{
#' token <- "webservice_token"
#' p1=c(25.671528, -100.306492) #Mexican History Museum in Monterrey, Mexico
#' p2=c(25.650443,-100.289725) #ITESM Campus Monterrey
#' test <- inegi_route(p1,p2,token=token)
#' mapa <- do.call("rbind",test$GEO$coordinates)
#' library(leaflet)
#' library(dplyr)
#' map <- leaflet() %>%
#'    addTiles() %>%
#'   setView(p1[2], p1[1], zoom = 13)  %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolylines(data = mapa, lng = mapa[,1], lat = mapa[,2])
#' map
#' }
#'
#' @importFrom XML xmlToList
#' @importFrom jsonlite fromJSON
#' @export
inegi_route <- function(from_io, to_io, route_type = 2, vehicle = 1, axis = 0, type = 1, token){
  if(type == 1){
    # line to line
    # use inegi_lines() to obtain id, source and target for the routes
    from <- inegi_lines(latitud=from_io[1],longitud=from_io[2], token = token)
    to <- inegi_lines(latitud=to_io[1],longitud=to_io[2], token = token)
    if(length(from)==5){}else{stop("Length of from parameter must be 2. A vector with source id and target id, in that order")}
    if(length(to)==5){}else{stop("Length of to parameter must be 2. A vector with source id and target id, in that order")}
    q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR",
                "&id_i=",from[1,1],
                "&source_i=", from[1,2], 
                "&target_i=",from[1,3],
                "&id_f=", to[1,1],
                "&source_f=", to[1,2],
                "&target_f=", to[1,3],
                "&p=", route_type,
                "&v=", vehicle,
                "&e=", axis,
                "&type=xml&key=", token)
    m <- "line to line"
  }else{
    if(type == 2){
      # destiny to destiny
      # use inegi_destiny() to obtain id, source and target for the routes
      from <- inegi_destiny(from_io,token)
      to <- inegi_destiny(to_io,token)
      q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR",
                  "&dest_i=",from[1,2], 
                  "&dest_f=", to[1,2], 
                  "&p=", route_type,
                  "&v=", vehicle,
                  "&e=", axis,
                  "&type=xml&key=", token)
      m <- "destiny to destiny"
    }else{
      if(type == 3){
        # line to destiny
        # use inegi_lines() + inegi_destiny() to obtain id, source and target for the routes
        from <- inegi_lines(latitud=from_io[1],longitud=from_io[2], token = token)
        to <- inegi_destiny(to_io,token)
        q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR",
                    "&id_i=",from[1,1],
                    "&source_i=", from[1,2], 
                    "&target_i=",from[1,3],
                    "&dest_f=", to[1,2], 
                    "&p=", route_type,
                    "&v=", vehicle,
                    "&e=", axis,
                    "&type=xml&key=", token)
        m <- "line to destiny"
      }else{
        if(type == 4){
          # destiny to line
          # use inegi_destiny + inegi_lines() to obtain id, source and target for the routes
          from <- inegi_destiny(from_io,token)
          to <- inegi_lines(latitud=to_io[1],longitud=to_io[2], token = token)
          q <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR",
                      "&dest_i=",from[1,2],
                      "&id_f=", to[1,1],
                      "&source_f=", to[1,2],
                      "&target_f=", to[1,3],
                      "&p=", route_type,
                      "&v=", vehicle,
                      "&e=", axis,
                      "&type=xml&key=", token)
          m <- "destiny to line"
        }else{stop("Type of query unrecognized. Use 1 through 4.")}
      }
    }
  }
  d <- XML::xmlToList(q)
  df <- data.frame("ROUTE_STRING" = paste0("Route, ", m, 
                                           " id: ", from[1,1], 
                                           " to id: ", to[1,1]),
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
