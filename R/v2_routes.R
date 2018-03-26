#' Returns INEGI destiny id's with matching searches
#'
#' Returns data.frame with id's and coordinates that match with the API names. 
#'
#' @param search Character vector to search for
#' @param token Sakbe API token supplied by INEGI
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
#' Returns the route between two points in Mexico
#'
#' Uses SAKBE API to return a route between two destiny id's considering the given parameters. 
#' @references See the official API here: http://www.inegi.org.mx/desarrolladores/sakbe/apisakbe.aspx
#' @param from Destiny id from where the route begins
#' @param to Destiny id of end of route
#' @param token Sakbe API token supplied by INEGI
#' @param pref Preference for road: 1 = with tolls (cuota), 2 = without tolls (libre), 2 = suggested route
#' @param vehicle Vehicle choice: 0 = motorcycle, 1 = auto, 2 = two axis bus, 3 = three axis bus, 4 = four axis bus, 5 = two axis truck, 6 = three axis truck, 7 = four axis truck, 8 = five axis truck, 9 = six axis truck, 10 = seven axis truck, 11 = eight axis truck, 12 = nine axis truck.
#' @param calc_cost if TRUE will use the price of gasoline to calculate total cost of trip. Very experimental, defaults to FALSE. 
#' @param rawJSON if TRUE returns only the JSON data, not parsed
#' 
#' @note To calculate the cost, it is wiser to use the more conservative estimate. Thus, this function assumes a premium type of gasoline (the most expensive) at the lower end bound of fuel-efficiency (11 kms per liter)
#' 
#' @return list
#' @author Eduardo Flores
#' @examples
#' # Macroplaza in Monterrey to Mexico City airport.
#' \dontrun{
#' token <- "webservice_token"
#' route <- inegi_route(from = 6940, to = 57, token, pref = 2, vehicle = 1)
#' }
#' @importFrom jsonlite fromJSON
#' @export
inegi_route <- function(from, to, token, pref, vehicle, calc_cost = FALSE, rawJSON = FALSE){
  p <- a <- d <- l <- b <- NULL
  
  p <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CR&dest_i=",from,
              "&dest_f=",to,
              "&p=",pref,
              "&v=",vehicle, 
              "&type=xml&key=",token)
  
  s <- XML::xmlToList(p)
  
  # extracting the coordinates in a data.frame
  a <- jsonlite::fromJSON(txt = s$ruta$geojson, 
                          simplifyDataFrame = T)
  d <- plyr::ldply(a$coordinates, as.data.frame)
  
  # extracting data for route
  b <- data.frame(
    "KMS" = as.numeric(s$ruta$long_km[1]),
    "TIME_MINS" = as.numeric(s$ruta$tiempo_min[1]),
    "TIME_HRS" = (as.numeric(s$ruta$tiempo_min[1])/60),
    "HAS_TOLL" = ifelse(s$ruta$peaje[1]=="t", TRUE, FALSE),
    "TOLL_COST" = as.numeric(s$ruta$costo_caseta[1])
    )
  
  if(calc_cost){
    b$TOTAL_COST <- b$KMS/11*inegiR::get_gas(token = token, 
                                             onlyPremium = TRUE)
  }else{
    b$TOTAL_COST <- NA
  }
  
  # return a list
  list("ROUTE" = b, "COORDINATE_PATH" = d)
}
#' Gets gas cost from INEGI API
#' 
#' Helper function
#' 
#' @param token Sakbe API token supplied by INEGI
#' @param onlyPremium Only export premium price
#' @return data.frame
#' @export
get_gas <- function(token, onlyPremium = FALSE){
  
  s <- paste0("http://gaia.inegi.org.mx/sakbe/wservice?make=CM&type=xml&key=",token)
  d <- XML::xmlToDataFrame(s)
  
  if(onlyPremium){
    r <- as.numeric(as.character(d[d$tipo == "Premium", 
                                   "costo"])
                    )
  }else{
    r <- d
  }
  
  return(r)
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
