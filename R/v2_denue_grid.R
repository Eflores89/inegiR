#' Businesses in a grid larger than 5 kms
#'
#' Returns data.frame with businesses registered in the DENUE in spaces larger than 5 kilometers.
#' Calls \code{make_grid}. Functions contributed by Arturo Cardenas \url{https://github.com/arturocm}. 
#' 
#' @details 
#' Makes a loop for each pair of coordinates, creating a grid to extract businesses inside. Uses maximum and minimum coordinate pairs to draw frame.
#' @param lat1 First corner (latitud)
#' @param lat2 Second corner (latitud)
#' @param lon1 First corner (longitud)
#' @param lon2 Second corner (longitud)
#' @param token API token supplied by INEGI
#' @param metros Distance in meters to search by coordinate
#' @param keyword Keyword of businesses to include. Defaults to all ("todos")
#' @param espacio_lat Space between latitud coordinates defaults to 0.07 degrees
#' @param espacio_lon Space between longitud coordinates defaults to 0.07 degrees
#' @param unicos Default = TRUE, eliminates duplicate businesses
#' 
#' @note Legacy function, will return data in spanish.
#' 
#' @return Data.frame
#'
#' @author Arturo Cardenas
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' latitud1 <- "25.669194"
#' latitud2 <- "25.169194"
#' longitud1 <- "-100.30990"
#' longitud2 <- "-101.20102"
#' Negocios <- denue_grid(latitud1, latitud2, longitud1, longitud2, token)
#' }
#'
#' @export
denue_grid <- function(lat1, lat2, lon1, lon2, token, metros = 5000, keyword = "todos", espacio_lat = 0.07, espacio_lon = 0.07, unicos = TRUE){
  #antes de correr, revisar datos de columnas
  if(class(lat1)=="numeric"){} else {stop(print("Latitud 1 no es numerica"))}
  if(class(lat2)=="numeric"){} else {stop(print("Latitud 2 no es numerica"))}
  if(class(lon1)=="numeric"){} else {stop(print("Longitud 1 no es numerica"))}
  if(class(lon2)=="numeric"){} else {stop(print("Longitud 2 no es numerica"))}
 
  grid <- inegiR::hacer_grid(lat1 = lat1, 
                             lat2 = lat2, 
                             lon1 = lon1, 
                             lon2 = lon2, 
                             espacio_lat = espacio_lat, espacio_lon = espacio_lon)
  output <- data.frame()
  for (i in 1:nrow(grid)){
    mapa <- tryCatch(inegiR::denue_inegi(latitud = grid[i,1], 
                                         longitud = grid[i,2],
                                 token = token,
                                 metros = metros,
                                 keyword = keyword),
                     # skip si no hay algo en DENUE
                     error = function(e){})
    output <- rbind.data.frame(output, mapa)
  }
  if(unicos){
    df <- unique(output)
  }else{
    df <- output
  }
 return(df)
}
