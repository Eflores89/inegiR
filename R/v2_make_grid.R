#' Makes a grid set of coordinates
#'
#' Returns a set of coordinates that intertwine to create an area larger than 5 kilometers. 
#' Suggestion by Arturo Cardenas \url{https://github.com/arturocm}.
#' 
#' @param lat1 First corner (latitud)
#' @param lat2 Second corner (latitud)
#' @param lon1 First corner (longitud)
#' @param lon2 Second corner (longitud)
#' @param espacio_lat Space between latitud coordinates defaults to 0.07 degrees
#' @param espacio_lon Space between longitud coordinates defaults to 0.07 degrees
#' @seealso denue_grid
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
#' # in spanish (legacy)
#' setofcoords <- hacer_grid(latitud1, latitud2, longitud1, longitud2)
#' # in english
#' setofcoords <- make_grid(latitud1, latitud2, longitud1, longitud2)
#' }
#' @name make_grid
NULL

#' @export
#' @rdname make_grid
make_grid <- function(lat1, lat2, lon1, lon2, espacio_lat = 0.07, espacio_lon = 0.07){
  # para asegurarnos de tener un cuadro
  lat1_n <- base::min(lat1, lat2)
  lat2_n <- base::max(lat1, lat2)
  lon1_n <- base::min(lon1, lon2)
  lon2_n <- base::max(lon1, lon2)
  
  grid_output <- NULL
  x <- base::abs(base::round((lat1_n-lat2_n)/espacio_lat, digits = 0))+1
  y <- base::abs(base::round((lon1_n-lon2_n)/espacio_lon, digits = 0))+1
  for (i in 1:x){
    for (j in 1:y){
      la <- lat1_n-((i*espacio_lat)-espacio_lat)
      lo <- lon1_n+((j*espacio_lon)-espacio_lon)
      prev <- cbind.data.frame(la, lo)
      grid_output <- rbind.data.frame(grid_output, prev)
    }
  }
  return(as.data.frame(grid_output))
}

#' @export
#' @rdname make_grid
hacer_grid <- function(lat1, lat2, lon1, lon2, espacio_lat = 0.07, espacio_lon = 0.07){
  # para asegurarnos de tener un cuadro
  lat1_n <- base::min(lat1, lat2)
  lat2_n <- base::max(lat1, lat2)
  lon1_n <- base::min(lon1, lon2)
  lon2_n <- base::max(lon1, lon2)
  
  grid_output <- NULL
  x <- base::abs(base::round((lat1_n-lat2_n)/espacio_lat, digits = 0))+1
  y <- base::abs(base::round((lon1_n-lon2_n)/espacio_lon, digits = 0))+1
  for (i in 1:x){
    for (j in 1:y){
      la <- lat1_n-((i*espacio_lat)-espacio_lat)
      lo <- lon1_n+((j*espacio_lon)-espacio_lon)
      prev <- cbind.data.frame(la, lo)
      grid_output <- rbind.data.frame(grid_output, prev)
    }
  }
  return(as.data.frame(grid_output))
}