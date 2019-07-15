#' Makes a grid set of coordinates
#'
#' Returns a set of coordinates that intertwine to create an area larger than 5 kilometers. 
#' Suggestion by Arturo Cardenas \url{https://github.com/arturocm}.
#' 
#' @param lat1 First corner (latitud). Must be numeric.
#' @param lat2 Second corner (latitud). Must be numeric.
#' @param lon1 First corner (longitud). Must be numeric.
#' @param lon2 Second corner (longitud). Must be numeric.
#' @param space_lat Space between latitud coordinates defaults to 0.07 degrees
#' @param space_lon Space between longitud coordinates defaults to 0.07 degrees
#' @seealso denue_grid
#' @return Data.frame
#'
#' @author Arturo Cardenas
#' 
#' @examples
#' 
#' latitud1 <- 25.66919
#' latitud2 <- 25.169194
#' longitud1 <- -100.30990
#' longitud2 <- -101.20102
#' setofcoords <- make_grid(latitud1, latitud2, longitud1, longitud2)
#' @name make_grid
NULL

#' @export
#' @rdname make_grid
make_grid <- function(lat1, lat2, lon1, lon2, space_lat = 0.07, space_lon = 0.07){
  
  lat1_n <- base::min(lat1, lat2)
  lat2_n <- base::max(lat1, lat2)
  lon1_n <- base::min(lon1, lon2)
  lon2_n <- base::max(lon1, lon2)
  
  grid_output <- NULL
  x <- base::abs(base::round((lat1_n-lat2_n)/space_lat, digits = 0))+1
  y <- base::abs(base::round((lon1_n-lon2_n)/space_lon, digits = 0))+1
  for (i in 1:x){
    for (j in 1:y){
      la <- lat1_n-((i*space_lat)-space_lat)
      lo <- lon1_n+((j*space_lon)-space_lon)
      prev <- cbind.data.frame(la, lo)
      grid_output <- rbind.data.frame(grid_output, prev)
    }
  }
  return(as.data.frame(grid_output))
}