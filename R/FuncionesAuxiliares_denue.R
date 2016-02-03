#' Obtiene estadisticas de coordenada
#' 
#' Regresa Data.Frame con estadísticas básicas sobre los establecimientos encontrados a la rendonda de coordenada, utilizando denue_inegi().
#' Trae una función de loop integrada, para que pueda regresar indicadores de muchas coordenadas, utilizando un data.frame.
#'
#' @param data Data.frame dónde se encuentran las dos columnas de coordenadas
#' @param col_lat número de columna de "data" en dónde se encuenta la latitud
#' @param col_long número de columna de "data" en dónde se encuenta la longitud
#' @param token Token emitida por INEGI para acceder a API
#' @param metros Distancia en metros a la redonda para buscar establecimientos. Default = 250
#' @param keyword Palabra clave de establecimiento para buscar. Por default busca todos.
#'
#' @return Data.frame
#'
#' @author Eduardo Flores
#'
#' @examples
#' #indicadores de 2 lugares
#' \dontrun{
#' token<-"webservice_token"
#' df<-as.data.frame(latitud  = c(25.669194, 25.121194),
#'                   longitud = c(-100.30990, -99.81923))
#' stats<-denue_varios_stats(data     = df,
#'                           col_lat  = 1,
#'                           col_long = 2,
#'                           metros   = 500)
#'}
#' @export

denue_varios_stats<-function(data, col_lat, col_long, token, metros = 250, keyword = "todos")
{ #cuantas coordenadas a revisar
  n <- length(data[,col_lat])
 
  #antes de correr, revisar datos de columnas
  if(class(data[,col_lat][1])=="numeric"){} else {stop(print("Columna de Latitud no es numerica"))}
  if(class(data[,col_long][1])=="numeric"){} else {stop(print("Columna de Longitud no es numerica"))}
 
  # poblar data frame inicial
  d<-data.frame(
    NEGOCIOS = 1:n,
    CALLES   = 1:n,
    ACTIVIDADES    = 1:n,
    NEGOCIOS_FIJOS = 1:n,
    NEGOCIOS_RAZON = 1:n,
    NEGOCIOS_TEL   = 1:n,
    NEGOCIOS_WEBSITE = 1:n,
    NEGOCIOS_SOBRE_AVENIDA = 1:n,
    EMPLEADOS_EST = 1:n,
    EMPLEADOS_SD  = 1:n,
    ACTIVIDAD_PRINCIPAL = 1:n
  )
 
  #loop
  for (i in 1:n)
  { tryCatch({
   
    #crear matriz por negocio
    m <- inegiR::denue_inegi(latitud  = data[,col_lat][i],
                   longitud = data[,col_long][i],
                   token    = token,
                   metros   = metros,
                   keyword  = keyword
    )
   
    #agregar numero empleados
    for (j in 1:length(m[,1]))
                        {
      m$Empleados[j]<-if(grepl(x = m["Estrato"][j,],pattern = "^11")){as.numeric(20)} else {
                      if(grepl(x = m["Estrato"][j,],pattern = "^0")){as.numeric(3)} else {
                      if(grepl(x = m["Estrato"][j,],pattern = "^6")){as.numeric(8)} else {
                      if(grepl(x = m["Estrato"][j,],pattern = "^31")){as.numeric(40)} else {
                      if(grepl(x = m["Estrato"][j,],pattern = "^101")){as.numeric(220)} else {
                      if(grepl(x = m["Estrato"][j,],pattern = "^51")){as.numeric(75)}  else {
                        #mas de 250
                        as.numeric(300)
                      } } } } } } }
   
    #cantidad de negocios
    d[i,]$NEGOCIOS<-if(is.null(length(m[,1]))){0} else {length(m[,1])}
    #cantidad de calles diferentes
    d[i,]$CALLES<-if(is.null(length(unique(m[,"Calle"])))){0} else {length(unique(m[,"Calle"]))}
    #cantidad de actividades diferentes
    d[i,]$ACTIVIDADES<-if(is.null(length(unique(m[,"Actividad"])))){0} else {length(unique(m[,"Actividad"]))}
    #negocios con telefono
    d[i,]$NEGOCIOS_TEL<-if(is.null(length(m[!grepl(pattern = "^$",m["Tel"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["Tel"][,1]),][,1])}
    #negocios con sitios de internet - no WWW, 
    d[i,]$NEGOCIOS_WEBSITE<-if(is.null(length(m[!grepl(pattern = "^$",m["SitioWeb"][,1]),][,1]))) {0} else {length(m[!grepl(pattern = "^$",m["SitioWeb"][,1]),][,1])}
    #negocios con razon social (la que sea)
    d[i,]$NEGOCIOS_RAZON<-if(is.null(length(m[!grepl(pattern = "^$",m["Razon"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["Razon"][,1]),][,1])}
    #negocios ubicados en avenida
    d[i,]$NEGOCIOS_SOBRE_AVENIDA<-if(is.null(length(subset(m,m$Vialidad == "AVENIDA")[,1]))){0} else {length(subset(m,m$Vialidad == "AVENIDA")[,1])}
    #negocios fijos
    d[i,]$NEGOCIOS_FIJOS<-if(is.null(length(subset(m,m$Tipo == "Fijo")[,1]))){0} else {length(subset(m,m$Tipo == "Fijo")[,1])}
    #estimacion de empleados en base a columna de estrato
    d[i,]$EMPLEADOS_EST<-if(is.null(sum(as.numeric((m[,"Empleados"]))))){0} else {sum(as.numeric((m[,"Empleados"])))}
    #desviacion standar de empleados en comercios
    d[i,]$EMPLEADOS_SD<-if(is.null(stats::sd(x = as.numeric((m[,"Empleados"]))))) {0} else {stats::sd(x = as.numeric((m[,"Empleados"])))}
    #actividad principal mas importante - usa otra funcion de este paquete.
    d[i,]$ACTIVIDAD_PRINCIPAL<-if(is.null(inegiR::ordenar_porconteo(m,Actividad)[1,1])){"Ninguna"} else {inegiR::ordenar_porconteo(m,Actividad)[1,1]}
    
    #termina instancia de tryCatch
  }, error = function(e){
    #continuar en caso de error en una de las coordenadas
    }
  #fin trycatch
  )
  }
  
  #calculos sobres data frame
  d$NEGOCIOSXCALLE <- d$NEGOCIOS/d$CALLES
  d$NEGOCIOSXMETRO <- d$NEGOCIOS/metros

  d$EMPLEADOSXNEGOCIO <- d$EMPLEADOS_EST/d$NEGOCIOS
  d$EMPLEADOSXMETRO   <- d$EMPLEADOS_EST/metros
 
  #OJO - revisar las divisiones por errores.
  d$PORCENTAJE_NEGOCIOS_FIJOS   <- d$NEGOCIOS_FIJOS/d$NEGOCIOS
  d$PORCENTAJE_NEGOCIOS_AVENIDA <- d$NEGOCIOS_SOBRE_AVENIDA/d$NEGOCIOS
  d$PORCENTAJE_NEGOCIOS_WEBSITE <- d$NEGOCIOS_WEBSITE/d$NEGOCIOS
  
  #exportar
  return(d)
}

#' Obtiene establecimientos del DENUE en una area mayor a 5kms
#'
#' Regresa data.frame de datos de establecimientos registrados en el DENUE en un grid con dos o mas areas de 5kms (el limite de la llamada a INEGI).  
#' Llama a \code{hacer_grid}, ambas posible gracias a Arturo Cardenas \url{https://github.com/arturocm}. 
#' @details 
#' Se hace un loop por cada par de coordenadas, que se sobrelapan en circulos alrededor de un cuadro proporcionado por las cuatro esquinas de los parametros. 
#' La función hacer_grid, usa máximos y mínimos de latitud y longitud para asignar pares, por lo que debes considerar con cuidad el área a mapear. 
#' @param lat1 Esquina 1 de cuadro o área en latitud. 
#' @param lat2 Esquina 2 de cuadro o área en latitud.
#' @param lon1 Esquina 1 de cuadro o área en longitud.
#' @param lon2 Esquina 2 de cuadro o área en longitud.
#' @param token Token emitida por INEGI para acceder a API
#' @param metros Distancia en metros a la redonda para buscar establecimientos. Default = 5000, que es el máximo permitido por INEGI. Considera que si no cambias el espacio entre mediciones (en los otros parámetros), no vas a dibujar un cuadro totalmente cubierto por circulos.
#' @param keyword Palabra clave de establecimiento para buscar, a pasar a denue_inegi. Por default busca todos. 
#' @param espacio_lat Espacio entre coordenadas, en latitud, por default = 0.07 grados.
#' @param espacio_lon Espacio entre coordenadas, en longitud, por default = 0.07 grados.
#' @param unicos Default = TRUE, solamente se exportan los negocios únicos (para evitar duplicar si sobrelapan los radios de las coordenadas).
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

#' Proporciona un set de coordenadas 
#'
#' Regresa un set de coordenadas que juntas se sobrelapan para crear un cuadro mayor a 5 kilómetros. Posible gracias a Arturo Cardenas \url{https://github.com/arturocm}.
#' @param lat1 Esquina 1 de cuadro o área en latitud. 
#' @param lat2 Esquina 2 de cuadro o área en latitud.
#' @param lon1 Esquina 1 de cuadro o área en longitud.
#' @param lon2 Esquina 2 de cuadro o área en longitud.
#' @param espacio_lat Espacio entre coordenadas, en latitud, por default = 0.07 grados.
#' @param espacio_lon Espacio entre coordenadas, en longitud, por default = 0.07 grados.
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
#' varias_coordenadas <- hacer_grid(latitud1, latitud2, longitud1, longitud2)
#' }
#'
#' @export
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
