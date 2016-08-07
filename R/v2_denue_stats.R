#' Returns statistics of coordinate 
#' 
#' Returns basic statistics of businesses, using DENUE, in the vecinity of coordinate.
#'
#' @param data data.frame with columns for latitud and longituds
#' @param col_lat number of column in data with latitud column
#' @param col_long number of column in data with longitud column
#' @param token API token supplied by INEGI
#' @param metros Distance in meters to search by coordinate
#' @param keyword Keyword of businesses to include. Defaults to all ("todos")
#'
#' @note Legacy function, will return data in spanish.
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' df <- as.data.frame(latitud  = c(25.669194, 25.121194),
#'                   longitud = c(-100.30990, -99.81923))
#' stats <- denue_varios_stats(data     = df,
#'                           col_lat  = 1,
#'                           col_long = 2,
#'                           metros   = 500)
#'}
#' @export
denue_varios_stats <- function(data, col_lat, col_long, token, metros = 250, keyword = "todos")
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
