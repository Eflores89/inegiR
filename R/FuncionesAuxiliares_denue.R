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
#' token<-"tokenProporcionadoporWebservice"
#' df<-as.data.frame(latitud=c(25.669194,25.121194),
#'                   longitud=c(-100.30990,-99.81923))
#' stats<-denue_varios_stats(data = df,
#'                           col_lat = 1,
#'                           col_long = 2,
#'                           metros = 500)
#' @export

denue_varios_stats<-function(data,col_lat, col_long, token, metros = 250, keyword = "todos")
{ #cuantas coordenadas a revisar
  n<-length(data[,col_lat])
  
  #antes de correr, revisar datos de columnas
  if(class(data[,col_lat][1])=="numeric"){} else {stop(print("Columna de Latitud no es númerica"))}
  if(class(data[,col_long][1])=="numeric"){} else {stop(print("Columna de Longitud no es númerica"))}
  
  # poblar data frame inicial
  d<-data.frame(
    NEGOCIOS = 1:n,
    CALLES = 1:n,
    ACTIVIDADES = 1:n,
    SOBRE_AVENIDA = 1:n,
    NEGOCIOS_FIJOS = 1:n,
    NEGOCIOS_WEBSITE = 1:n
  )
  
  #loop
  for (i in 1:n)
  { #crear matriz por negocio
    m<-denue_inegi(latitud  = data[,col_lat][i],
                   longitud = data[,col_long][i],
                   token = token,
                   metros = metros,
                   keyword = keyword
    )
    #cantidad de negocios
    d[i,]$NEGOCIOS<-length(m[,1])
    #cantidad de calles diferentes
    d[i,]$CALLES<-length(unique(m[,"Calle"]))
    #cantidad de actividades diferentes
    d[i,]$ACTIVIDADES<-length(unique(m[,"Actividad"]))
    #negocios con sitios de internet - no WWW, parse:""
    d[i,]$NEGOCIOS_WEBSITE<-length(m[!grepl(pat= "^$",m["SitioWeb"][,1]),][,1])
    #negocios ubicados en avenida
    d[i,]$SOBRE_AVENIDA<-if(is.null(length(subset(m,m$Vialidad == "AVENIDA")[,1]))){0} else {length(subset(m,m$Vialidad == "AVENIDA")[,1])}
    #negocios fijos
    d[i,]$NEGOCIOS_FIJOS<-if(is.null(length(subset(m,m$Tipo == "Fijo")[,1]))){0} else {length(subset(m,m$Tipo == "Fijo")[,1])}
  }
  
  #calculos sobres data frame
  d$NEGOCIOSXCALLE <- d$NEGOCIOS/d$CALLES
  #OJO - revisar las divisiones por errores.
  d$PORCENTAJE_NEGOCIOS_FIJOS <-d$NEGOCIOS_FIJOS/d$NEGOCIOS
  
  #exportar
  return(d)
}