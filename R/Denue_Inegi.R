#' Obtiene establecimientos del DENUE
#'
#' Regresa Data.Frame datos de establecimientos registrados en el DENUE en zona aledaña a las coordenadas.  
#' Es una de las funciones primitivas del paquete.
#'
#' @param latitud Vector en caracter de latitud (en decimal) de lugar
#' @param longitud Vector en caracter de longitud (en decimal) de lugar
#' @param token Token emitida por INEGI para acceder a API
#' @param metros Distancia en metros a la redonda para buscar establecimientos. Default = 250 
#' @param keyword Palabra clave de establecimiento para buscar. Por default busca todos. 
#' 
#' @return Dataframe
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' #Traer todos los establecimientos a 1 km de la macro plaza en Monterrey
#' token<-"tokenProporcionadoporWebservice"
#' latitud<-"-1321"
#' longitud<-"0000"
#' Negocios <- Denue_Inegi(latitud,longitud,token,metros = 1000)
#' @export

Denue_Inegi<-function(latitud,longitud,token, metros = 250, keyword = "todos")
{ #hacer consulta
  url<-"http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/"
  coordenadas<-paste0(latitud,",",longitud)
  consulta<-paste0(url,keyword,"/",coordenadas,"/",token)
  #consulta<-http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/todos/21.85717833,-102.28487238/250/f3fe034d-3273-4be5-a5b3-45b990eb0534
  
  datos<-xmlParse(consulta,isHTML = TRUE)
      s<-xmlToList(datos)
      l<-strsplit(as.character(s$body),split = "\\{")
      
    #un poco como what?
  #beta no run
      l<-strsplit(as.character(l),split = "\\\\")
      l<-as.list(unlist(l))
  #vector_ids<-
  
}