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
{ #configurar consulta
  url<-"http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/"
  coordenadas<-paste0(latitud,",",longitud)
  consulta<-paste0(url,keyword,"/",coordenadas,"/",metros,"/",token)
  #consulta de prueba: http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/todos/21.85717833,-102.28487238/250/f3fe034d-3273-4be5-a5b3-45b990eb0534
      
      # extraccion inicial
      s<-xmlToList(xmlParse(consulta,isHTML = TRUE))
      l<-strsplit(as.character(s$body),split = "\",\"|}")
      l<-as.list(l[[1]])
          largo<-length(l)-1
      l<-l[1:largo]
      
      # limpia
      l_limpia<-gsub(pattern = "\"",replacement = "",l)  
      l_limpia<-gsub(pattern = "{",replacement = "",l_limpia,perl=TRUE)
    
      # Revisar que sea divisible
        if(length(l_split)%%18==0) {} else {
          stop(print("Error en definición de datos: uno o más de los negocios traen más o menos de 18 campos"))}
      
      # dividir
      l_split<-split(x = l_limpia, f = 1:18)
      names(l_split)<-c("Id","Nombre","Razon",
                        "Actividad","Estrato","Vialidad",
                        "Calle","Exterior","Interior",
                        "Colonia","CP","Ubicacion",
                        "Tel","Mail","Sitio",
                        "Tipo","Longitud","Latitud")
      # Hacer en un data.frame 
      LimpiarRapido<-function(pat,elemento)
        {
          exit<-substr(elemento, regexpr(pattern = pat,text = elemento)+1,stop = 4000)
          return(exit)
        }
      df<-data.frame(
          id=LimpiarRapido(":",l_split$'1'),
          Nombre=LimpiarRapido(":",l_split$'2'),
          Razon=LimpiarRapido(":",l_split$'3'),
          Actividad=LimpiarRapido(":",l_split$'4'),
          Estrato=LimpiarRapido(":",l_split$'5'),
          Vialidad=LimpiarRapido(":",l_split$'6'),
          Calle=LimpiarRapido(":",l_split$'7'),
          NumExterior=LimpiarRapido(":",l_split$'8'),
          NumInterior=LimpiarRapido(":",l_split$'9'),
          Colonia=LimpiarRapido(":",l_split$'10'),
          CP=LimpiarRapido(":",l_split$'11'),
          Ubicacion=LimpiarRapido(":",l_split$'12'),
          Tel=LimpiarRapido(":",l_split$'13'),
          eMail=LimpiarRapido(":",l_split$'14'),
          SitioWeb=LimpiarRapido(":",l_split$'15'),
          Tipo=LimpiarRapido(":",l_split$'16'),
          Longitud=LimpiarRapido(":",l_split$'17'),
          Latitud=LimpiarRapido(":",l_split$'18'),
          stringsAsFactors = FALSE
        )
  return(df)
}