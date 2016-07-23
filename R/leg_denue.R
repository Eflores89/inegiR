#' Obtiene establecimientos del DENUE
#'
#' Regresa data.frame de datos de establecimientos registrados en el DENUE en zona aledaña a las coordenadas.  
#' Es una de las funciones primitivas del paquete.
#'
#' @param latitud Vector en caracter de latitud (en decimal) de lugar
#' @param longitud Vector en caracter de longitud (en decimal) de lugar
#' @param token Token emitida por INEGI para acceder a API
#' @param metros Distancia en metros a la redonda para buscar establecimientos. Default = 250 
#' @param keyword Palabra clave de establecimiento para buscar. Por default busca todos. 
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' #Traer todos los establecimientos a 1 km de la macro plaza en Monterrey
#' \dontrun{
#' token<-"webservice_token"
#' latitud<-"25.669194"
#' longitud<-"-100.30990"
#' Negocios <- denue_inegi(latitud, longitud, token, metros = 1000)
#' }
#'
#' @importFrom XML xmlToList
#' @importFrom XML xmlParse
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @export

denue_inegi<-function(latitud, longitud, token, metros = 250, keyword = "todos")
{ 

#Revisar que coordenadas esten en Mexico---
.EstaEnMexico<-function(latitud,longitud){
  if(as.numeric(latitud)<14.559507 | as.numeric(latitud)> 32.757120 | as.numeric(longitud)> -86.708301 | as.numeric(longitud)< -118.312155)
  {FALSE} else {TRUE}}
  
  if(.EstaEnMexico(latitud, longitud)){}else{stop("Coordenadas no estan en Mexico")}

#configurar consulta
  url<-"http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/"
  coordenadas<-paste0(latitud, ",", longitud)
  consulta<-paste0(url, keyword, "/", coordenadas, "/", metros, "/", token)
  
      # extraccion inicial
         #codigo de xmlparse usa print() como "warnings". Esto lo suprime,
          invisible(
            utils::capture.output(
                s<-xmlToList(xmlParse(consulta, isHTML = TRUE, encoding = "UTF-8"))
                  ))
      l<-strsplit(x = as.character(s$body), split = "\",\"|}")
      l<-as.list(l[[1]])
          largo<-length(l)-1
      l<-l[1:largo]
      
      # limpia
      l_limpia<-gsub(pattern = "\"", replacement = "",l)  
      l_limpia<-gsub(pattern = "{", replacement = "", l_limpia, perl=TRUE)
    
      # Revisar que sea divisible
        if(length(l_limpia)%%18==0) {} else {
          stop(print("Error en definicion de datos: uno o mas de los negocios traen mas o menos de 18 campos"))}
      
      # dividir
      l_split <- split(x = l_limpia, f = 1:18)
  
      # Hacer en un data.frame 
      LimpiarRapido <- function(pat, elemento)
        {
          exit<-substr(elemento, regexpr(pattern = pat, text = elemento)+1, stop = 4000)
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