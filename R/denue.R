#' Returns DENUE businesses
#'
#' Returns data.frame with businesses registered in DENUE in the vicinity of supplied coordinates. 
#'
#' @param latitud Character vector with latitud (in decimals)
#' @param longitud Character vector with longitud (in decimals)
#' @param token API token supplied by INEGI
#' @param meters Meters to search in a circle from coordinates. Defaults to 250 
#' @param keyword Keyword to search in business description (in spanish). Defaults to all (todos). 
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' # All businesses in a 1 km radius from the Macroplaza in Monterrey, Mex.
#' \dontrun{
#' token<-"webservice_token"
#' latitud<-"25.669194"
#' longitud<-"-100.30990"
#' Negocios <- inegi_denue(latitud, longitud, token, meters = 1000)
#' }
#'
#' @importFrom XML xmlToList
#' @importFrom XML xmlParse
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @export
inegi_denue<-function(latitud, longitud, token, meters = 250, keyword = "todos")
{ 

#Revisar que coordenadas esten en Mexico---
.EstaEnMexico<-function(latitud,longitud){
  if(as.numeric(latitud)<14.559507 | as.numeric(latitud)> 32.757120 | as.numeric(longitud)> -86.708301 | as.numeric(longitud)< -118.312155)
  {FALSE} else {TRUE}}
  
  if(.EstaEnMexico(latitud, longitud)){}else{stop("Coordinates are not in Mexico")}

#configurar consulta
  url<-"http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/"
  coordenadas<-paste0(latitud, ",", longitud)
  consulta<-paste0(url, keyword, "/", coordenadas, "/", meters, "/", token)
  
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
          stop(print("Error in data definition. Number of columns for one or more businesses is != 18"))}
      
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
          Name=LimpiarRapido(":",l_split$'2'),
          LegalName=LimpiarRapido(":",l_split$'3'),
          Activity=LimpiarRapido(":",l_split$'4'),
          EconomicClass=LimpiarRapido(":",l_split$'5'),
          Roads=LimpiarRapido(":",l_split$'6'),
          Street=LimpiarRapido(":",l_split$'7'),
          ExteriorNum=LimpiarRapido(":",l_split$'8'),
          InteriorNum=LimpiarRapido(":",l_split$'9'),
          Colonia=LimpiarRapido(":",l_split$'10'),
          PostalCode=LimpiarRapido(":",l_split$'11'),
          Location=LimpiarRapido(":",l_split$'12'),
          Tel=LimpiarRapido(":",l_split$'13'),
          eMail=LimpiarRapido(":",l_split$'14'),
          Website=LimpiarRapido(":",l_split$'15'),
          Type=LimpiarRapido(":",l_split$'16'),
          Longitud=LimpiarRapido(":",l_split$'17'),
          Latitud=LimpiarRapido(":",l_split$'18'),
          stringsAsFactors = FALSE
        )
  return(df)
}