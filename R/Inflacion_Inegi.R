#' Obtener tasa de inflación
#'
#' Obtiene tasa de inflación inter anual en porcentaje. Es un wrapper de las funciones Serie_Inegi() y YoY(). 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Vector numerico
#'
#' @examples
#' Inflacion<-Inflacion_Inegi(token)
#' @export
#' 

Inflacion_Inegi<-function (token){
  #Serie de INPC general;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
  i<-Serie_Inegi(s,token)
  t<-YoY(serie = i$Valores,lapso = 12,decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,Valores=t)
  return(d)
}