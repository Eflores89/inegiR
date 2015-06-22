#' Obtiene las tasas de crecimiento del PIB por Estado
#'
#' Obtiene la tasa de crecimiento vs. mismo periodo de un año antes en porcentaje de cada estado del país (en columna) Es un wrapper de las funciones Serie_Inegi() y YoY(). 
#' El orden de los estados es orden alfabetico. 
#' 
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame numerico con 33 columnas (1 por estado y fecha)
#'
#' @examples
#' PIBEstados <- TasaPIB_Estados_Inegi(token)
#' @export
#' 

TasaPIB_Estados_Inegi<-function (token){
  #Series de PIB;
  Ags<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383217/00000/es/false/xml/"
  BC<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383218/00000/es/false/xml/"
  BCS<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383219/00000/es/false/xml/"
  #Bajar
  Aguascalientes<-cbind.data.frame(Fechas=Serie_Inegi(Ags,token)$Fechas,
                     Valores= YoY(Serie_Inegi(Ags,token)$Valores,1,decimal=FALSE))
  BajaCalifornia<-cbind.data.frame(Fechas=Serie_Inegi(BC,token)$Fechas,
                                   Valores= YoY(Serie_Inegi(BC,token)$Valores,1,decimal=FALSE))
  BajaCaliforniaSur<-cbind.data.frame(Fechas=Serie_Inegi(BCS,token)$Fechas,
                                   Valores= YoY(Serie_Inegi(BCS,token)$Valores,1,decimal=FALSE))
  #merge
  d<-Reduce(function(...) merge(..., all=TRUE), list(Aguascalientes, BajaCalifornia))
  return(d)
}