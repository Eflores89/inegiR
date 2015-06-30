#' Obtener tasa de crecimiento del PIB
#'
#' Obtiene tasa de crecimiento vs. mismo periodo de un año antes en porcentaje. 
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @note Ruta tematica BIE: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' CrecimientoMex<-Tasa_PIB(token)
#' @export
#' 

Tasa_PIB<-function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-Serie_Inegi(s,token)
  t<-YoY(serie = i$Valores,lapso = 4,decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,Valores=t)
  return(d)
}

#' Obtener Desempleo Urbano
#'
#' Obtiene tasa de desocupación (serie unificada) urbana (agregado de 32 ciudades)
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' Desempleo<-Tasa_Desempleo(token)
#' @note Encoding no permite acentos en titulo de descripción
#' @export
#'

Tasa_Desempleo<-function(token)
{ #Retornar el desempleo
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d<-Serie_Inegi(s,token)
  
  return(d)
}