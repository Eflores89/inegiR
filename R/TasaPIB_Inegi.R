#' Obtener tasa de crecimiento del PIB
#'
#' Obtiene tasa de crecimiento vs. mismo periodo de un año antes en porcentaje. Es un wrapper de las funciones Serie_Inegi() y YoY(). 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Vector numerico
#'
#' @note Ruta tematica BIE: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' CrecimientoMex<-TasaPIB_Inegi(token)
#' @export
#' 

TasaPIB_Inegi<-function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-Serie_Inegi(s,token)
  return(YoY(serie = i$Valores,lapso = 12,decimal = FALSE))
}