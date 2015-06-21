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

}