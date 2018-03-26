#' Get rate of inflation
#'
#' Returns anual inflation rate (national, overall rate). Technically, it is the percent anual change of the INPC index.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' inflation <- overall_inflation(token)
#' }
#' @name inflation
NULL

#' @export
#' @rdname inflation
overall_inflation <- function (token){
  #Serie de INPC general
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
  i<-inegiR::inegi_series(s, token)
  t<-inegiR::YoY(serie = i$Values, lapso = 12, decimal = FALSE)
  d<-cbind.data.frame(Dates=i$Dates, Values=t)
  return(d)
}

#' @export
#' @rdname inflation
inflacion_general<-function (token){
  #Serie de INPC general
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
  i<-inegiR::serie_inegi(s, token)
  t<-inegiR::YoY(serie = i$Valores, lapso = 12, decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas, Valores=t)
  
  warning("This function is not being maintained. Use overall_inflation() instead.")
  return(d)
}