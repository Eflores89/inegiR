#' Commerce growth rate
#'
#' Returns commerce growth rate (terciary activity as defined officially by INEGI) vs. same month year earlier. 
#' Wrapper for \code{serie_inegi()} and \code{YoY()}. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Commerce <- rate_commerce(token)
#' }
#' @name commerce
NULL

#' @export
#' @rdname commerce
rate_commerce <- function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-inegiR::serie_inegi(serie = s, token)
  d<-cbind.data.frame(Dates = d$Fechas, Values = d$Valores,"YoY" = YoY(serie = d$Valores,
                                                                       lapso = 12, 
                                                                       decimal = FALSE))
  return(d)
}

#' @export
#' @rdname commerce
tasa_comercio<-function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-inegiR::serie_inegi(s,token)
  d<-cbind.data.frame(Fechas=d$Fechas,Valores=d$Valores,"YoY"=YoY(serie = d$Valores,
                                                                  lapso = 12, 
                                                                  decimal = FALSE))
  warning("This function is not being maintained. Use rate_commerce() instead.")
  return(d)
}
