#' Urban unemployment rate
#'
#' Returns a unified data series (32 city aggregate) of urban unemployment rate.
#' Wrapper of \code{serie_inegi()} and \code{YoY()}. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Desempleo <- rate_unemployment(token)
#' }
#' @name unemployment
NULL

#' @export
#' @rdname unemployment
rate_unemployment <- function(token)
{ #Retornar el desempleo
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d <- inegiR::inegi_series(series = s, token)
  
  return(d)
}
#' @export
#' @rdname unemployment
tasa_desempleo<-function(token)
{ #Retornar el desempleo
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d <- inegiR::serie_inegi(s,token)
  
  return(d)
}