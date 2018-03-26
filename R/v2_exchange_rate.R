#' Exchange rate
#'
#' Returns exchange rate (interbank, sale) for pesos to U.S. dollars. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' USD <- series_tipocambio(token)
#' USD <- exchange_rate(token)
#' }
#' @name exchange_rate
NULL

#' @export
#' @rdname exchange_rate
exchange_rate <- function(token)
{ 
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/824/00000/en/false/xml/"
  
  d <- inegiR::inegi_series(s, token)
  return(d)
}

#' @export
#' @rdname exchange_rate

series_tipocambio<-function(token)
{ 
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/824/00000/en/false/xml/"
  
  d <- inegiR::serie_inegi(s, token)
  warning("This function is not being maintained. Use exchange_rate() instead.")
  return(d)
}