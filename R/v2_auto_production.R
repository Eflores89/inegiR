#' Auto production
#'
#' Returns auto production in Mexico and year-over-year change.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' Autos <- auto_production(token)
#' }
#' @name auto_production
NULL

#' @export
#' @rdname auto_production
auto_production <- function(token)
{ #Retornar la prod automotriz
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/15166/00000/en/false/xml/"
  
  i <- inegiR::inegi_series(s, token)
  t <- inegiR::YoY(serie=i$Values, lapso=12, decimal=FALSE)
  d <- cbind.data.frame(Dates = i$Dates,
                        "Autos" = i$Values, 
                        "YoY" = t)
  return(d)
}

#' @export
#' @rdname auto_production
series_produccion_autos<-function(token)
{ #Retornar la prod automotriz
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/15166/00000/en/false/xml/"
  
  i <- inegiR::serie_inegi(s, token)
  t <- inegiR::YoY(serie=i$Valores, lapso=12, decimal=FALSE)
  d <- cbind.data.frame(Fechas=i$Fechas,"Autos"=i$Valores,"YoY"=t)
  warning("This function is not being maintained. Use auto_production() instead.")
  return(d)
}