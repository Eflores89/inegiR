#' Terms of trade
#'
#' Returns the terms of trade for Mexico, defined as the price index of exports over the price index of imports.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame 
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' tot <- trade_terms(token)
#' } 
#' @name terms_trade
NULL

#' @export
#' @rdname terms_trade
trade_terms <- function(token)
{ #calcular terminos de intercambio (Terms-Of-Trade)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37502/00000/es/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37503/00000/es/false/xml/"
  
  x_val<-inegiR::inegi_series(x,token)
  names(x_val)<-c("x","Dates")
  m_val<-inegiR::inegi_series(m,token)
  names(m_val)<-c("m","Dates")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$ToT<-df$x/df$m
  
  d<-cbind.data.frame(Dates=df$Dates, Values=df$ToT)
  return(d)
}

#' @export
#' @rdname terms_trade
inflacion_tot<-function(token)
{ #calcular terminos de intercambio (Terms-Of-Trade)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37502/00000/es/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37503/00000/es/false/xml/"
  
  x_val<-inegiR::serie_inegi(x,token)
  names(x_val)<-c("x","Fechas")
  m_val<-inegiR::serie_inegi(m,token)
  names(m_val)<-c("m","Fechas")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$ToT<-df$x/df$m
  
  d<-cbind.data.frame(Fechas=df$Fechas,Valores=df$ToT)
  return(d)
}