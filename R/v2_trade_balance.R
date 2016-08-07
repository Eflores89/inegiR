#' Trade balance
#'
#' Returns exports, imports and trade balance (all products, services and countries) in a data.frame.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' external_com <- trade_balance(token)
#' }
#' @name trade_balance
NULL

#' @export
#' @rdname trade_balance
trade_balance<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml/"
  
  x_val<-inegiR::inegi_series(x,token)
  names(x_val)<-c("Exports","Dates")
  m_val<-inegiR::inegi_series(m,token)
  names(m_val)<-c("Imports","Dates")
  
  df <- Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  d <- cbind.data.frame(Dates = df$Dates,
                      Exports = df$Exports,
                      Imports = df$Imports,
                      Balance = df$Exports-df$Imports)
  return(d)
}

#' @export
#' @rdname trade_balance
series_balanza_comercial<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml/"
  
  x_val<-inegiR::serie_inegi(x,token)
  names(x_val)<-c("Exportaciones","Fechas")
  m_val<-inegiR::serie_inegi(m,token)
  names(m_val)<-c("Importaciones","Fechas")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  d<-cbind.data.frame(Fechas=df$Fechas,
                      Exportaciones=df$Exportaciones,
                      Importaciones=df$Importaciones,
                      Balance=df$Exportaciones-df$Importaciones)
  
  return(d)
}