#' IGAE growth rate
#'
#' Returnes IGAE (Indicador Global de Actividad Economica) growth rate year over year and month over month.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}. 
#' 
#' @details Month over month is a seasonally-adjusted series, while the original is used for year over year.
#' @param token token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' igae <- rate_IGAE(token)
#' }
#' @name IGAE
NULL

#' @export
#' @rdname IGAE
rate_IGAE <- function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383152/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/405698/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(serie = s1, token)
  i2<-inegiR::serie_inegi(serie = s2, token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal = FALSE)
  t1<-cbind.data.frame(Dates=i1$Fechas, "Original (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal = FALSE)
  t2<-cbind.data.frame(Dates=i1$Fechas, "Adjusted (YoY)"=t2)
  t3<-inegiR::YoY(serie = i2$Valores, lapso = 1, decimal = FALSE)
  t3<-cbind.data.frame(Dates=i1$Fechas, "Adjusted (MoM)"=t3)
  
  #union
  df<-Reduce(function(...) merge(..., all = TRUE),list(t1, t2, t3))
  return(df)
}

#' @export
#' @rdname IGAE
tasa_IGAE<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383152/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/405698/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1,token)
  i2<-inegiR::serie_inegi(s2,token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal = FALSE)
  t1<-cbind.data.frame(Fechas=i1$Fechas, "Serie Original (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal = FALSE)
  t2<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (YoY)"=t2)
  t3<-inegiR::YoY(serie = i2$Valores, lapso = 1, decimal = FALSE)
  t3<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (MoM)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=TRUE),list(t1,
                                                    t2,
                                                    t3))
  warning("This function is not being maintained. Use rate_IGAE() instead.")
  return(df)
}