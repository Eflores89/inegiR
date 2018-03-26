#' Consumer confidence
#'
#' Returns the rate of change in consumer confidence. 
#' Month over month is a seassonally adjusted rate, year over year is the original series.
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' conf <- rate_cconfidence(token)
#' }
#' @name confidence
NULL

#' @export
#' @rdname confidence
rate_cconfidence <- function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/63017/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/132944/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(serie = s1, token)
  i2<-inegiR::serie_inegi(serie = s2, token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
  t1<-cbind.data.frame(Dates = i1$Fechas, "Original (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
  t2<-cbind.data.frame(Dates = i1$Fechas, "Adjusted (YoY)"=t2)
  t3<-inegiR::YoY(serie = i2$Valores, lapso = 1, decimal=FALSE)
  t3<-cbind.data.frame(Dates = i1$Fechas, "Adjusted (MoM)"=t3)
  
  #union
  df <- Reduce(function(...) merge(...,all = TRUE),list(t1, t2, t3))
  return(df)
}

#' @export
#' @rdname confidence
tasa_confianza<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/63017/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/132944/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1, token)
  i2<-inegiR::serie_inegi(s2, token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
  t1<-cbind.data.frame(Fechas=i1$Fechas, "Serie Original (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
  t2<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (YoY)"=t2)
  t3<-inegiR::YoY(serie = i2$Valores, lapso = 1, decimal=FALSE)
  t3<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (MoM)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all = TRUE),list(t1,
                                                      t2,
                                                      t3))
  warning("This function is not being maintained. Use rate_cconfidence() instead.")
  return(df)
}