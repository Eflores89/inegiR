#' Sectorial growth rates
#'
#' Returns growth rate by economic sector as defined in INEGI (subsectors of IGAE). 
#' None of the series are seasonally adjusted and percent change is year over year.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}. 
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' sectors <- rate_econsectors(token)
#' }
#' @name econ_sectors
NULL

#' @export
#' @rdname econ_sectors
rate_econsectors <- function(token)
{#traer sectores
  #primarios
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383153/00000/en/false/xml/"
  #secundarios
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383154/00000/en/false/xml/"
  #terciarios
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383159/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(serie = s1, token)
  i2<-inegiR::serie_inegi(serie = s2, token)
  i3<-inegiR::serie_inegi(serie = s3, token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
  t1<-cbind.data.frame(Dates=i1$Fechas, "Primary (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
  t2<-cbind.data.frame(Dates=i1$Fechas, "Secundary (YoY)"=t2)
  t3<-inegiR::YoY(serie = i3$Valores, lapso = 12, decimal=FALSE)
  t3<-cbind.data.frame(Dates=i1$Fechas, "Terciary (YoY)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=TRUE),list(t1, t2, t3))
  return(df)
}

#' @export
#' @rdname econ_sectors
tasa_sectoresYoY<-function(token)
{#traer sectores
  #primarios
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383153/00000/en/false/xml/"
  #secundarios
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383154/00000/en/false/xml/"
  #terciarios
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383159/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1,token)
  i2<-inegiR::serie_inegi(s2,token)
  i3<-inegiR::serie_inegi(s3,token)
  
  t1<-inegiR::YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
  t1<-cbind.data.frame(Fechas=i1$Fechas, "Primarios (YoY)"=t1)
  t2<-inegiR::YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
  t2<-cbind.data.frame(Fechas=i1$Fechas, "Secundarios (YoY)"=t2)
  t3<-inegiR::YoY(serie = i3$Valores, lapso = 12, decimal=FALSE)
  t3<-cbind.data.frame(Fechas=i1$Fechas, "Terciarios (YoY)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=TRUE),list(t1,
                                                    t2,
                                                    t3))
  warning("This function is not being maintained. Use rate_econsectors() instead.")
  return(df)
}