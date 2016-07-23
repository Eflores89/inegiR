#' GDP growth rate
#'
#' Returns GDP growth rate vs. same period a year earlier. Wrapper of \code{serie_inegi()} and \code{YoY()}.
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @note BIE route: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' gdp <- rate_GDP(token)
#' }
#' @export
rate_GDP <- function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-inegiR::serie_inegi(serie = s, token)
  t<-inegiR::YoY(serie = i$Valores, lapso = 4, decimal = FALSE)
  d<-cbind.data.frame(Values=t, Dates = i$Fechas)
  return(d)
}

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
#' @export
rate_unemployment <- function(token)
{ #Retornar el desempleo
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d<-inegiR::inegi_series(series = s, token)
  
  return(d)
}

#' Commerce growth rate
#'
#' Returns commerce growth rate (terciary activity) vs. same month year earlier. 
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
#' @export
rate_commerce <- function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-inegiR::serie_inegi(serie = s, token)
    d<-cbind.data.frame(Dates = d$Fechas, Values = d$Valores,"YoY" = YoY(serie = d$Valores,
                                                                    lapso = 12, 
                                                                    decimal = FALSE))
return(d)
}

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
#' @export
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
#' @export
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
#' @export
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
