#' Exports by country
#'
#' Returns exports to main trading partners of all products. Regions are the following: United States, Canada, China, CentralAmerica, SouthAmerica
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' xbycountry <- exports_country(token)
#' }
#' @name country_exports
NULL

#' @export
#' @rdname country_exports
exports_country <- function(token)
{ #exports por pais
  usa <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133172/00000/en/false/xml/"
  can <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133171/00000/en/false/xml/"
  chn <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133285/00000/en/false/xml/"
  cam <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133173/00000/en/false/xml/"
  sur <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133183/00000/en/false/xml/"
  
  usa_v <- inegiR::inegi_series(usa, token)
  names(usa_v)<-c("UnitedStates","Dates")
  can_v <- inegiR::inegi_series(can, token)
  names(can_v)<-c("Canada","Dates")
  chn_v <- inegiR::inegi_series(chn, token)
  names(chn_v)<-c("China","Dates")
  cam_v <- inegiR::inegi_series(cam, token)
  names(cam_v)<-c("CentralAmerica","Dates") 
  sur_v <- inegiR::inegi_series(sur, token)
  names(sur_v)<-c("SouthAmerica","Dates") 
  
  df <- Reduce(function(...) merge(..., all = TRUE),
             list(usa_v,can_v,chn_v,cam_v,sur_v))
  return(df)
}

#' @export
#' @rdname country_exports
series_exportaciones_pais<-function(token)
{ #exports por pais
  usa<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133172/00000/en/false/xml/"
  can<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133171/00000/en/false/xml/"
  chn<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133285/00000/en/false/xml/"
  cam<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133173/00000/en/false/xml/"
  sur<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133183/00000/en/false/xml/"
  
  usa_v<-inegiR::serie_inegi(usa, token)
  names(usa_v)<-c("Estados Unidos","Fechas")
  can_v<-inegiR::serie_inegi(can, token)
  names(can_v)<-c("Canada","Fechas")
  chn_v<-inegiR::serie_inegi(chn, token)
  names(chn_v)<-c("China","Fechas")
  cam_v<-inegiR::serie_inegi(cam, token)
  names(cam_v)<-c("Centro America","Fechas") 
  sur_v<-inegiR::serie_inegi(sur, token)
  names(sur_v)<-c("America del Sur","Fechas") 
  
  df<-Reduce(function(...) merge(..., all = TRUE),
             list(usa_v,can_v,chn_v,cam_v,sur_v))
  warning("This function is not being maintained. Use exports_country() instead.")
  return(df)
}