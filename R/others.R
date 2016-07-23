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
#' @export
#' 
#' 
trade_balance<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml/"
  
  x_val<-inegiR::inegi_series(x,token)
    names(x_val)<-c("Exports","Dates")
  m_val<-inegiR::inegi_series(m,token)
    names(m_val)<-c("Imports","Dates")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  d<-cbind.data.frame(Dates = df$Dates,
                      Exports = df$Exports,
                      Imports = df$Imports,
                      Balance = df$Exports-df$Imports)
  
  return(d)
}
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
#' @export
#' 
exports_country <- function(token)
{ #exports por pais
  usa<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133172/00000/en/false/xml/"
  can<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133171/00000/en/false/xml/"
  chn<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133285/00000/en/false/xml/"
  cam<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133173/00000/en/false/xml/"
  sur<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133183/00000/en/false/xml/"
    
  usa_v<-inegiR::inegi_series(usa, token)
    names(usa_v)<-c("UnitedStates","Dates")
  can_v<-inegiR::inegi_series(can, token)
    names(can_v)<-c("Canada","Dates")
  chn_v<-inegiR::inegi_series(chn, token)
    names(chn_v)<-c("China","Dates")
  cam_v<-inegiR::inegi_series(cam, token)
    names(cam_v)<-c("CentralAmerica","Dates") 
  sur_v<-inegiR::inegi_series(sur, token)
    names(sur_v)<-c("SouthAmerica","Dates") 
  
  df<-Reduce(function(...) merge(..., all = TRUE),
             list(usa_v,can_v,chn_v,cam_v,sur_v))
  return(df)
}

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
#' @export
#'

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


#' Balance of Payments
#'
#' Returns the main components of balance of payments.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' MxBoP <- balance_payments(token)
#' } 
#' @export
balance_payments <- function(token)
{ 
  
  #with_all
  pre <-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  last <-"/00000/en/false/xml/"
  
  #Cuenta Corriente
  cc_ing<-inegiR::serie_inegi(paste0(pre,"214053",last),token)
    names(cc_ing)<-c("Cuenta Corriente - Ingresos","Fechas")
  cc_egr<-inegiR::serie_inegi(paste0(pre,"214069",last),token)
    names(cc_egr)<-c("Cuenta Corriente - Egresos","Fechas")
  cc_tot<-inegiR::serie_inegi(paste0(pre,"214052",last),token)
    names(cc_tot)<-c("Cuenta Corriente (Total)","Fechas")
  
  #Cuenta Financiera
  cf_tot<-inegiR::serie_inegi(paste0(pre,"214088",last),token)
    names(cf_tot)<-c("Cuenta Financiera (Total)","Fechas")
  cf_eyo<-inegiR::serie_inegi(paste0(pre,"214113",last),token)
    names(cf_eyo)<-c("Cuenta Financiera - Errores y Omisiones","Fechas")
  cf_res<-inegiR::serie_inegi(paste0(pre,"214114",last),token)
    names(cf_res)<-c("Cuenta Financiera - Cambio en Reservas","Fechas")
  cf_ajv<-inegiR::serie_inegi(paste0(pre,"214115",last),token)
    names(cf_ajv)<-c("Cuenta Financiera - Ajustes en Valoracion","Fechas")
  
  #union
  df<-Reduce(function(...) merge(..., all=TRUE),list(cc_tot,
                                                 cc_ing,
                                                 cc_egr,
                                                 cf_tot,
                                                 cf_res,
                                                 cf_eyo,
                                                 cf_ajv))
  return(df)
}