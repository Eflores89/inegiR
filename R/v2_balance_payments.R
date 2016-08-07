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
#' @name balance_payments
NULL

#' @export
#' @rdname balance_payments
balance_payments <- function(token)
{ 
  
  #with_all
  pre <-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  last <-"/00000/en/false/xml/"
  
  #Cuenta Corriente
  cc_ing<-inegiR::serie_inegi(paste0(pre,"214053",last),token)
  names(cc_ing)<-c("Current Account - Revenue","Dates")
  cc_egr<-inegiR::serie_inegi(paste0(pre,"214069",last),token)
  names(cc_egr)<-c("Current Account - Expense","Dates")
  cc_tot<-inegiR::serie_inegi(paste0(pre,"214052",last),token)
  names(cc_tot)<-c("Cuenta Corriente (Total)","Dates")
  
  #Cuenta Financiera
  cf_tot<-inegiR::serie_inegi(paste0(pre,"214088",last),token)
  names(cf_tot)<-c("Financial Account (Total)","Dates")
  cf_eyo<-inegiR::serie_inegi(paste0(pre,"214113",last),token)
  names(cf_eyo)<-c("Financial Account - Errors and Omisions","Dates")
  cf_res<-inegiR::serie_inegi(paste0(pre,"214114",last),token)
  names(cf_res)<-c("Financial Account - Reserves","Dates")
  cf_ajv<-inegiR::serie_inegi(paste0(pre,"214115",last),token)
  names(cf_ajv)<-c("Financial Account - Value adjustments","Dates")
  
  #union
  df <- Reduce(function(...) merge(..., all=TRUE),list(cc_tot,
                                                     cc_ing,
                                                     cc_egr,
                                                     cf_tot,
                                                     cf_res,
                                                     cf_eyo,
                                                     cf_ajv))
  return(df)
}
#' @export
#' @rdname balance_payments

series_balanza_pagos<-function(token)
{ #Retornar la Balanza de Pagos de Mexico
  
  #with_all
  pre <-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  last <-"/00000/en/false/xml/"
  
  #Cuenta Corriente
  cc_ing<-inegiR::serie_inegi(paste0(pre,"214053",last),token)
  names(cc_ing) <- c("Cuenta Corriente - Ingresos","Fechas")
  cc_egr<-inegiR::serie_inegi(paste0(pre,"214069",last),token)
  names(cc_egr) <- c("Cuenta Corriente - Egresos","Fechas")
  cc_tot<-inegiR::serie_inegi(paste0(pre,"214052",last),token)
  names(cc_tot) <- c("Cuenta Corriente (Total)","Fechas")
  
  #Cuenta Financiera
  cf_tot<-inegiR::serie_inegi(paste0(pre,"214088",last),token)
  names(cf_tot) <- c("Cuenta Financiera (Total)","Fechas")
  cf_eyo<-inegiR::serie_inegi(paste0(pre,"214113",last),token)
  names(cf_eyo) <- c("Cuenta Financiera - Errores y Omisiones","Fechas")
  cf_res<-inegiR::serie_inegi(paste0(pre,"214114",last),token)
  names(cf_res) <- c("Cuenta Financiera - Cambio en Reservas","Fechas")
  cf_ajv<-inegiR::serie_inegi(paste0(pre,"214115",last),token)
  names(cf_ajv) <- c("Cuenta Financiera - Ajustes en Valoracion","Fechas")
  
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