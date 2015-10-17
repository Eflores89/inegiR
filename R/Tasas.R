#' Obtener tasa de crecimiento del PIB
#'
#' Obtiene tasa de crecimiento vs. mismo periodo de un año antes en porcentaje. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @note Ruta tematica BIE: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' CrecimientoMex<-tasa_PIB(token)
#' }
#' @export
#' 

tasa_PIB<-function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-inegiR::serie_inegi(s, token)
  t<-inegiR::YoY(serie = i$Valores, lapso = 4, decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas, Valores=t)
  return(d)
}

#' Obtener Desempleo Urbano
#'
#' Obtiene tasa de desocupación (serie unificada) urbana (agregado de 32 ciudades)
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Desempleo<-tasa_desempleo(token)
#' }
#' @export
#'

tasa_desempleo<-function(token)
{ #Retornar el desempleo
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d<-inegiR::serie_inegi(s,token)
  
  return(d)
}

#' Obtener Tasa de Crecimiento de Comercio
#'
#' Obtiene tasa de crecimiento del Comercio (Actividad Terciaria), por mes.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Comercio<-tasa_comercio(token)
#' }
#' @export
#'

tasa_comercio<-function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-inegiR::serie_inegi(s,token)
    d<-cbind.data.frame(Fechas=d$Fechas,Valores=d$Valores,"YoY"=YoY(serie = d$Valores,
                                                                    lapso = 12, 
                                                                    decimal = FALSE))
return(d)
}

#' Obtener IGAE
#'
#' Obtiene Tasas de Crecimiento de Indicador Global de Actividad Económica
#' Devuelve tasas de serie desestacionalizada anual, desestacionalizada contra mes previo y serie original anual.
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' ActividadEconomica<-tasa_IGAE(token)
#' }
#' @export
#'

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
  return(df)
}

#' Obtener cambios porcentuales por sector
#'
#' Obtiene Tasas de Crecimiento de Indicador Global de Actividad Económica por subsector. 
#' Todas las tasas son con series originales. Cambio porcentual anual. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Sectores<-tasa_sectoresYoY(token)
#' }
#' @export
#'

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
  return(df)
}

#' Obtener Confianza del Consumidor
#'
#' Obtiene Tasas de Cambio de Confianza del Consumidor
#' Devuelve tasas de serie desestacionalizada anual, desestacionalizada contra mes previo y serie original anual.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#' 
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' ConfianzaEconomia<-tasa_confianza(token)
#' }
#' @export
#'

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
  return(df)
}
