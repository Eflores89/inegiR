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
#' CrecimientoMex<-tasa_PIB(token)
#' @export
#' 

tasa_PIB<-function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-serie_inegi(s,token)
  t<-YoY(serie = i$Valores,lapso = 4,decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,Valores=t)
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
#' Desempleo<-tasa_desempleo(token)
#' @note Encoding no permite acentos en título de descripción
#' @export
#'

tasa_desempleo<-function(token)
{ #Retornar el desempleo
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d<-serie_inegi(s,token)
  
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
#' Comercio<-tasa_comercio(token)
#' @export
#'

tasa_comercio<-function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-serie_inegi(s,token)
    d<-cbind.data.frame(Fechas=d$Fechas,Valores=d$Valores,"YoY"=YoY(serie = d$Valores,lapso = 12, decimal=FALSE))
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
#' ActividadEconomica<-tasa_IGAE(token)
#' @export
#'

tasa_IGAE<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383152/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/405698/00000/en/false/xml/"
  
  i1<-serie_inegi(s1,token)
  i2<-serie_inegi(s2,token)
  
  t1<-YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
    t1<-cbind.data.frame(Fechas=i1$Fechas, "Serie Original (YoY)"=t1)
  t2<-YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
    t2<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (YoY)"=t2)
  t3<-YoY(serie = i2$Valores, lapso = 1, decimal=FALSE)
    t3<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (MoM)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=T),list(t1,
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
#' Sectores<-tasa_sectoresYoY(token)
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
  
  i1<-serie_inegi(s1,token)
  i2<-serie_inegi(s2,token)
  i3<-serie_inegi(s3,token)
  
  t1<-YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
    t1<-cbind.data.frame(Fechas=i1$Fechas, "Primarios (YoY)"=t1)
  t2<-YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
    t2<-cbind.data.frame(Fechas=i1$Fechas, "Secundarios (YoY)"=t2)
  t3<-YoY(serie = i3$Valores, lapso = 12, decimal=FALSE)
    t3<-cbind.data.frame(Fechas=i1$Fechas, "Terciarios (YoY)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=T),list(t1,
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
#' ConfianzaEconomia<-tasa_confianza(token)
#' @export
#'

tasa_confianza<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/63017/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/132944/00000/en/false/xml/"
  
  i1<-serie_inegi(s1,token)
  i2<-serie_inegi(s2,token)
  
  t1<-YoY(serie = i1$Valores, lapso = 12, decimal=FALSE)
  t1<-cbind.data.frame(Fechas=i1$Fechas, "Serie Original (YoY)"=t1)
  t2<-YoY(serie = i2$Valores, lapso = 12, decimal=FALSE)
  t2<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (YoY)"=t2)
  t3<-YoY(serie = i2$Valores, lapso = 1, decimal=FALSE)
  t3<-cbind.data.frame(Fechas=i1$Fechas, "Serie Desest. (MoM)"=t3)
  
  #union
  df<-Reduce(function(...) merge(...,all=T),list(t1,
                                                 t2,
                                                 t3))
  return(df)
}
