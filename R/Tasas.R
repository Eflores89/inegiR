#' Obtener tasa de crecimiento del PIB
#'
#' Obtiene tasa de crecimiento vs. mismo periodo de un año antes en porcentaje. 
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @note Ruta tematica BIE: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' CrecimientoMex<-Tasa_PIB(token)
#' @export
#' 

Tasa_PIB<-function (token){
  #Serie de PIB;
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i<-Serie_Inegi(s,token)
  t<-YoY(serie = i$Valores,lapso = 4,decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,Valores=t)
  return(d)
}

#' Obtener Desempleo Urbano
#'
#' Obtiene tasa de desocupación (serie unificada) urbana (agregado de 32 ciudades)
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' Desempleo<-Tasa_Desempleo(token)
#' @note Encoding no permite acentos en título de descripción
#' @export
#'

Tasa_Desempleo<-function(token)
{ #Retornar el desempleo
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/444612/00000/en/false/xml/"
  d<-Serie_Inegi(s,token)
  
  return(d)
}

#' Obtener Tasa de Crecimiento de Comercio
#'
#' Obtiene tasa de crecimiento del Comercio (Actividad Terciaria), por mes.
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' Comercio<-Tasa_Comercio(token)
#' @export
#'

Tasa_Comercio<-function(token)
{ #traer tasa de actividad terciaria - comercio.
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383160/00000/en/false/xml/"
  d<-Serie_Inegi(s,token)
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
#' ActividadEconomica<-Tasa_IGAE(token)
#' @export
#'

Tasa_IGAE<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383152/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/405698/00000/en/false/xml/"
  
  i1<-Serie_Inegi(s1,token)
  i2<-Serie_Inegi(s2,token)
  
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
