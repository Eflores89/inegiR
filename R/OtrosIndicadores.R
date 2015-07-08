#' Obtener balanza comercial
#'
#' Obtiene exportaciones, importaciones y balance de los dos en un mismo data.frame por mes.
#' Todos los productos y todos los países. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#' 
#' @examples
#' ComercioExterior<-series_balanza_comercial(token)
#' @export
#' 
#' 
series_balanza_comercial<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml//"
  
  x_val<-serie_inegi(x,token)
    names(x_val)<-c("Exportaciones","Fechas")
  m_val<-serie_inegi(m,token)
    names(m_val)<-c("Importaciones","Fechas")
  
  df<-Reduce(function(...) merge(...,all=T),list(m_val,x_val))
  d<-cbind.data.frame(Fechas=df$Fechas,
                      Exportaciones=df$Exportaciones,
                      Importaciones=df$Importaciones,
                      Balance=df$Exportaciones-df$Importaciones)
  
  return(d)
}
#' Obtener exportaciones por paises
#'
#' Obtiene exportaciones de principales socios comerciales.
#' Todos los productos y Estados Unidos, Canadá, China, CentroAmerica y América del Sur. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' ExportacionesMx<-series_exportaciones_pais(token)
#' @note Encoding no permite acéntos en título de descripción
#' @export
#' 
series_exportaciones_pais<-function(token)
{ #exports por pais
  usa<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133172/00000/en/false/xml/"
  can<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133171/00000/en/false/xml/"
  chn<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133285/00000/en/false/xml/"
  cam<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133173/00000/en/false/xml/"
  sur<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133183/00000/en/false/xml/"
    
  usa_v<-serie_inegi(usa,token)
    names(usa_v)<-c("Estados Unidos","Fechas")
  can_v<-serie_inegi(can,token)
    names(can_v)<-c("Canadá","Fechas")
  chn_v<-serie_inegi(chn,token)
    names(chn_v)<-c("China","Fechas")
  cam_v<-serie_inegi(cam,token)
    names(cam_v)<-c("Centro América","Fechas") 
  sur_v<-serie_inegi(sur,token)
    names(sur_v)<-c("América del Sur","Fechas") 
  
  df<-Reduce(function(...) merge(...,all=T),list(usa_v,can_v,chn_v,cam_v,sur_v))
  return(df)
}

#' Obtener Produccion de Autos
#'
#' Obtiene producción automotriz en México y cambio porcentual anual.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' ProduccionAutos<-series_produccion_autos(token)
#' @note Encoding no permite acentos en título de descripción
#' @export
#'

series_produccion_autos<-function(token)
{ #Retornar la producción automotriz
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/15166/00000/en/false/xml/"
  
  i<-serie_inegi(s,token)
  t<-YoY(serie=i$Valores, lapso=12, decimal=FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,"Autos"=i$Valores,"YoY"=t)
  
  return(d)
}


#' Obtener Balanza de Pagos
#'
#' Obtiene principales componentes de la Balanza de Pagos: 2 de la Cuenta Corriente, 3 de la Cuenta Financiera y sus 2 resultados.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' BalanzadePagosMexico<-series_balanza_pagos(token)
#' @export
#'

series_balanza_pagos<-function(token)
{ #Retornar la Balanza de Pagos de México
  
  #with_all
  pre<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  last<-"/00000/en/false/xml/"
  
  #Cuenta Corriente
  cc_ing<-serie_inegi(paste0(pre,"214053",last),token)
    names(cc_ing)<-c("Cuenta Corriente - Ingresos","Fechas")
  cc_egr<-serie_inegi(paste0(pre,"214069",last),token)
    names(cc_egr)<-c("Cuenta Corriente - Egresos","Fechas")
  cc_tot<-serie_inegi(paste0(pre,"214052",last),token)
    names(cc_tot)<-c("Cuenta Corriente (Total)","Fechas")
  
  #Cuenta Financiera
  cf_tot<-serie_inegi(paste0(pre,"214088",last),token)
    names(cf_tot)<-c("Cuenta Financiera (Total)","Fechas")
  cf_eyo<-serie_inegi(paste0(pre,"214113",last),token)
    names(cf_eyo)<-c("Cuenta Financiera - Errores y Omisiones","Fechas")
  cf_res<-serie_inegi(paste0(pre,"214114",last),token)
    names(cf_res)<-c("Cuenta Financiera - Cambio en Reservas","Fechas")
  cf_ajv<-serie_inegi(paste0(pre,"214115",last),token)
    names(cf_ajv)<-c("Cuenta Financiera - Ajustes en Valoración","Fechas")
  
  #union
  df<-Reduce(function(...) merge(...,all=T),list(cc_tot,
                                                 cc_ing,
                                                 cc_egr,
                                                 cf_tot,
                                                 cf_res,
                                                 cf_eyo,
                                                 cf_ajv))
  return(df)
}

#' Obtener opiniones empresariales por sector
#'
#' Obtiene principales componentes de encuestas de Opinión Empresarial del INEGI dividido en 3 sectores: Comercio, Manufacturas y Construcción.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame 
#'
#' @examples
#' OpinionMexicanos<-series_opiniones(token)
#' @export
#'

series_opiniones<-function(token)
{ #traer opinión empresarial por subsector
  #comercio
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/437473/00000/en/false/xml/"
  #manuf
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/289075/00000/en/false/xml/"
  #construccion
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/437459/00000/en/false/xml/"
  
  i1<-serie_inegi(s1,token)
  i2<-serie_inegi(s2,token)
  i3<-serie_inegi(s3,token)
  
  t1<-cbind.data.frame(Fechas=i1$Fechas,"Comercio (YoY)"=YoY(i1$Valores,lapso=12,decimal=FALSE), "Comercio"= i1$Valores)
  t2<-cbind.data.frame(Fechas=i2$Fechas,"Manufacturas (YoY)"=YoY(i2$Valores,lapso=12,decimal=FALSE), "Manufacturas"= i2$Valores)
  t3<-cbind.data.frame(Fechas=i3$Fechas,"Construcción (YoY)"=YoY(i3$Valores,lapso=12,decimal=FALSE), "Construcción"= i3$Valores)
  
  df<-Reduce(function(...) merge(...,all=T),list(t1,
                                                 t2,
                                                 t3))
  return(df)  
}
