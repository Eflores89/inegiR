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
  
  x_val<-inegiR::serie_inegi(x,token)
    names(x_val)<-c("Exportaciones","Fechas")
  m_val<-inegiR::serie_inegi(m,token)
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
    
  usa_v<-inegiR::serie_inegi(usa,token)
    names(usa_v)<-c("Estados Unidos","Fechas")
  can_v<-inegiR::serie_inegi(can,token)
    names(can_v)<-c("Canadá","Fechas")
  chn_v<-inegiR::serie_inegi(chn,token)
    names(chn_v)<-c("China","Fechas")
  cam_v<-inegiR::serie_inegi(cam,token)
    names(cam_v)<-c("Centro América","Fechas") 
  sur_v<-inegiR::serie_inegi(sur,token)
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
  
  i<-inegiR::serie_inegi(s,token)
  t<-inegiR::YoY(serie=i$Valores, lapso=12, decimal=FALSE)
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
  
  i1<-inegiR::serie_inegi(s1,token)
  i2<-inegiR::serie_inegi(s2,token)
  i3<-inegiR::serie_inegi(s3,token)
  
  t1<-cbind.data.frame(Fechas=i1$Fechas,"Comercio (YoY)"=inegiR::YoY(i1$Valores,lapso=12,decimal=FALSE), "Comercio"= i1$Valores)
  t2<-cbind.data.frame(Fechas=i2$Fechas,"Manufacturas (YoY)"=inegiR::YoY(i2$Valores,lapso=12,decimal=FALSE), "Manufacturas"= i2$Valores)
  t3<-cbind.data.frame(Fechas=i3$Fechas,"Construcción (YoY)"=inegiR::YoY(i3$Valores,lapso=12,decimal=FALSE), "Construcción"= i3$Valores)
  
  df<-Reduce(function(...) merge(...,all=T),list(t1,
                                                 t2,
                                                 t3))
  return(df)  
}

#' Obtener crecimientos de actividad industrial
#'
#' Obtiene principales tasas de crecimiento YoY de componentes de Actividad Industrial (series originales): Construcción, Manufacturas, Minería y Generación de Luz y Agua.
#' Aun y cuando son las mismas series reportadas en el IGAE unas semanas después, estas pueden sufrir ajustes (ver documentación del INEGI así como número de indicador mediante metadata = TRUE).
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}.  
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame 
#'
#' @examples
#' ActividadIndustrial<-series_actividad_industrial(token)
#' @export
#'

series_actividad_industrial<-function(token)
{ #traer actividad empresarial por subsector
  #act industrial 
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/402752/00000/en/false/xml/"
  #construccion
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/402760/00000/en/false/xml/"
  #manuf
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/402764/00000/en/false/xml/"
  #mineria
  s4<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/402753/00000/en/false/xml/"
  #generacion 
  s5<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/402757/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1,token)
  i2<-inegiR::serie_inegi(s2,token)
  i3<-inegiR::serie_inegi(s3,token)
  i4<-inegiR::serie_inegi(s4,token)
  i5<-inegiR::serie_inegi(s5,token)
  
  t1<-cbind.data.frame(Fechas=i1$Fechas,"Actividad Industria (YoY)"=inegiR::YoY(i1$Valores,lapso=12,decimal=FALSE))
  t2<-cbind.data.frame(Fechas=i2$Fechas,"Construcción (YoY)"=inegiR::YoY(i2$Valores,lapso=12,decimal=FALSE))
  t3<-cbind.data.frame(Fechas=i3$Fechas,"Manufacturas (YoY)"=inegiR::YoY(i3$Valores,lapso=12,decimal=FALSE))
  t4<-cbind.data.frame(Fechas=i3$Fechas,"Minería (YoY)"=inegiR::YoY(i4$Valores,lapso=12,decimal=FALSE))
  t5<-cbind.data.frame(Fechas=i3$Fechas,"Generación Luz y Agua (YoY)"=inegiR::YoY(i5$Valores,lapso=12,decimal=FALSE))
  
  df<-Reduce(function(...) merge(...,all=T),list(t1,
                                                 t2,
                                                 t3,
                                                 t4,
                                                 t5))
  return(df)  
}
