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
#' \dontrun{
#' token<-"webservice_token"
#' ComercioExterior<-series_balanza_comercial(token)
#' }
#' @export
#' 
#' 
series_balanza_comercial<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml/"
  
  x_val<-inegiR::serie_inegi(x,token)
    names(x_val)<-c("Exportaciones","Fechas")
  m_val<-inegiR::serie_inegi(m,token)
    names(m_val)<-c("Importaciones","Fechas")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
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
#' \dontrun{
#' token<-"webservice_token"
#' ExportacionesMx<-series_exportaciones_pais(token)
#' }
#' @export
#' 
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
#' \dontrun{
#' token<-"webservice_token"
#' ProduccionAutos<-series_produccion_autos(token)
#' }
#' @export
#'

series_produccion_autos<-function(token)
{ #Retornar la prod automotriz
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/15166/00000/en/false/xml/"
  
  i <- inegiR::serie_inegi(s, token)
  t <- inegiR::YoY(serie=i$Valores, lapso=12, decimal=FALSE)
  d <- cbind.data.frame(Fechas=i$Fechas,"Autos"=i$Valores,"YoY"=t)
  
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
#' \dontrun{
#' token<-"webservice_token"
#' BalanzadePagosMexico<-series_balanza_pagos(token)
#' } 
#' @export
#'

series_balanza_pagos<-function(token)
{ #Retornar la Balanza de Pagos de Mexico
  
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
#' \dontrun{
#' token<-"webservice_token"
#' OpinionMexicanos<-series_opiniones(token)
#' }
#' @export
#'

series_opiniones<-function(token)
{ #traer opinion empresarial por subsector
  #comercio
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/437473/00000/en/false/xml/"
  #manuf
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/289075/00000/en/false/xml/"
  #construccion
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/437459/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1,token)
  i2<-inegiR::serie_inegi(s2,token)
  i3<-inegiR::serie_inegi(s3,token)
  
  t1<-cbind.data.frame(Fechas=i1$Fechas,"Comercio (YoY)"=inegiR::YoY(i1$Valores,
                                                                     lapso=12,
                                                                     decimal=FALSE), 
                       "Comercio"= i1$Valores)
  t2<-cbind.data.frame(Fechas=i2$Fechas,"Manufacturas (YoY)"=inegiR::YoY(i2$Valores,
                                                                         lapso=12,
                                                                         decimal=FALSE), 
                       "Manufacturas"= i2$Valores)
  t3<-cbind.data.frame(Fechas=i3$Fechas,"Construccion (YoY)"=inegiR::YoY(i3$Valores,
                                                                         lapso=12,
                                                                         decimal=FALSE), 
                       "Construccion"= i3$Valores)
  
  df<-Reduce(function(...) merge(...,all=TRUE),list(t1, t2, t3))
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
#' \dontrun{
#' token<-"webservice_token"
#' ActividadIndustrial<-series_actividad_industrial(token)
#' }
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
  
  t1<-cbind.data.frame(Fechas=i1$Fechas,"Actividad Industria (YoY)"=inegiR::YoY(i1$Valores,
                                                                                lapso=12,
                                                                                decimal=FALSE))
  t2<-cbind.data.frame(Fechas=i2$Fechas,"Construccion (YoY)"=inegiR::YoY(i2$Valores,
                                                                         lapso=12,
                                                                         decimal=FALSE))
  t3<-cbind.data.frame(Fechas=i3$Fechas,"Manufacturas (YoY)"=inegiR::YoY(i3$Valores,
                                                                         lapso=12,
                                                                         decimal=FALSE))
  t4<-cbind.data.frame(Fechas=i3$Fechas,"Mineria (YoY)"=inegiR::YoY(i4$Valores,
                                                                    lapso=12,
                                                                    decimal=FALSE))
  t5<-cbind.data.frame(Fechas=i3$Fechas,"Generacion Luz y Agua (YoY)"=inegiR::YoY(i5$Valores,
                                                                                  lapso=12,
                                                                                  decimal=FALSE))
  
  df<-Reduce(function(...) merge(...,all=TRUE),list(t1, t2, t3, t4, t5))
  return(df)  
}
#' Obtener PIB por Entidad Federativa
#'
#' Obtiene series originales del PIB a Precios 2008 por Entidad Federativa. Unidades: millones de pesos a precios de 2008.
#' Es un wrapper de \code{serie_inegi()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame 
#' @seealso series_crecimiento_regiones
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Estados<-series_PIB_estados(token)
#' }
#' @export
#'

series_PIB_estados <- function(token)
{ # unidades: millones de pesos a precios de 2008...
  
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383217/00000/en/false/xml/"
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383218/00000/en/false/xml/"
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383219/00000/en/false/xml/"
  s4<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383220/00000/en/false/xml/"
  s5<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383221/00000/en/false/xml/"
  s6<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383222/00000/en/false/xml/"
  s7<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383223/00000/en/false/xml/"
  s8<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383224/00000/en/false/xml/"
  s9<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383225/00000/en/false/xml/"
  s10<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383226/00000/en/false/xml/"
  s11<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383227/00000/en/false/xml/"
  s12<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383228/00000/en/false/xml/"
  s13<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383229/00000/en/false/xml/"
  s14<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383230/00000/en/false/xml/"
  s15<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383231/00000/en/false/xml/"
  s16<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383232/00000/en/false/xml/"
  s17<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383233/00000/en/false/xml/"
  s18<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383234/00000/en/false/xml/"
  s19<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383235/00000/en/false/xml/"
  s20<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383236/00000/en/false/xml/"
  s21<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383237/00000/en/false/xml/"
  s22<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383238/00000/en/false/xml/"
  s23<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383239/00000/en/false/xml/"
  s24<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383240/00000/en/false/xml/"
  s25<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383241/00000/en/false/xml/"
  s26<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383242/00000/en/false/xml/"
  s27<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383243/00000/en/false/xml/"
  s28<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383244/00000/en/false/xml/"
  s29<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383245/00000/en/false/xml/"
  s30<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383246/00000/en/false/xml/"
  s31<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383247/00000/en/false/xml/"
  s32<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/383248/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1,token)
  names(i1)<-c("AGS","Fechas")
  i2<-inegiR::serie_inegi(s2,token)
  names(i2)<-c("BC","Fechas")
  i3<-inegiR::serie_inegi(s3,token)
  names(i3)<-c("BCS","Fechas")
  i4<-inegiR::serie_inegi(s4,token)
  names(i4)<-c("CAMP","Fechas")
  i5<-inegiR::serie_inegi(s5,token)
  names(i5)<-c("COAH","Fechas")
  i6<-inegiR::serie_inegi(s6,token)
  names(i6)<-c("COL","Fechas")
  i7<-inegiR::serie_inegi(s7,token)
  names(i7)<-c("CHPS","Fechas")
  i8<-inegiR::serie_inegi(s8,token)
  names(i8)<-c("CHIH","Fechas")
  i9<-inegiR::serie_inegi(s9,token)
  names(i9)<-c("CDMX","Fechas")
  i10<-inegiR::serie_inegi(s10,token)
  names(i10)<-c("DGO","Fechas")
  i11<-inegiR::serie_inegi(s11,token)
  names(i11)<-c("GTO","Fechas")
  i12<-inegiR::serie_inegi(s12,token)
  names(i12)<-c("GRO","Fechas")
  i13<-inegiR::serie_inegi(s13,token)
  names(i13)<-c("HGO","Fechas")
  i14<-inegiR::serie_inegi(s14,token)
  names(i14)<-c("JAL","Fechas")
  i15<-inegiR::serie_inegi(s15,token)
  names(i15)<-c("MEX","Fechas")
  i16<-inegiR::serie_inegi(s16,token)
  names(i16)<-c("MICH","Fechas")
  i17<-inegiR::serie_inegi(s17,token)
  names(i17)<-c("MOR","Fechas")
  i18<-inegiR::serie_inegi(s18,token)
  names(i18)<-c("NAY","Fechas")
  i19<-inegiR::serie_inegi(s19,token)
  names(i19)<-c("NL","Fechas")
  i20<-inegiR::serie_inegi(s20,token)
  names(i20)<-c("OAX","Fechas")
  i21<-inegiR::serie_inegi(s21,token)
  names(i21)<-c("PUE","Fechas") 
  i22<-inegiR::serie_inegi(s22,token)
  names(i22)<-c("QRO","Fechas")   
  i23<-inegiR::serie_inegi(s23,token)
  names(i23)<-c("QROO","Fechas")   
  i24<-inegiR::serie_inegi(s24,token)
  names(i24)<-c("SLP","Fechas")   
  i25<-inegiR::serie_inegi(s25,token)
  names(i25)<-c("SIN","Fechas")   
  i26<-inegiR::serie_inegi(s26,token)
  names(i26)<-c("SON","Fechas")   
  i27<-inegiR::serie_inegi(s27,token)
  names(i27)<-c("TAB","Fechas")   
  i28<-inegiR::serie_inegi(s28,token)
  names(i28)<-c("TAM","Fechas")   
  i29<-inegiR::serie_inegi(s29,token)
  names(i29)<-c("TLAX","Fechas")   
  i30<-inegiR::serie_inegi(s30,token)
  names(i30)<-c("VER","Fechas")   
  i31<-inegiR::serie_inegi(s31,token)
  names(i31)<-c("YUC","Fechas")   
  i32<-inegiR::serie_inegi(s32,token)
  names(i32)<-c("ZAC","Fechas")  
  
  df<-Reduce(function(...) merge(...,all=T),list(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,
                                                 i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,
                                                 i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,
                                                 i31,i32))
  return(df)  
}
#' Obtiene indicador de actividad por zona
#'
#' Obtiene índice de PIB por zona geográfica.
#' Este es un wrapper de \code{serie_inegi()}, con una metodología propia de En El Margen, se puede consultar la misma aquí: \url{http://enelmargen.org/eem/regiones/}. 
#' El crecimiento del mismo es una estimación de la dinámica de crecimiento regional nacional, inspirado en \url{http://www.banxico.org.mx/publicaciones-y-discursos/publicaciones/informes-periodicos/reportes-sobre-las-economias-regionales/{1C8EFC32-C12C-8393-6C29-5AF0A7F45686}.pdf}.
#' Los agrupamientos regionales provienen del INEGI \url{http://www.inegi.org.mx/est/contenidos/proyectos/cn/itaee/default.aspx}
#' 
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame 
#' @seealso series_PIB_estados, crecer, series_ITAE_estados
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' CrecimientoZonas<-series_crecimiento_regiones(token)
#' }
#' @export
#'
#'
series_crecimiento_regiones<-function(token)
{
  PIB_estados<-inegiR::series_PIB_estados(token)
  
  ##########################################################
  #### (DEL INEGI):
  # La Region Norte comprende las entidades de:
  # Baja California, Baja California Sur, Coahuila de Zaragoza,
  # Chihuahua, Nuevo Leon, Sinaloa, Sonora y Tamaulipas.
  Norte_PIB<-subset(x = PIB_estados,
                    select = c(Fechas,BC,BCS,
                               COAH,CHIH,SIN,
                               SON,NL,TAM))
  
  #ponderacion - se calculan ultimos 4 anios
  Norte_PIB$Total<-rowSums(x = subset(Norte_PIB,
                                      select = -c(Fechas)))
  Norte_l<-inegiR::ultimos(Norte_PIB, n = 4)
  
  #Estados - no se hace loop para dejar todo explicito
  BajaCalifornia_p<-mean(Norte_l$BC/Norte_l$Total)
  BajaCaliforniaSur_p<-mean(Norte_l$BCS/Norte_l$Total)
  Coahuila_p<-mean(Norte_l$COAH/Norte_l$Total)
  Chihuahua_p<-mean(Norte_l$CHIH/Norte_l$Total)
  Sinaloa_p<-mean(Norte_l$SIN/Norte_l$Total)
  Sonora_p<-mean(Norte_l$SON/Norte_l$Total)
  NuevoLeon_p<-mean(Norte_l$NL/Norte_l$Total)
  Tamaulipas_p<-mean(Norte_l$TAM/Norte_l$Total)
  
  ##########################################################
  # La Region Centro-Norte comprende las entidades de:
  # Aguascalientes, Colima, Durango, Guanajuato, Jalisco,
  # Nayarit, San Luis Potosi y Zacatecas.
  Centro_Nte_PIB<-subset(x = PIB_estados,
                         select = c(Fechas, AGS, COL,
                                    DGO, GTO, JAL, NAY, SLP, ZAC))
  #ponderacion
  Centro_Nte_PIB$Total<-rowSums(x = subset(Centro_Nte_PIB, select = -c(Fechas)))
  Centro_Nte_l<-inegiR::ultimos(Centro_Nte_PIB, n = 4)
  
  #Estados - no se hace loop para dejar todo explicito
  Aguascalientes_p<-mean(Centro_Nte_l$AGS/Centro_Nte_l$Total)
  Colima_p<-mean(Centro_Nte_l$COL/Centro_Nte_l$Total)
  Durango_p<-mean(Centro_Nte_l$DGO/Centro_Nte_l$Total)
  Guanajuato_p<-mean(Centro_Nte_l$GTO/Centro_Nte_l$Total)
  Jalisco_p<-mean(Centro_Nte_l$JAL/Centro_Nte_l$Total)
  Nayarit_p<-mean(Centro_Nte_l$NAY/Centro_Nte_l$Total)
  SanLuisPotosi_p<-mean(Centro_Nte_l$SLP/Centro_Nte_l$Total)
  Zacatecas_p<-mean(Centro_Nte_l$ZAC/Centro_Nte_l$Total)
  
  ##########################################################
  # La Region Centro comprende las entidades de:
  # El Distrito Federal y Mexico.
  Centro_PIB<-subset(x = PIB_estados,
                     select = c(Fechas, MEX, CDMX))
  #ponderacion
  Centro_PIB$Total<-rowSums(x = subset(Centro_PIB, 
                                       select = -c(Fechas)))
  Centro_l<-inegiR::ultimos(Centro_PIB, n = 4)
  
  #Estados - no se hace loop para dejar todo explicito
  EdoMexico_p<-mean(Centro_l$MEX/Centro_l$Total)
  DF_p<-mean(Centro_l$CDMX/Centro_l$Total)
  
  ##########################################################
  # La Region Centro-Sur comprende las entidades de:
  # Guerrero, Hidalgo, Michoacan de Ocampo,
  # Morelos, Puebla, Queretaro y Tlaxcala.
  Centro_Sur_PIB<-subset(x = PIB_estados,
                         select = c(Fechas, GRO, HGO, MICH, MOR,
                                    PUE, QRO, TLAX))
  #ponderacion
  Centro_Sur_PIB$Total<-rowSums(x = subset(Centro_Sur_PIB,select = -c(Fechas)))
  Centro_Sur_l<-inegiR::ultimos(Centro_Sur_PIB, n = 4)
  
  #Estados - no se hace loop para dejar todo explicito
  Guerrero_p<-mean(Centro_Sur_l$GRO/Centro_Sur_l$Total)
  Hidalgo_p<-mean(Centro_Sur_l$HGO/Centro_Sur_l$Total)
  Michoacan_p<-mean(Centro_Sur_l$MICH/Centro_Sur_l$Total)
  Morelos_p<-mean(Centro_Sur_l$MOR/Centro_Sur_l$Total)
  Puebla_p<-mean(Centro_Sur_l$PUE/Centro_Sur_l$Total)
  Queretaro_p<-mean(Centro_Sur_l$QRO/Centro_Sur_l$Total)
  Tlaxcala_p<-mean(Centro_Sur_l$TLAX/Centro_Sur_l$Total)
  
  ##########################################################
  # La Region Sur-Sureste comprende las entidades de:
  # Campeche, Chiapas, Oaxaca, Quintana Roo,
  # Tabasco, Veracruz de Ignacio de la Llave y Yucatan.
  Sur_PIB<-subset(x = PIB_estados,
                  select = c(Fechas, CAMP, CHPS, OAX, QROO,
                             TAB, VER, YUC))
  #ponderacion
  Sur_PIB$Total<-rowSums(x = subset(Sur_PIB,select = -c(Fechas)))
  Sur_l<-inegiR::ultimos(Sur_PIB, n = 4)
  
  #Estados - no se hace loop para dejar todo explicito
  Campeche_p<-mean(Sur_l$CAMP/Sur_l$Total)
  Chiapas_p<-mean(Sur_l$CHPS/Sur_l$Total)
  Oaxaca_p<-mean(Sur_l$OAX/Sur_l$Total)
  QuintanaRoo_p<-mean(Sur_l$QROO/Sur_l$Total)
  Tabasco_p<-mean(Sur_l$TAB/Sur_l$Total)
  Veracruz_p<-mean(Sur_l$VER/Sur_l$Total)
  Yucatan_p<-mean(Sur_l$YUC/Sur_l$Total)
  
  #################################################################
  ################## fin de todas las ponderaciones por estado
  ##################
  #### ahora, me voy a traer todos los ITAE's
  ITAEs<-inegiR::series_ITAE_estados(token)
  
  # solamente el "scope" del estudio (trimestres antes)
  ITAEs<-inegiR::ultimos(ITAEs, n = 16) # ultimos 16 trimestres (5 anios) - para calcular cambios de 12
  
  TasasCambio<-apply(ITAEs[,2:length(ITAEs)], 2, function(x) {1+inegiR::YoY(x,1)})
  TasasCambio<-cbind.data.frame(Fechas = ITAEs$Fechas, TasasCambio)
  
  ####################
  #### Calcular cambios e ITAE por zona ------------
  
  #Region Norte
  Norte_tasas<-subset(x = TasasCambio,
                      select = c(Fechas, BC, BCS,
                                 COAH, CHIH, SIN,
                                 SON, NL, TAM))
  Norte_ITAE<-subset(x = ITAEs,
                     select = c(Fechas, BC, BCS,
                                COAH, CHIH, SIN,
                                SON, NL, TAM))
  
  #Suma con crecimientos - 1 explicacion, solamente para norte.... 
  # voy a usar las tasas de crecimiento del ITAE de Baja california para crecer
  Baja<-inegiR::crecer(Norte_tasas$BC[2:length(Norte_tasas$BC)], 
                       # el ITAE original multiplicado por el peso que el estado tiene en la region
                       Norte_ITAE$BC[1]*BajaCalifornia_p)
  #### despues hago lo mismo para todos los estados y lo sumo...
  #### fin de explicacion, continuo con toda la zona....
  Norte<-(Baja+
            inegiR::crecer(Norte_tasas$BCS[2:length(Norte_tasas$BCS)],
                           Norte_ITAE$BCS[1]*BajaCaliforniaSur_p) +
            inegiR::crecer(Norte_tasas$COAH[2:length(Norte_tasas$COAH)],
                           Norte_ITAE$COAH[1]*Coahuila_p)+
            inegiR::crecer(Norte_tasas$CHIH[2:length(Norte_tasas$CHIH)],
                           Norte_ITAE$CHIH[1]*Chihuahua_p)+
            inegiR::crecer(Norte_tasas$SIN[2:length(Norte_tasas$SIN)],
                           Norte_ITAE$SIN[1]*Sinaloa_p)+
            inegiR::crecer(Norte_tasas$SON[2:length(Norte_tasas$SON)],
                           Norte_ITAE$SON[1]*Sonora_p)+
            inegiR::crecer(Norte_tasas$NL[2:length(Norte_tasas$NL)],
                           Norte_ITAE$NL[1]*NuevoLeon_p)+
            inegiR::crecer(Norte_tasas$TAM[2:length(Norte_tasas$TAM)],
                           Norte_ITAE$TAM[1]*Tamaulipas_p)
  )
  
  #Region Centro Norte
  Centro_Norte_tasas<-subset(x = TasasCambio,
                             select = c(Fechas, AGS, COL,
                                        DGO, GTO, JAL, NAY, SLP, ZAC))
  Centro_Norte_ITAE<-subset(x = ITAEs,
                            select = c(Fechas, AGS, COL,
                                       DGO, GTO, JAL, NAY, SLP, ZAC))
  Centro_Norte<-(inegiR::crecer(Centro_Norte_tasas$AGS[2:length(Centro_Norte_tasas$AGS)],
                                Centro_Norte_ITAE$AGS[1]*Aguascalientes_p)+
                   inegiR::crecer(Centro_Norte_tasas$COL[2:length(Centro_Norte_tasas$COL)],
                                  Centro_Norte_ITAE$COL[1]*Colima_p)+
                   inegiR::crecer(Centro_Norte_tasas$DGO[2:length(Centro_Norte_tasas$DGO)],
                                  Centro_Norte_ITAE$DGO[1]*Durango_p)+
                   inegiR::crecer(Centro_Norte_tasas$GTO[2:length(Centro_Norte_tasas$GTO)],
                                  Centro_Norte_ITAE$GTO[1]*Guanajuato_p)+
                   inegiR::crecer(Centro_Norte_tasas$JAL[2:length(Centro_Norte_tasas$JAL)],
                                  Centro_Norte_ITAE$JAL[1]*Jalisco_p)+
                   inegiR::crecer(Centro_Norte_tasas$NAY[2:length(Centro_Norte_tasas$NAY)],
                                  Centro_Norte_ITAE$NAY[1]*Nayarit_p)+
                   inegiR::crecer(Centro_Norte_tasas$SLP[2:length(Centro_Norte_tasas$SLP)],
                                  Centro_Norte_ITAE$SLP[1]*SanLuisPotosi_p)+
                   inegiR::crecer(Centro_Norte_tasas$ZAC[2:length(Centro_Norte_tasas$ZAC)],
                                  Centro_Norte_ITAE$ZAC[1]*Zacatecas_p)
  )
  
  #Region Centro
  Centro_tasas<-subset(x = TasasCambio,
                       select = c(Fechas, MEX, CDMX))
  Centro_ITAE<-subset(x = ITAEs,
                      select = c(Fechas, MEX, CDMX))
  
  Centro<-(inegiR::crecer(Centro_tasas$MEX[2:length(Centro_tasas$MEX)],
                          Centro_ITAE$MEX[1]*EdoMexico_p)+
             inegiR::crecer(Centro_tasas$CDMX[2:length(Centro_tasas$CDMX)],
                            Centro_ITAE$CDMX[1]*DF_p)
  )
  
  #Region Centro - Sur
  Centro_Sur_tasas<-subset(x = TasasCambio,
                           select = c(Fechas, GRO, HGO, MICH, MOR,
                                      PUE, QRO, TLAX))
  Centro_Sur_ITAE<-subset(x = ITAEs,
                          select = c(Fechas, GRO, HGO, MICH, MOR,
                                     PUE, QRO, TLAX))
  
  Centro_Sur<-(inegiR::crecer(Centro_Sur_tasas$GRO[2:length(Centro_Sur_tasas$GRO)],
                              Centro_Sur_ITAE$GRO[1]*Guerrero_p)+
                 inegiR::crecer(Centro_Sur_tasas$HGO[2:length(Centro_Sur_tasas$HGO)],
                                Centro_Sur_ITAE$HGO[1]*Hidalgo_p)+
                 inegiR::crecer(Centro_Sur_tasas$MICH[2:length(Centro_Sur_tasas$MICH)],
                                Centro_Sur_ITAE$MICH[1]*Michoacan_p)+
                 inegiR::crecer(Centro_Sur_tasas$MOR[2:length(Centro_Sur_tasas$MOR)],
                                Centro_Sur_ITAE$MOR[1]*Morelos_p)+
                 inegiR::crecer(Centro_Sur_tasas$PUE[2:length(Centro_Sur_tasas$PUE)],
                                Centro_Sur_ITAE$PUE[1]*Puebla_p)+
                 inegiR::crecer(Centro_Sur_tasas$QRO[2:length(Centro_Sur_tasas$QRO)],
                                Centro_Sur_ITAE$QRO[1]*Queretaro_p)+
                 inegiR::crecer(Centro_Sur_tasas$TLAX[2:length(Centro_Sur_tasas$TLAX)],
                                Centro_Sur_ITAE$TLAX[1]*Tlaxcala_p)
  )
  #Region Sur
  Sur_tasas<-subset(x = TasasCambio,
                    select = c(Fechas, CAMP, CHPS, OAX, QROO,
                               TAB, VER, YUC))
  Sur_ITAE<-subset(x = ITAEs,
                   select = c(Fechas, CAMP, CHPS, OAX, QROO,
                              TAB, VER, YUC))
  
  Sur<-(inegiR::crecer(Sur_tasas$CAMP[2:length(Sur_tasas$CAMP)],
                       Sur_ITAE$CAMP[1]*Campeche_p)+
          inegiR::crecer(Sur_tasas$CHPS[2:length(Sur_tasas$CHPS)],
                         Sur_ITAE$CHPS[1]*Chiapas_p)+
          inegiR::crecer(Sur_tasas$OAX[2:length(Sur_tasas$OAX)],
                         Sur_ITAE$OAX[1]*Oaxaca_p)+
          inegiR::crecer(Sur_tasas$QROO[2:length(Sur_tasas$QROO)],
                         Sur_ITAE$QROO[1]*QuintanaRoo_p)+
          inegiR::crecer(Sur_tasas$TAB[2:length(Sur_tasas$TAB)],
                         Sur_ITAE$TAB[1]*Tabasco_p)+
          inegiR::crecer(Sur_tasas$VER[2:length(Sur_tasas$VER)],
                         Sur_ITAE$VER[1]*Veracruz_p)+
          inegiR::crecer(Sur_tasas$YUC[2:length(Sur_tasas$YUC)],
                         Sur_ITAE$YUC[1]*Yucatan_p)
  )
  
  salida<-cbind.data.frame("Fechas" = ITAEs$Fechas[2:length(ITAEs$Fechas)],
                           "Norte" = Norte,
                           "Centro_Norte" = Centro_Norte,
                           "Centro" = Centro, 
                           "Centro_Sur" = Centro_Sur,
                           "Sur" = Sur)
  return(salida)
}

#' Obtiene ITAE
#'
#' Obtiene series originales de Indicador Trimestral de Actividad Económica (ITAE) por estado.
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#' @seealso series_PIB_estados, series_crecimiento_regiones
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' ITAE<-series_ITAE_estados(token)
#' }
#' @export
series_ITAE_estados<-function(token)
{
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428026/00000/en/false/xml/"
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428027/00000/en/false/xml/"
  s3<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428028/00000/en/false/xml/"
  s4<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428029/00000/en/false/xml/"
  s5<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428031/00000/en/false/xml/"
  s6<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428033/00000/en/false/xml/"
  s7<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428034/00000/en/false/xml/"
  s8<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428036/00000/en/false/xml/"
  s9<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428037/00000/en/false/xml/"
  s10<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428038/00000/en/false/xml/"
  s11<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428039/00000/en/false/xml/"
  s12<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428040/00000/en/false/xml/"
  s13<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428041/00000/en/false/xml/"
  s14<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428042/00000/en/false/xml/"
  s15<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428043/00000/en/false/xml/"
  s16<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428044/00000/en/false/xml/"
  s17<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428045/00000/en/false/xml/"
  #18 nayarit
  s18<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428046/00000/en/false/xml/"
  s19<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428047/00000/en/false/xml/"
  s20<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428049/00000/en/false/xml/"
  s21<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428050/00000/en/false/xml/"
  s22<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428052/00000/en/false/xml/"
  #23 quintana roo
  s23<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428053/00000/en/false/xml/"
  s24<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428054/00000/en/false/xml/"
  s25<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428056/00000/en/false/xml/"
  s26<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428057/00000/en/false/xml/"
  s27<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428058/00000/en/false/xml/"
  s28<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428060/00000/en/false/xml/"
  #29 tlaxcala
  s29<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428062/00000/en/false/xml/"
  s30<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428063/00000/en/false/xml/"
  s31<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428065/00000/en/false/xml/"
  s32<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/428066/00000/en/false/xml/"
  
  i1<-inegiR::serie_inegi(s1, token)
  names(i1)<-c("AGS","Fechas")
  i2<-inegiR::serie_inegi(s2,token)
  names(i2)<-c("BC","Fechas")
  i3<-inegiR::serie_inegi(s3,token)
  names(i3)<-c("BCS","Fechas")
  i4<-inegiR::serie_inegi(s4,token)
  names(i4)<-c("CAMP","Fechas")
  i5<-inegiR::serie_inegi(s5,token)
  names(i5)<-c("COAH","Fechas")
  i6<-inegiR::serie_inegi(s6,token)
  names(i6)<-c("COL","Fechas")
  i7<-inegiR::serie_inegi(s7,token)
  names(i7)<-c("CHPS","Fechas")
  i8<-inegiR::serie_inegi(s8,token)
  names(i8)<-c("CHIH","Fechas")
  i9<-inegiR::serie_inegi(s9,token)
  names(i9)<-c("CDMX","Fechas")
  i10<-inegiR::serie_inegi(s10,token)
  names(i10)<-c("DGO","Fechas")
  i11<-inegiR::serie_inegi(s11,token)
  names(i11)<-c("GTO","Fechas")
  i12<-inegiR::serie_inegi(s12,token)
  names(i12)<-c("GUE","Fechas")
  i13<-inegiR::serie_inegi(s13,token)
  names(i13)<-c("HGO","Fechas")
  i14<-inegiR::serie_inegi(s14,token)
  names(i14)<-c("JAL","Fechas")
  i15<-inegiR::serie_inegi(s15,token)
  names(i15)<-c("MEX","Fechas")
  i16<-inegiR::serie_inegi(s16,token)
  names(i16)<-c("MICH","Fechas")
  i17<-inegiR::serie_inegi(s17,token)
  names(i17)<-c("MOR","Fechas")
  i18<-inegiR::serie_inegi(s18,token)
  names(i18)<-c("NAY","Fechas")
  i19<-inegiR::serie_inegi(s19,token)
  names(i19)<-c("NL","Fechas")
  i20<-inegiR::serie_inegi(s20,token)
  names(i20)<-c("OAX","Fechas")
  i21<-inegiR::serie_inegi(s21,token)
  names(i21)<-c("PUE","Fechas")
  i22<-inegiR::serie_inegi(s22,token)
  names(i22)<-c("QRO","Fechas")
  i23<-inegiR::serie_inegi(s23,token)
  names(i23)<-c("QROO","Fechas")
  i24<-inegiR::serie_inegi(s24,token)
  names(i24)<-c("SLP","Fechas")
  i25<-inegiR::serie_inegi(s25,token)
  names(i25)<-c("SIN","Fechas")
  i26<-inegiR::serie_inegi(s26,token)
  names(i26)<-c("SON","Fechas")
  i27<-inegiR::serie_inegi(s27,token)
  names(i27)<-c("TAB","Fechas")
  i28<-inegiR::serie_inegi(s28,token)
  names(i28)<-c("TAM","Fechas")
  i29<-inegiR::serie_inegi(s29,token)
  names(i29)<-c("TLAX","Fechas")
  i30<-inegiR::serie_inegi(s30,token)
  names(i30)<-c("VER","Fechas")
  i31<-inegiR::serie_inegi(s31,token)
  names(i31)<-c("YUC","Fechas")
  i32<-inegiR::serie_inegi(s32,token)
  names(i32)<-c("ZAC","Fechas")
  
  df<-Reduce(function(...) merge(...,all = TRUE),list(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,
                                                      i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,
                                                      i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,
                                                      i31,i32))
  return(df)
}
#' Obtener Tipo de Cambio Peso - USD
#'
#' Obtiene tipo de cambio interbancario (venta) histórico de pesos a dólares.
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' USD<-series_tipocambio(token)
#' }
#' @export
#'

series_tipocambio<-function(token)
{ 
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/824/00000/en/false/xml/"
  
  d<-inegiR::serie_inegi(s, token)
  return(d)
}
#' Obtener productividad de manufactura por estado
#'
#' Obtiene la productivdad de la manufactura por estado. Se calcula a partir de las series de personal ocupado y de valor de la producción de 
#' la Encuesta Mensual de la Industria Manufacturera (EMIM) al dividirse valor (en miles de pesos) entre personal (personas).
#' Es un wrapper de la función \code{serie_inegi()}.
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Productividad <- series_productividad_man(token)
#' }
#' @export
#' 
#' 
series_productividad_man <- function(token)
{ 
  # series de personal ocupado... 
  sp1 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294731/00000/en/false/xml/"
  sp2 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294745/00000/en/false/xml/"
  sp3 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294766/00000/en/false/xml/"
  sp4 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294767/00000/en/false/xml/"
  sp5 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294768/00000/en/false/xml/"
  sp6 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294788/00000/en/false/xml/"
  sp7 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294789/00000/en/false/xml/"
  sp8 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294790/00000/en/false/xml/"
  sp9 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294810/00000/en/false/xml/"
  sp10 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294831/00000/en/false/xml/"
  sp11 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294842/00000/en/false/xml/"
  sp12 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294861/00000/en/false/xml/"
  sp13 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294862/00000/en/false/xml/"
  sp14 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294877/00000/en/false/xml/"
  sp15 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294899/00000/en/false/xml/"
  sp16 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294921/00000/en/false/xml/"
  sp17 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294933/00000/en/false/xml/"
  sp18 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294934/00000/en/false/xml/"
  sp19 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294935/00000/en/false/xml/"
  sp20 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294956/00000/en/false/xml/"
  sp21 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294957/00000/en/false/xml/"
  sp22 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294975/00000/en/false/xml/"
  sp23 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294990/00000/en/false/xml/"
  sp24 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/294991/00000/en/false/xml/"
  sp25 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295008/00000/en/false/xml/"
  sp26 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295009/00000/en/false/xml/"
  sp27 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295026/00000/en/false/xml/"
  sp28 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295027/00000/en/false/xml/"
  sp29 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295044/00000/en/false/xml/"
  sp30 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295045/00000/en/false/xml/"
  sp31 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295058/00000/en/false/xml/"
  sp32 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295059/00000/en/false/xml/"
  
  # series de valor de producido....
  sv1 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295389/00000/en/false/xml/"
  sv2 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295403/00000/en/false/xml/"
  sv3 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295424/00000/en/false/xml/"
  sv4 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295425/00000/en/false/xml/"
  sv5 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295426/00000/en/false/xml/"
  sv6 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295446/00000/en/false/xml/"
  sv7 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295447/00000/en/false/xml/"
  sv8 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295448/00000/en/false/xml/"
  sv9 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295468/00000/en/false/xml/"
  sv10 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295489/00000/en/false/xml/"
  sv11 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295500/00000/en/false/xml/"
  sv12 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295519/00000/en/false/xml/"
  sv13 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295520/00000/en/false/xml/"
  sv14 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295535/00000/en/false/xml/"
  sv15 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295557/00000/en/false/xml/"
  sv16 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295579/00000/en/false/xml/"
  sv17 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295591/00000/en/false/xml/"
  sv18 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295592/00000/en/false/xml/"
  sv19 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295593/00000/en/false/xml/"
  sv20 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295614/00000/en/false/xml/"
  sv21 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295615/00000/en/false/xml/"
  sv22 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295633/00000/en/false/xml/"
  sv23 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295648/00000/en/false/xml/"
  sv24 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295649/00000/en/false/xml/"
  sv25 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295666/00000/en/false/xml/"
  sv26 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295667/00000/en/false/xml/"
  sv27 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295684/00000/en/false/xml/"
  sv28 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295685/00000/en/false/xml/"
  sv29 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295702/00000/en/false/xml/"
  sv30 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295703/00000/en/false/xml/"
  sv31 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295716/00000/en/false/xml/"
  sv32 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/295717/00000/en/false/xml/"
  
  # descargar las series de personal...
  spd1 <- inegiR::serie_inegi(sp1, token)
  spd2 <- inegiR::serie_inegi(sp2, token)
  spd3 <- inegiR::serie_inegi(sp3, token)
  spd4 <- inegiR::serie_inegi(sp4, token)
  spd5 <- inegiR::serie_inegi(sp5, token)
  spd6 <- inegiR::serie_inegi(sp6, token)
  spd7 <- inegiR::serie_inegi(sp7, token)
  spd8 <- inegiR::serie_inegi(sp8, token)
  spd9 <- inegiR::serie_inegi(sp9, token)
  spd10 <- inegiR::serie_inegi(sp10, token)
  spd11 <- inegiR::serie_inegi(sp11, token)
  spd12 <- inegiR::serie_inegi(sp12, token)
  spd13 <- inegiR::serie_inegi(sp13, token)
  spd14 <- inegiR::serie_inegi(sp14, token)
  spd15 <- inegiR::serie_inegi(sp15, token)
  spd16 <- inegiR::serie_inegi(sp16, token)
  spd17 <- inegiR::serie_inegi(sp17, token)
  spd18 <- inegiR::serie_inegi(sp18, token)
  spd19 <- inegiR::serie_inegi(sp19, token)
  spd20 <- inegiR::serie_inegi(sp20, token)
  spd21 <- inegiR::serie_inegi(sp21, token)
  spd22 <- inegiR::serie_inegi(sp22, token)
  spd23 <- inegiR::serie_inegi(sp23, token)
  spd24 <- inegiR::serie_inegi(sp24, token)
  spd25 <- inegiR::serie_inegi(sp25, token)
  spd26 <- inegiR::serie_inegi(sp26, token)
  spd27 <- inegiR::serie_inegi(sp27, token)
  spd28 <- inegiR::serie_inegi(sp28, token)
  spd29 <- inegiR::serie_inegi(sp29, token)
  spd30 <- inegiR::serie_inegi(sp30, token)
  spd31 <- inegiR::serie_inegi(sp31, token)
  spd32 <- inegiR::serie_inegi(sp32, token)
  
  # descargar las series de valor 
  svd1 <- inegiR::serie_inegi(sv1, token)
  svd2 <- inegiR::serie_inegi(sv2, token)
  svd3 <- inegiR::serie_inegi(sv3, token)
  svd4 <- inegiR::serie_inegi(sv4, token)
  svd5 <- inegiR::serie_inegi(sv5, token)
  svd6 <- inegiR::serie_inegi(sv6, token)
  svd7 <- inegiR::serie_inegi(sv7, token)
  svd8 <- inegiR::serie_inegi(sv8, token)
  svd9 <- inegiR::serie_inegi(sv9, token)
  svd10 <- inegiR::serie_inegi(sv10, token)
  svd11 <- inegiR::serie_inegi(sv11, token)
  svd12 <- inegiR::serie_inegi(sv12, token)
  svd13 <- inegiR::serie_inegi(sv13, token)
  svd14 <- inegiR::serie_inegi(sv14, token)
  svd15 <- inegiR::serie_inegi(sv15, token)
  svd16 <- inegiR::serie_inegi(sv16, token)
  svd17 <- inegiR::serie_inegi(sv17, token)
  svd18 <- inegiR::serie_inegi(sv18, token)
  svd19 <- inegiR::serie_inegi(sv19, token)
  svd20 <- inegiR::serie_inegi(sv20, token)
  svd21 <- inegiR::serie_inegi(sv21, token)
  svd22 <- inegiR::serie_inegi(sv22, token)
  svd23 <- inegiR::serie_inegi(sv23, token)
  svd24 <- inegiR::serie_inegi(sv24, token)
  svd25 <- inegiR::serie_inegi(sv25, token)
  svd26 <- inegiR::serie_inegi(sv26, token)
  svd27 <- inegiR::serie_inegi(sv27, token)
  svd28 <- inegiR::serie_inegi(sv28, token)
  svd29 <- inegiR::serie_inegi(sv29, token)
  svd30 <- inegiR::serie_inegi(sv30, token)
  svd31 <- inegiR::serie_inegi(sv31, token)
  svd32 <- inegiR::serie_inegi(sv32, token)
  
  # crear serie de productividad...
  p1 <- svd1$Valores/spd1$Valores
  p2 <- svd2$Valores/spd2$Valores
  p3 <- svd3$Valores/spd3$Valores
  p4 <- svd4$Valores/spd4$Valores
  p5 <- svd5$Valores/spd5$Valores
  p6 <- svd6$Valores/spd6$Valores
  p7 <- svd7$Valores/spd7$Valores
  p8 <- svd8$Valores/spd8$Valores
  p9 <- svd9$Valores/spd9$Valores
  p10 <- svd10$Valores/spd10$Valores
  p11 <- svd11$Valores/spd11$Valores
  p12 <- svd12$Valores/spd12$Valores
  p13 <- svd13$Valores/spd13$Valores
  p14 <- svd14$Valores/spd14$Valores
  p15 <- svd15$Valores/spd15$Valores
  p16 <- svd16$Valores/spd16$Valores
  p17 <- svd17$Valores/spd17$Valores
  p18 <- svd18$Valores/spd18$Valores
  p19 <- svd19$Valores/spd19$Valores
  p20 <- svd20$Valores/spd20$Valores
  p21 <- svd21$Valores/spd21$Valores
  p22 <- svd22$Valores/spd22$Valores
  p23 <- svd23$Valores/spd23$Valores
  p24 <- svd24$Valores/spd24$Valores
  p25 <- svd25$Valores/spd25$Valores
  p26 <- svd26$Valores/spd26$Valores
  p27 <- svd27$Valores/spd27$Valores
  p28 <- svd28$Valores/spd28$Valores
  p29 <- svd29$Valores/spd29$Valores
  p30 <- svd30$Valores/spd30$Valores
  p31 <- svd31$Valores/spd31$Valores
  p32 <- svd32$Valores/spd32$Valores
  
  # nombrar, para unir... 
  pd1 <- cbind.data.frame("AGS" = p1, 
                          "Fechas" = svd1$Fechas)
  pd2 <- cbind.data.frame("BC" = p2, 
                          "Fechas" = svd2$Fechas)
  pd3 <- cbind.data.frame("BCS" = p3, 
                          "Fechas" = svd3$Fechas)
  pd4 <- cbind.data.frame("CAMP" = p4, 
                          "Fechas" = svd3$Fechas)
  pd5 <- cbind.data.frame("COAH" = p5, 
                          "Fechas" = svd3$Fechas)
  pd6 <- cbind.data.frame("COL" = p6, 
                          "Fechas" = svd3$Fechas)
  pd7 <- cbind.data.frame("CHPS" = p7, 
                          "Fechas" = svd3$Fechas)
  pd8 <- cbind.data.frame("CHIH" = p8, 
                          "Fechas" = svd3$Fechas)
  pd9 <- cbind.data.frame("CDMX" = p9, 
                          "Fechas" = svd3$Fechas)
  pd10 <- cbind.data.frame("DGO" = p10, 
                           "Fechas" = svd3$Fechas)
  pd11 <- cbind.data.frame("GTO" = p11, 
                           "Fechas" = svd3$Fechas)
  pd12 <- cbind.data.frame("GRO" = p12, 
                           "Fechas" = svd3$Fechas)
  pd13 <- cbind.data.frame("HGO" = p13, 
                           "Fechas" = svd3$Fechas)
  pd14 <- cbind.data.frame("JAL" = p14, 
                           "Fechas" = svd3$Fechas)
  pd15 <- cbind.data.frame("MEX" = p15, 
                           "Fechas" = svd3$Fechas)
  pd16 <- cbind.data.frame("MICH" = p16, 
                           "Fechas" = svd3$Fechas)
  pd17 <- cbind.data.frame("MOR" = p17, 
                           "Fechas" = svd3$Fechas)
  pd18 <- cbind.data.frame("NAY" = p18, 
                           "Fechas" = svd3$Fechas)
  pd19 <- cbind.data.frame("NL" = p19, 
                           "Fechas" = svd3$Fechas)
  pd20 <- cbind.data.frame("OAX" = p20, 
                           "Fechas" = svd3$Fechas)
  pd21 <- cbind.data.frame("PUE" = p21, 
                           "Fechas" = svd3$Fechas)
  pd22 <- cbind.data.frame("QRO" = p22, 
                           "Fechas" = svd3$Fechas)
  pd23 <- cbind.data.frame("QROO" = p23, 
                           "Fechas" = svd3$Fechas)
  pd24 <- cbind.data.frame("SLP" = p24, 
                           "Fechas" = svd3$Fechas)
  pd25 <- cbind.data.frame("SIN" = p25, 
                           "Fechas" = svd3$Fechas)
  pd26 <- cbind.data.frame("SON" = p26, 
                           "Fechas" = svd3$Fechas)
  pd27 <- cbind.data.frame("TAB" = p27, 
                           "Fechas" = svd3$Fechas)
  pd28 <- cbind.data.frame("TAM" = p28, 
                           "Fechas" = svd3$Fechas)
  pd29 <- cbind.data.frame("TLAX" = p29, 
                           "Fechas" = svd3$Fechas)
  pd30 <- cbind.data.frame("VER" = p30, 
                           "Fechas" = svd3$Fechas)
  pd31 <- cbind.data.frame("YUC" = p31, 
                           "Fechas" = svd3$Fechas)
  pd32 <- cbind.data.frame("ZAC" = p32, 
                           "Fechas" = svd3$Fechas)
  
  df<-Reduce(function(...) merge(..., all=TRUE), 
             list(pd1, pd2, pd3, pd4, pd5, pd6, pd7, pd8, pd9, pd10,
                  pd11, pd12, pd13, pd14, pd15, pd16, pd17, pd18, pd19, pd20, 
                  pd21, pd22, pd23, pd24, pd25, pd26, pd27, pd28, pd29, pd30, 
                  pd31, pd32
             ))
  return(df)
}
#' Obtener productividad de construccion por estado
#'
#' Obtiene la productivdad de la construcción generada por estado. Se calcula a partir de las series de personal ocupado y de valor de la producción de 
#' la Encuesta Nacional de Empresas Constructoras (ENEC) al dividirse valor (en miles de pesos a precios corrientes) entre personal (personas).
#' Es un wrapper de la función \code{serie_inegi()}.
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Productividad <- series_productividad_const(token)
#' }
#' @export
#' 
#' 
series_productividad_const <- function(token)
{ 
  # series de personal ocupado... 
  sp1 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291123/00000/en/false/xml/"
  sp2 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291124/00000/en/false/xml/"
  sp3 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291125/00000/en/false/xml/"
  sp4 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291126/00000/en/false/xml/"
  sp5 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291127/00000/en/false/xml/"
  sp6 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291128/00000/en/false/xml/"
  sp7 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291129/00000/en/false/xml/"
  sp8 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291130/00000/en/false/xml/"
  sp9 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291131/00000/en/false/xml/"
  sp10 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291132/00000/en/false/xml/"
  sp11 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291133/00000/en/false/xml/"
  sp12 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291134/00000/en/false/xml/"
  sp13 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291135/00000/en/false/xml/"
  sp14 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291136/00000/en/false/xml/"
  sp15 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291137/00000/en/false/xml/"
  sp16 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291138/00000/en/false/xml/"
  sp17 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291139/00000/en/false/xml/"
  sp18 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291140/00000/en/false/xml/"
  sp19 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291141/00000/en/false/xml/"
  sp20 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291142/00000/en/false/xml/"
  sp21 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291143/00000/en/false/xml/"
  sp22 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291144/00000/en/false/xml/"
  sp23 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291145/00000/en/false/xml/"
  sp24 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291146/00000/en/false/xml/"
  sp25 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291147/00000/en/false/xml/"
  sp26 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291148/00000/en/false/xml/"
  sp27 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291149/00000/en/false/xml/"
  sp28 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291150/00000/en/false/xml/"
  sp29 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291151/00000/en/false/xml/"
  sp30 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291152/00000/en/false/xml/"
  sp31 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291153/00000/en/false/xml/"
  sp32 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/291154/00000/en/false/xml/"
  
  # series de valor de producido....
  sv1 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293136/00000/en/false/xml/"
  sv2 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293137/00000/en/false/xml/"
  sv3 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293138/00000/en/false/xml/"
  sv4 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293139/00000/en/false/xml/"
  sv5 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293140/00000/en/false/xml/"
  sv6 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293141/00000/en/false/xml/"
  sv7 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293142/00000/en/false/xml/"
  sv8 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293143/00000/en/false/xml/"
  sv9 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293144/00000/en/false/xml/"
  sv10 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293145/00000/en/false/xml/"
  sv11 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293146/00000/en/false/xml/"
  sv12 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293147/00000/en/false/xml/"
  sv13 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293148/00000/en/false/xml/"
  sv14 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293149/00000/en/false/xml/"
  sv15 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293150/00000/en/false/xml/"
  sv16 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293151/00000/en/false/xml/"
  sv17 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293152/00000/en/false/xml/"
  sv18 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293153/00000/en/false/xml/"
  sv19 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293154/00000/en/false/xml/"
  sv20 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293155/00000/en/false/xml/"
  sv21 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293156/00000/en/false/xml/"
  sv22 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293157/00000/en/false/xml/"
  sv23 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293158/00000/en/false/xml/"
  sv24 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293159/00000/en/false/xml/"
  sv25 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293160/00000/en/false/xml/"
  sv26 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293161/00000/en/false/xml/"
  sv27 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293162/00000/en/false/xml/"
  sv28 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293163/00000/en/false/xml/"
  sv29 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293164/00000/en/false/xml/"
  sv30 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293165/00000/en/false/xml/"
  sv31 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293166/00000/en/false/xml/"
  sv32 <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/293167/00000/en/false/xml/"
  
  # descargar las series de personal...
  spd1 <- inegiR::serie_inegi(sp1, token)
  spd2 <- inegiR::serie_inegi(sp2, token)
  spd3 <- inegiR::serie_inegi(sp3, token)
  spd4 <- inegiR::serie_inegi(sp4, token)
  spd5 <- inegiR::serie_inegi(sp5, token)
  spd6 <- inegiR::serie_inegi(sp6, token)
  spd7 <- inegiR::serie_inegi(sp7, token)
  spd8 <- inegiR::serie_inegi(sp8, token)
  spd9 <- inegiR::serie_inegi(sp9, token)
  spd10 <- inegiR::serie_inegi(sp10, token)
  spd11 <- inegiR::serie_inegi(sp11, token)
  spd12 <- inegiR::serie_inegi(sp12, token)
  spd13 <- inegiR::serie_inegi(sp13, token)
  spd14 <- inegiR::serie_inegi(sp14, token)
  spd15 <- inegiR::serie_inegi(sp15, token)
  spd16 <- inegiR::serie_inegi(sp16, token)
  spd17 <- inegiR::serie_inegi(sp17, token)
  spd18 <- inegiR::serie_inegi(sp18, token)
  spd19 <- inegiR::serie_inegi(sp19, token)
  spd20 <- inegiR::serie_inegi(sp20, token)
  spd21 <- inegiR::serie_inegi(sp21, token)
  spd22 <- inegiR::serie_inegi(sp22, token)
  spd23 <- inegiR::serie_inegi(sp23, token)
  spd24 <- inegiR::serie_inegi(sp24, token)
  spd25 <- inegiR::serie_inegi(sp25, token)
  spd26 <- inegiR::serie_inegi(sp26, token)
  spd27 <- inegiR::serie_inegi(sp27, token)
  spd28 <- inegiR::serie_inegi(sp28, token)
  spd29 <- inegiR::serie_inegi(sp29, token)
  spd30 <- inegiR::serie_inegi(sp30, token)
  spd31 <- inegiR::serie_inegi(sp31, token)
  spd32 <- inegiR::serie_inegi(sp32, token)
  
  # descargar las series de valor 
  svd1 <- inegiR::serie_inegi(sv1, token)
  svd2 <- inegiR::serie_inegi(sv2, token)
  svd3 <- inegiR::serie_inegi(sv3, token)
  svd4 <- inegiR::serie_inegi(sv4, token)
  svd5 <- inegiR::serie_inegi(sv5, token)
  svd6 <- inegiR::serie_inegi(sv6, token)
  svd7 <- inegiR::serie_inegi(sv7, token)
  svd8 <- inegiR::serie_inegi(sv8, token)
  svd9 <- inegiR::serie_inegi(sv9, token)
  svd10 <- inegiR::serie_inegi(sv10, token)
  svd11 <- inegiR::serie_inegi(sv11, token)
  svd12 <- inegiR::serie_inegi(sv12, token)
  svd13 <- inegiR::serie_inegi(sv13, token)
  svd14 <- inegiR::serie_inegi(sv14, token)
  svd15 <- inegiR::serie_inegi(sv15, token)
  svd16 <- inegiR::serie_inegi(sv16, token)
  svd17 <- inegiR::serie_inegi(sv17, token)
  svd18 <- inegiR::serie_inegi(sv18, token)
  svd19 <- inegiR::serie_inegi(sv19, token)
  svd20 <- inegiR::serie_inegi(sv20, token)
  svd21 <- inegiR::serie_inegi(sv21, token)
  svd22 <- inegiR::serie_inegi(sv22, token)
  svd23 <- inegiR::serie_inegi(sv23, token)
  svd24 <- inegiR::serie_inegi(sv24, token)
  svd25 <- inegiR::serie_inegi(sv25, token)
  svd26 <- inegiR::serie_inegi(sv26, token)
  svd27 <- inegiR::serie_inegi(sv27, token)
  svd28 <- inegiR::serie_inegi(sv28, token)
  svd29 <- inegiR::serie_inegi(sv29, token)
  svd30 <- inegiR::serie_inegi(sv30, token)
  svd31 <- inegiR::serie_inegi(sv31, token)
  svd32 <- inegiR::serie_inegi(sv32, token)
  
  
  # crear serie de productividad...
  p1 <- svd1$Valores/spd1$Valores
  p2 <- svd2$Valores/spd2$Valores
  p3 <- svd3$Valores/spd3$Valores
  p4 <- svd4$Valores/spd4$Valores
  p5 <- svd5$Valores/spd5$Valores
  p6 <- svd6$Valores/spd6$Valores
  p7 <- svd7$Valores/spd7$Valores
  p8 <- svd8$Valores/spd8$Valores
  p9 <- svd9$Valores/spd9$Valores
  p10 <- svd10$Valores/spd10$Valores
  p11 <- svd11$Valores/spd11$Valores
  p12 <- svd12$Valores/spd12$Valores
  p13 <- svd13$Valores/spd13$Valores
  p14 <- svd14$Valores/spd14$Valores
  p15 <- svd15$Valores/spd15$Valores
  p16 <- svd16$Valores/spd16$Valores
  p17 <- svd17$Valores/spd17$Valores
  p18 <- svd18$Valores/spd18$Valores
  p19 <- svd19$Valores/spd19$Valores
  p20 <- svd20$Valores/spd20$Valores
  p21 <- svd21$Valores/spd21$Valores
  p22 <- svd22$Valores/spd22$Valores
  p23 <- svd23$Valores/spd23$Valores
  p24 <- svd24$Valores/spd24$Valores
  p25 <- svd25$Valores/spd25$Valores
  p26 <- svd26$Valores/spd26$Valores
  p27 <- svd27$Valores/spd27$Valores
  p28 <- svd28$Valores/spd28$Valores
  p29 <- svd29$Valores/spd29$Valores
  p30 <- svd30$Valores/spd30$Valores
  p31 <- svd31$Valores/spd31$Valores
  p32 <- svd32$Valores/spd32$Valores
  
  # nombrar, para unir... 
  pd1 <- cbind.data.frame("AGS" = p1, 
                          "Fechas" = svd1$Fechas)
  pd2 <- cbind.data.frame("BC" = p2, 
                          "Fechas" = svd2$Fechas)
  pd3 <- cbind.data.frame("BCS" = p3, 
                          "Fechas" = svd3$Fechas)
  pd4 <- cbind.data.frame("CAMP" = p4, 
                          "Fechas" = svd3$Fechas)
  pd5 <- cbind.data.frame("COAH" = p5, 
                          "Fechas" = svd3$Fechas)
  pd6 <- cbind.data.frame("COL" = p6, 
                          "Fechas" = svd3$Fechas)
  pd7 <- cbind.data.frame("CHPS" = p7, 
                          "Fechas" = svd3$Fechas)
  pd8 <- cbind.data.frame("CHIH" = p8, 
                          "Fechas" = svd3$Fechas)
  pd9 <- cbind.data.frame("CDMX" = p9, 
                          "Fechas" = svd3$Fechas)
  pd10 <- cbind.data.frame("DGO" = p10, 
                           "Fechas" = svd3$Fechas)
  pd11 <- cbind.data.frame("GTO" = p11, 
                           "Fechas" = svd3$Fechas)
  pd12 <- cbind.data.frame("GRO" = p12, 
                           "Fechas" = svd3$Fechas)
  pd13 <- cbind.data.frame("HGO" = p13, 
                           "Fechas" = svd3$Fechas)
  pd14 <- cbind.data.frame("JAL" = p14, 
                           "Fechas" = svd3$Fechas)
  pd15 <- cbind.data.frame("MEX" = p15, 
                           "Fechas" = svd3$Fechas)
  pd16 <- cbind.data.frame("MICH" = p16, 
                           "Fechas" = svd3$Fechas)
  pd17 <- cbind.data.frame("MOR" = p17, 
                           "Fechas" = svd3$Fechas)
  pd18 <- cbind.data.frame("NAY" = p18, 
                           "Fechas" = svd3$Fechas)
  pd19 <- cbind.data.frame("NL" = p19, 
                           "Fechas" = svd3$Fechas)
  pd20 <- cbind.data.frame("OAX" = p20, 
                           "Fechas" = svd3$Fechas)
  pd21 <- cbind.data.frame("PUE" = p21, 
                           "Fechas" = svd3$Fechas)
  pd22 <- cbind.data.frame("QRO" = p22, 
                           "Fechas" = svd3$Fechas)
  pd23 <- cbind.data.frame("QROO" = p23, 
                           "Fechas" = svd3$Fechas)
  pd24 <- cbind.data.frame("SLP" = p24, 
                           "Fechas" = svd3$Fechas)
  pd25 <- cbind.data.frame("SIN" = p25, 
                           "Fechas" = svd3$Fechas)
  pd26 <- cbind.data.frame("SON" = p26, 
                           "Fechas" = svd3$Fechas)
  pd27 <- cbind.data.frame("TAB" = p27, 
                           "Fechas" = svd3$Fechas)
  pd28 <- cbind.data.frame("TAM" = p28, 
                           "Fechas" = svd3$Fechas)
  pd29 <- cbind.data.frame("TLAX" = p29, 
                           "Fechas" = svd3$Fechas)
  pd30 <- cbind.data.frame("VER" = p30, 
                           "Fechas" = svd3$Fechas)
  pd31 <- cbind.data.frame("YUC" = p31, 
                           "Fechas" = svd3$Fechas)
  pd32 <- cbind.data.frame("ZAC" = p32, 
                           "Fechas" = svd3$Fechas)
  
  df<-Reduce(function(...) merge(..., all=TRUE), 
             list(pd1, pd2, pd3, pd4, pd5, pd6, pd7, pd8, pd9, pd10,
                  pd11, pd12, pd13, pd14, pd15, pd16, pd17, pd18, pd19, pd20, 
                  pd21, pd22, pd23, pd24, pd25, pd26, pd27, pd28, pd29, pd30, 
                  pd31, pd32
             ))
  return(df)
}
