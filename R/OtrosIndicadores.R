#' Obtener balanza comercial
#'
#' Obtiene exportaciones, importaciones y balance de los dos en un mismo data.frame por mes.
#' Todos los productos y todos los países 
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#' 
#' @examples
#' ComercioExterior<-Balanza_Comercial(token)
#' @export
#' 
#' 
Balanza_Comercial<-function(token)
{ #balanza comercial as-is (no YoY)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33223/00000/en/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/33226/00000/en/false/xml//"
  
  x_val<-Serie_Inegi(x,token)
    names(x_val)<-c("Exportaciones","Fechas")
  m_val<-Serie_Inegi(m,token)
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
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' ExportacionesMx<-Exportaciones_Pais(token)
#' @note Encoding no permite acéntos en título de descripción
#' @export
#' 
Exportaciones_Pais<-function(token)
{ #exports por pais
  usa<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133172/00000/en/false/xml/"
  can<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133171/00000/en/false/xml/"
  chn<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133285/00000/en/false/xml/"
  cam<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133173/00000/en/false/xml/"
  sur<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/133183/00000/en/false/xml/"
    
  usa_v<-Serie_Inegi(usa,token)
    names(usa_v)<-c("Estados Unidos","Fechas")
  can_v<-Serie_Inegi(can,token)
    names(can_v)<-c("Canadá","Fechas")
  chn_v<-Serie_Inegi(chn,token)
    names(chn_v)<-c("China","Fechas")
  cam_v<-Serie_Inegi(cam,token)
    names(cam_v)<-c("Centro América","Fechas") 
  sur_v<-Serie_Inegi(sur,token)
    names(sur_v)<-c("América del Sur","Fechas") 
  
  df<-Reduce(function(...) merge(...,all=T),list(usa_v,can_v,chn_v,cam_v,sur_v))
  return(df)
}

#' Obtener Produccion de Autos
#'
#' Obtiene producción automotriz en México y cambio porcentual anual.
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' ProduccionAutos<-Autos(token)
#' @note Encoding no permite acentos en título de descripción
#' @export
#'

Autos<-function(token)
{ #Retornar la producción automotriz
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/15166/00000/en/false/xml/"
  
  i<-Serie_Inegi(s,token)
  t<-YoY(serie=i$Valores, lapso=12, decimal=FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas,"Autos"=i$Valores,"YoY"=t)
  
  return(d)
}

#' Obtener Confianza del Consumidor
#'
#' Obtiene Tasas de Cambio de Confianza del Consumidor
#' Devuelve tasas de serie desestacionalizada anual, desestacionalizada contra mes previo y serie original anual.
#' Es un wrapper de las funciones \code{Serie_Inegi()} y \code{YoY()}. 
#' 
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame
#'
#' @examples
#' ConfianzaEconomia<-Tasa_Confianza(token)
#' @export
#'

Tasa_Confianza<-function(token)
{ #Retornar IGAE
  
  #serie original.
  s1<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/63017/00000/en/false/xml/"
  #serie desest.
  s2<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/132944/00000/en/false/xml/"
  
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

#' Obtener Balanza de Pagos
#'
#' Obtiene principales componentes de la Balanza de Pagos: 2 de la Cuenta Corriente, 3 de la Cuenta Financiera y sus 2 resultados.
#'
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores
#' @return Data.frame con 8 columnas
#'
#' @examples
#' BalanzadePagosMexico<-BoP(token)
#' @export
#'

BoP<-function(token)
{ #Retornar la Balanza de Pagos de México
  
  #with_all
  pre<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  last<-"/00000/en/false/xml/"
  
  #Cuenta Corriente
  cc_ing<-Serie_Inegi(paste0(pre,"214053",last),token)
  names(cc_ing)<-c("Cuenta Corriente - Ingresos","Fechas")
  cc_egr<-Serie_Inegi(paste0(pre,"214069",last),token)
  names(cc_egr)<-c("Cuenta Corriente - Egresos","Fechas")
  cc_tot<-Serie_Inegi(paste0(pre,"214052",last),token)
  names(cc_tot)<-c("Cuenta Corriente (Total)","Fechas")
  
  #Cuenta Financiera
  cf_tot<-Serie_Inegi(paste0(pre,"214088",last),token)
  names(cf_tot)<-c("Cuenta Financiera (Total)","Fechas")
  cf_eyo<-Serie_Inegi(paste0(pre,"214113",last),token)
  names(cf_eyo)<-c("Cuenta Financiera - Errores y Omisiones","Fechas")
  cf_res<-Serie_Inegi(paste0(pre,"214114",last),token)
  names(cf_res)<-c("Cuenta Financiera - Cambio en Reservas","Fechas")
  cf_ajv<-Serie_Inegi(paste0(pre,"214115",last),token)
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

  