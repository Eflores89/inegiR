#' Obtener balanza comercial de México
#'
#' Obtiene exportaciones, importaciones y balance de los dos en un mismo data.frame por mes.
#' Todos los productos y todos los países 
#'
#' @param token token persona emitido por el INEGI para acceder al API de indicadores.
#' @author Eduardo Flores 
#' @return data.frame con 4 columnas
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
#' Obtener exportaciones por principal país
#'
#' Obtiene exportaciones de principales socios comerciales.
#' Todos los productos y Estados Unidos, Canadá, China, CentroAmerica y América del Sur. 
#'
#' @param token token persona emitido por el INEGI para acceder al API de indicadores.
#' @author Eduardo Flores 
#' @return data.frame con 6 columnas
#'
#' @examples
#' ExportacionesMx<-Exportaciones_Pais(token)
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