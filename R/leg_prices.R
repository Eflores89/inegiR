#' Obtener tasa de inflacion
#'
#' Obtiene tasa de inflación inter anual en porcentaje.
#' La inflación se define como el cambio porcentual en el INPC. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API de indicadores.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' Inflacion<-inflacion_general(token)
#' }
#' @export
#' 

inflacion_general<-function (token){
  #Serie de INPC general
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
  i<-inegiR::serie_inegi(s, token)
  t<-inegiR::YoY(serie = i$Valores, lapso = 12, decimal = FALSE)
  d<-cbind.data.frame(Fechas=i$Fechas, Valores=t)
  return(d)
}

#' Obtener tasa de inflacion de Estudiantes
#'
#' Obtiene tasa de inflación de estudiantes, inter anual en porcentaje. Es un wrapper de las funciones Serie_Inegi() y YoY(). 
#' La metodología del índice se puede encontrar aquí: \url{http://enelmargen.org/eem/ipe/}
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token persona emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' InflacionEstudiantes<-inflacion_estudiantes(token)
#' }
#' @export
#' 

inflacion_estudiantes<-function (token){
  #Series de INPC;
  s1<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216065/00000/es/false/xml/",token)
  names(s1)<-c("s1","Fechas")
  s2<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216066/00000/es/false/xml/",token)
  names(s2)<-c("s2","Fechas")
  s3<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216067/00000/es/false/xml/",token)
  names(s3)<-c("s3","Fechas")
  s4<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216068/00000/es/false/xml/",token)
  names(s4)<-c("s4","Fechas")
  s5<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216069/00000/es/false/xml/",token)
  names(s5)<-c("s5","Fechas")
  s6<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216070/00000/es/false/xml/",token)
  names(s6)<-c("s6","Fechas")
  s7<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216071/00000/es/false/xml/",token)
  names(s7)<-c("s7","Fechas")
  s8<-inegiR::serie_inegi("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216072/00000/es/false/xml/",token)
  names(s8)<-c("s8","Fechas")
  
  df<-Reduce(function(...) merge(...,all=T), list(s1,s2,s3,s4,s5,s6,s7,s8))
  df$ipe<-(df$s1*0.331417)+(df$s2*0.032764)+(df$s3*0.077735)+(df$s4*0.00378)+(df$s5*0.028353177)+(df$s6*0.199190)+(df$s7*0.0606992)+(df$s8*0.266067)
  
  st<-inegiR::YoY(serie = df$ipe, lapso = 12, decimal = FALSE)
  d<-cbind.data.frame(Fechas=df$Fechas, Valores=st)
  return(d)
}
#' Obtener terminos de intercambio
#'
#' Obtiene la razón de términos de intercambio para México (ToT). Es un wrapper de las funciones serie_inegi() y YoY(). 
#' La razón se define como el índice de precios de exportaciones entre el índice de precios de importaciones. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame 
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' TerminosIntercambio<-inflacion_tot(token)
#' } 
#' @export
#'

inflacion_tot<-function(token)
{ #calcular terminos de intercambio (Terms-Of-Trade)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37502/00000/es/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37503/00000/es/false/xml/"
  
  x_val<-inegiR::serie_inegi(x,token)
  names(x_val)<-c("x","Fechas")
  m_val<-inegiR::serie_inegi(m,token)
  names(m_val)<-c("m","Fechas")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$ToT<-df$x/df$m
  
  d<-cbind.data.frame(Fechas=df$Fechas,Valores=df$ToT)
  return(d)
}

#' Obtener inflacion por Ciudad
#'
#' Obtiene la tasa de inflación mensual por ciudad. 
#' Es un wrapper de las funciones \code{serie_inegi()} y \code{YoY()}. 
#'
#' @param token token personal emitido por el INEGI para acceder al API.
#' @author Eduardo Flores 
#' @return Data.frame 
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' InflacionCiudades<-inflacion_ciudades(token)
#' }
#' @export
#'

inflacion_ciudades<-function(token){
  #Series de INPC;
  SeriesDf<-
    data.frame(
  "Ciudad"=c(
      "DF","Merida","Morelia","Guadalajara","Monterrey",
      "Mexicali","CdJuarez","Acapulco","Culiacan","Leon",
      "Puebla","SanLuisPotosi","Tapachula","Toluca","Torreon",
      "Veracruz","Villahermosa","Tampico","Chihuahua","Hermosillo","Monclova",
      "Cordoba","Ags","Tijuana","Matamoros","Colima","LaPaz","Chetumal",
      "Jacona","Fresnillo","Iguala","Huatabampo","Tulancingo","Cortazar",
      "CdJimenez","Durango","Tepic","Oaxaca","Queretaro","Cuernavaca",
      "Tlaxcala","SanAndres","Campeche","Tepatitlan","Tehuantepec","CdAcuna"), 
  "Data"=c(
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216095/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216096/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216097/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216098/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216099/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216100/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216101/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216102/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216103/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216104/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216105/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216106/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216107/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216108/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216109/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216110/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216111/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216112/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216113/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216114/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216115/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216116/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216117/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216118/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216119/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216120/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216121/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216122/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216123/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216124/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216125/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216126/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216127/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216128/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216129/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216130/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216131/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216132/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216133/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216134/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216135/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216136/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216137/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216138/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216139/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216140/00000/en/false/xml/"
    ), 
  stringsAsFactors = FALSE)
  
  # download
  dloads<-list()
  for(i in 1:46)
  {
    s<-SeriesDf$Data[i]
    dloads[[i]]<-inegiR::serie_inegi(serie = s, 
                                     token)
  }
  
  # names
  names(dloads)<-as.character(SeriesDf$Ciudad)
  for(i in 1:46)
  {
    names(dloads[[i]])<-c(names(dloads[i]),"Fechas")
  }
  
  #join
  df<-Reduce(function(...) merge(..., all=TRUE), dloads)
  
  # year over year
  ts<-apply(df[,2:47],
            2, function(x){
              inegiR::YoY(serie = x, lapso = 12, decimal = FALSE)})
  ts<-as.data.frame(ts)
  # bind
  ts$Fechas<-df$Fechas
  return(ts)
}
