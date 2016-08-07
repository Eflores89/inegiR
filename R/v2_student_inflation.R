#' Student Price Index Inflation
#'
#' Returns the calculated inflation for students. See \url{http://enelmargen.org/eem/ipe/} for more information. 
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' studentinflation <- inflacion_estudiantes(token)
#' }
#' @name student_inflation
NULL

#' @export
#' @rdname student_inflation
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
