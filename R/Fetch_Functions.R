#' Obtiene serie de tiempo de INEGI
#'
#' Regresa Data.Frame con la serie de tiempo escogida, al buscar en el webservice del INEGI y parsear via RSDMX y ZOO. 
#' Si parametro Metadata=TRUE, regresa además Región, Unidad, Indicador (# INEGI) y Frecuencia.
#'
#' @param serie Vector en caracter de url de dirección. Este es un metódo directo (se requiere de URL con token)
#' @param metadata Default = FALSE, si TRUE, trae columnas con Región, Unidad, Indicador (# INEGI) y Frecuencia.
#'
#' @return Dataframe
#'
#' @examples
#' #Serie de INPC General 
#' token<-"tokenProporcionadoporWebservice"
#' url <- paste0("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/",token)
#' Serie <- Serie_Unegi(url,metadata = TRUE)
#' @export

Serie_Inegi<-function(serie,metadata=FALSE)
{
  library(plyr,quietly=TRUE)
  library(zoo,quietly=TRUE)
  
  s<-xmlToList(serie)  
  Valores<-as.numeric(ldply(s$Data$Serie,"[[",'CurrentValue')[,'[['])
  Fechas<-ldply(s$Data$Serie,"[[",'TimePeriod')[,'[[']
  Fechas_Date<-as.Date(as.yearmon(Fechas, "%Y/%m"))
  #df
  df<-cbind.data.frame(as.numeric(Valores),Fechas_Date)
  # asegurar nombres y classes
  names(df)<-c("Valores","Fechas")
  class(df[,'Valores'])<-"numeric"
  
  if(metadata){
    Region<-s$MetaData$Region
    Unidad<-s$MetaData$Unit
    Indicador<-s$MetaData$Indicator
    Frecuencia<-s$MetaData$Freq
    df_meta<-cbind.data.frame(Valores,Fechas,Region,Unidad,Indicador,Frecuencia)
    return(df_meta)
  }
  else{
    return(df)
  }
}