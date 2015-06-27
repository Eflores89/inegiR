#' Obtiene serie de tiempo de INEGI
#'
#' Regresa Data.Frame con la serie de tiempo escogida, al buscar en el webservice del INEGI y parsear via RSDMX y ZOO. 
#' Si parametro Metadata=TRUE, regresa además Región, Unidad, Indicador (# INEGI) y Frecuencia.
#' Es una de las funciones primitivas del paquete.
#'
#' @param serie Vector en caracter de url de dirección. Este es un metódo directo (se requiere de URL en formato XML, con token)
#' @param metadata Default = FALSE, si TRUE, trae columnas con Región, Unidad, Indicador (# INEGI) y Frecuencia.
#' 
#' @return Dataframe
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' #Serie de INPC General 
#' token<-"tokenProporcionadoporWebservice"
#' url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#' Serie <- Serie_Inegi(url,token)
#' @export

Serie_Inegi<-function(serie,token,metadata=FALSE)
{
  serie<-paste0(serie,token)
  #### esto falta mejorar 
  #revisar si es una url o un nombre
  #if(grepl(pattern = "http:", x = serie))
   # {} else {
    #  #traer el url del catalogo
     # tryCatch(
      #{#ojo: hacerlo to_lower, para que no importe como esta en catalogo
       # serie<-subset(Catalogo_Series, Nombre == serie)['URL']}, 
        #warning=function(w){print("Warning")},
        #error=function(e){print("Error, no encontró serie en catálogo")}
      #)
        #falta tokenizar catalogo!!
  #  }
  #########################
  ##library(dplyr,quietly=TRUE)
  ##library(zoo,quietly=TRUE)
  
  s<-xmlToList(serie)  
  Fechas<-ldply(.data = s$Data$Serie, .fun = "[[",'TimePeriod')[,'[[']
  
    if(s$MetaData$Freq=="Anual" | s$MetaData$Freq=="Yearly" | s$MetaData$Freq=="Annual" | s$MetaData$Freq=="Quinquenal")
      {Fechas_Date<-as.Date(as.yearmon(x = paste0("01/",Fechas),format = "%m/%Y"))
      } 
  else {
    if(s$MetaData$Freq=="Trimestral" | s$MetaData$Freq=="Quarterly" )
      {
      Fechas<-gsub(pattern = "/04", replacement = "/10", x = Fechas)
      Fechas<-gsub(pattern = "/03", replacement = "/07", x = Fechas)
      Fechas<-gsub(pattern = "/02", replacement = "/04", x = Fechas)
      Fechas_Date<-as.Date(as.yearmon(Fechas, "%Y/%m"))
        } else {Fechas_Date<-as.Date(as.yearmon(Fechas, "%Y/%m"))}
      }
  
  # Values
  Valores<-as.numeric(ldply(s$Data$Serie,"[[",'CurrentValue')[,'[['])
  
  # df
  df<-cbind.data.frame(as.numeric(Valores),Fechas_Date)
  
  # Asegurar nombres y classes
  names(df)<-c("Valores","Fechas")
  class(df[,'Valores'])<-"numeric"
  
  if(metadata){
    MetaData<-list(
          Nombre=s$MetaData$Name,
          UltimaActualizacion=s$MetaData$LastUpdate,
          Region=s$MetaData$Region,
          Unidad=s$MetaData$Unit,
          Indicador=s$MetaData$Indicator,
          Frecuencia=s$MetaData$Freq)
    Obj<-list(MetaData=MetaData,Datos=df)
    
    return(Obj)
    } else{return(df)} 
}