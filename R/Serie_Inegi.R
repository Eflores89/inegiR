#' Obtiene serie de tiempo de INEGI
#'
#' Regresa Data.Frame con la serie de tiempo escogida, al buscar en el webservice del INEGI y parsear via XML y ZOO. 
#' Si parametro Metadata=TRUE, regresa lista con indicadores meta y datos.
#' Es una de las funciones primitivas del paquete.
#'
#' @param serie Vector en caracter de url de dirección. Este es un metódo directo (se requiere de URL en formato XML, con token)
#' @param token token personal emitido por el INEGI para acceder al API.
#' @param metadata Default = FALSE, si TRUE, parsea una lista con metadatos de serie.
#' @param coercionar Por default (TRUE), los indicadores quincenales serán coercionados a mensuales. Aparecerán todas las observaciones pero en el mismo día del mes a pesar de estar en diferentes quincenas. Para usar días = FALSE.
#' 
#' @return Dataframe o lista
#'
#' @author Eduardo Flores 
#' 
#' @importFrom XML xmlToList
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @examples
#' #Serie de INPC General 
#' token<-"tokenProporcionadoporWebservice"
#' url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#' Serie <- Serie_Inegi(url,token)
#' @export

Serie_Inegi<-function(serie,token,metadata=FALSE,coercionar=TRUE)
{
  serie<-paste0(serie,token)
  
  s<-xmlToList(serie)  
  Fechas<-ldply(.data = s$Data$Serie, .fun = "[[",'TimePeriod')[,'[[']
  
  # note to former - zoo::as.yearmon
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
        } else {
          if(s$MetaData$Freq=="Quincenal")
            {if(coercionar){
              Mensaje<-"Importante: El indicador quincenal fue coercionado a meses - para evitar, correr con opción coercionar=FALSE"
              Fechas_Date<-as.Date(as.yearmon(Fechas, "%Y/%m"))
                            } else {
                              FechasN<-gsub(pattern="/02$",replacement="/15",x = Fechas)
                              Fechas_Date<-as.Date(x = FechasN,format = "%Y/%m/%d")}
            } else {
              Fechas_Date<-as.Date(as.yearmon(Fechas, "%Y/%m"))
            }
        }}
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
    #warn-on-quincenal
    if(exists("Mensaje"))
      { warning(print(Mensaje))
      }
    return(Obj)
    } else{
      #warn-on-quincenal
      if(exists("Mensaje"))
      { warning(print(Mensaje))
      }
      return(df)
    } 
}