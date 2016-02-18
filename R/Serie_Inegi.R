#' Obtiene serie de tiempo de INEGI
#'
#' Regresa data.frame con la serie de tiempo escogida, al buscar en el webservice del INEGI y parsear via XML y ZOO. 
#' Si parametro Metadata=TRUE, regresa lista con indicadores meta y datos.
#' Es una de las funciones primitivas del paquete.
#'
#' @param serie Vector en caracter de url de dirección. Este es un metódo directo (se requiere de URL en formato XML, con token)
#' @param token token personal emitido por el INEGI para acceder al API.
#' @param metadata Default = FALSE, si TRUE, parsea una lista con metadatos de serie.
#' @param coercionar Por default (TRUE), los indicadores quincenales serán coercionados a mensuales. Aparecerán todas las observaciones pero en el mismo día del mes a pesar de estar en diferentes quincenas. Para usar días = FALSE.
#' @note La instancia "?callback?", requerida por la documentación del INEGI para series JSON no es necesaria.
#' @return Dataframe o lista
#'
#' @author Eduardo Flores 
#' 
#' @importFrom XML xmlToList
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @examples
#' \dontrun{
#' #Serie de INPC General 
#' token<-"webservice_token"
#' url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#' Serie <- serie_inegi(url, token)
#' }
#' @export

serie_inegi<-function(serie, token, metadata=FALSE, coercionar=TRUE)
{ #detener en error de pegado
  if (grepl(pattern = "xml/$", x = serie) | grepl(pattern = "json/$", x = serie)){}
  else{stop("La serie no termina con xml/ o json/")}
  
  if(grepl(pattern = "json/$", x = serie)){
    #call json
    df<-serie_inegi_json(serie, token, metadata, coercionar)
    return(df)
  }else{
    #parse xml
    
  serie<-paste0(serie, token)
  s<-xmlToList(serie)
    # revisar que no haya errores de origen
    if(!is.null(s$ErrorInfo)){
      warning(paste0("Error de INEGI: ", s$ErrorInfo))
      return(NULL)
    }
  Fechas<-ldply(.data = s$Data$Serie, .fun = "[[",'TimePeriod')[,'[[']
  
  # note to former - zoo::as.yearmon
    if(s$MetaData$Freq == "Anual" | s$MetaData$Freq == "Yearly" | s$MetaData$Freq == "Annual" | s$MetaData$Freq == "Quinquenal" | s$MetaData$Freq == "Decenal" | s$MetaData$Freq == "Bienal")
      {Fechas_Date<-as.Date(zoo::as.yearmon(x = paste0("01/",Fechas), format = "%m/%Y"))
      } 
  else {
    if(s$MetaData$Freq == "Trimestral" | s$MetaData$Freq == "Quarterly" )
      {
      Fechas<-gsub(pattern = "/04", replacement = "/10", x = Fechas)
      Fechas<-gsub(pattern = "/03", replacement = "/07", x = Fechas)
      Fechas<-gsub(pattern = "/02", replacement = "/04", x = Fechas)
      Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
        } else {
          if(s$MetaData$Freq == "Quincenal")
            {if(coercionar){
              Mensaje <- "Importante: El indicador quincenal fue coercionado a meses - para evitar, correr con opcion coercionar=FALSE"
              Fechas_Date <- as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
                            } else {
                              FechasN <- gsub(pattern="/02$", replacement = "/15", x = Fechas)
                              Fechas_Date <- as.Date(x = FechasN, format = "%Y/%m/%d")}
            } else {
              Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
            }
        }}
  # Values
  Valores<-as.numeric(ldply(s$Data$Serie,"[[",'CurrentValue')[,'[['])
  
  # df
  df<-cbind.data.frame(as.numeric(Valores), Fechas_Date)
  
  # Asegurar nombres y classes
  names(df)<-c("Valores", "Fechas")
  class(df[,'Valores']) <- "numeric"
  
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
    }} 
}
#' Obtiene serie de tiempo de INEGI en formato JSON
#'
#' Regresa data.frame con la serie de tiempo escogida, al buscar en el webservice del INEGI y parsear via Jsonlite. 
#' Si parametro Metadata=TRUE, regresa lista con indicadores meta y datos.
#' 
#' @details Esta función se llama directamente en \code{serie_inegi()}, cuando el parametro "serie" termina en "json/". 
#' @note La instancia "?callback?" requerida por la documentación del INEGI no es necesaria.
#'
#' @param serie Vector en caracter de url de dirección. Este es un metódo directo (se requiere de URL en formato XML, con token)
#' @param token token personal emitido por el INEGI para acceder al API.
#' @param metadata Default = FALSE, si TRUE, parsea una lista con metadatos de serie.
#' @param coercionar Por default (TRUE), los indicadores quincenales serán coercionados a mensuales. Aparecerán todas las observaciones pero en el mismo día del mes a pesar de estar en diferentes quincenas. Para usar días = FALSE.
#' 
#' @return Dataframe o lista
#'
#' @author Eduardo Flores 
#' @seealso serie_inegi
#' @importFrom jsonlite fromJSON
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @examples
#' \dontrun{
#' #Serie de INPC General 
#' token<-"webservice_token"
#' url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#' Serie <- serie_inegi(url,token)
#' }
#' @export

serie_inegi_json<-function(serie, token, metadata=FALSE, coercionar=TRUE)
{
  serie <- paste0(serie, token, "?callback?")
  
  s<-jsonlite::fromJSON(serie)
  
  # revisar que no haya errores de origen
  if(!is.null(s$ErrorInfo)){
    warning(paste0("Error de INEGI: ", s$ErrorInfo))
    return(NULL)
  }
  
  #valores 
  Valores <- as.numeric(as.data.frame(s$Data$Serie)[,c("CurrentValue")])
  
  #fechas 
  Fechas <- (s$Data$Serie)[,c("TimePeriod")]
  if(s$MetaData$Freq == "Anual" | s$MetaData$Freq == "Yearly" | s$MetaData$Freq == "Annual" | s$MetaData$Freq == "Quinquenal" | s$MetaData$Freq == "Decenal" | s$MetaData$Freq == "Bienal")
  {Fechas_Date<-as.Date(zoo::as.yearmon(x = paste0("01/",Fechas), format = "%m/%Y"))
  } else {
    if(s$MetaData$Freq == "Trimestral" | s$MetaData$Freq == "Quarterly" )
    {
      Fechas<-gsub(pattern = "/04", replacement = "/10", x = Fechas)
      Fechas<-gsub(pattern = "/03", replacement = "/07", x = Fechas)
      Fechas<-gsub(pattern = "/02", replacement = "/04", x = Fechas)
      Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
    } else {
      if(s$MetaData$Freq=="Quincenal")
      {if(coercionar){
        Mensaje<-"Importante: El indicador quincenal fue coercionado a meses - para evitar, correr con opcion coercionar=FALSE"
        Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
      } else {
        FechasN<-gsub(pattern="/02$",replacement="/15",x = Fechas)
        Fechas_Date<-as.Date(x = FechasN,format = "%Y/%m/%d")}
      } else {
        Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
      }
    }}
  
  df <- data.frame("Valores" = Valores,
                   "Fechas" = Fechas_Date)
  
  if(metadata){
    MetaData<-list(
      Nombre=s$MetaData$Name,
      UltimaActualizacion=s$MetaData$LastUpdate,
      Region=s$MetaData$Region,
      Unidad=s$MetaData$Unit,
      Indicador=s$MetaData$Indicator,
      Frecuencia=s$MetaData$Freq)
    Obj<-list(MetaData=MetaData,
              Datos=df)
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
