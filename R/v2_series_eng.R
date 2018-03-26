#' Returns INEGI data series
#'
#' Returns a data.frame with the time series chosen from INEGI webservice. If the parameter Metadata is TRUE, a list
#' is returned with two objects: data and metadata. 
#'
#' @param series Vector with complete url in character, obtained via INEGI
#' @param token API token supplied by INEGI
#' @param metadata Defaults to FALSE, if TRUE, returns a list with metadata information.
#' @param coerce Defaults to TRUE. The bi-weekly indicators will be coerced to monthly. All observations will be kept but in the same day of the month.
#' @note if using JSON url, the "?callback?" at the end of the string is not needed.
#' @return data.frame or list
#'
#' @author Eduardo Flores 
#' 
#' @importFrom XML xmlToList
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @examples
#' \dontrun{
#' # General INPC series
#' token<-"webservice_token"
#' url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#' INPC <- inegi_series(url, token)
#' }
#' @name inegi_series
NULL

#' @export
#' @rdname inegi_series
inegi_series <- function(series, token, metadata=FALSE, coerce=TRUE)
{ #detener en error de pegado
  if (grepl(pattern = "xml/$", x = series) | grepl(pattern = "json/$", x = series)){}
  else{stop("Series URL does not end with xml/ or json/")}
  
  if(grepl(pattern = "json/$", x = series)){
    #call json
    df <- inegi_series_json(series, token, metadata, coerce)
    return(df)
  }else{
    #parse xml
    
    serie <- paste0(series, token)
    s <- XML::xmlToList(serie)
    # revisar que no haya errores de origen
    if(!is.null(s$ErrorInfo)){
      warning(paste0("INEGI error: ", s$ErrorInfo))
      return(NULL)
    }
    Fechas <- plyr::ldply(.data = s$Data$Serie, .fun = "[[",'TimePeriod')[,'[[']
    
    # note to former - zoo::as.yearmon
    if(s$MetaData$Freq == "Anual" | s$MetaData$Freq == "Yearly" | s$MetaData$Freq == "Annual" | s$MetaData$Freq == "Quinquenal" | s$MetaData$Freq == "Decenal" | s$MetaData$Freq == "Bienal")
    { Fechas_Date <- zoo::as.Date(zoo::as.yearmon(x = paste0("01/",Fechas), format = "%m/%Y"))
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
        {if(coerce){
          Mensaje <- "Important: Bi weekly indicator was coerced to monthly. Use coerce = FALSE to avoid."
          Fechas_Date <- as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
        } else {
          FechasN <- gsub(pattern="/02$", replacement = "/15", x = Fechas)
          Fechas_Date <- as.Date(x = FechasN, format = "%Y/%m/%d")}
        } else {
          Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
        }
      }}
    # Values
    Valores <- as.numeric(plyr::ldply(s$Data$Serie,"[[",'CurrentValue')[,'[['])
    
    # check length of vectors, it is impossible to determine the skipped data
    if(!(length(Valores)==length(Fechas))){
      stop("Data error: the number of observations and dates does not match. 
           The most probable culprit is a null observation anywhere in the data. 
           Contact INEGI for more information.")
      
    }
    
    # df
    df <- cbind.data.frame(as.numeric(Valores), Fechas_Date)
    
    # Asegurar nombres y classes
    names(df)<-c("Values", "Dates")
    class(df[,'Values']) <- "numeric"
    
    if(metadata){
      MetaData <- list(
        Name = s$MetaData$Name,
        LastUpdate = s$MetaData$LastUpdate,
        Region = s$MetaData$Region,
        Units = s$MetaData$Unit,
        Indicators = s$MetaData$Indicator,
        Frequency = s$MetaData$Freq)
      Obj <- list(MetaData = MetaData, Data = df)
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

#' @export
#' @rdname inegi_series
inegi_series_json<-function(series, token, metadata=FALSE, coerce=TRUE)
{
  serie <- paste0(series, token, "?callback?")
  
  s <- jsonlite::fromJSON(serie)
  
  # revisar que no haya errores de origen
  if(!is.null(s$ErrorInfo)){
    warning(paste0("INEGI error: ", s$ErrorInfo))
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
      {if(coerce){
        Mensaje <- "Important: Bi weekly indicator was coerced to monthly. Use coerce = FALSE to avoid."
        Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
      } else {
        FechasN<-gsub(pattern="/02$",replacement="/15",x = Fechas)
        Fechas_Date<-as.Date(x = FechasN,format = "%Y/%m/%d")}
      } else {
        Fechas_Date<-as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
      }
    }}
  
  # check length of vectors, it is impossible to determine the skipped data
  if(!(length(Valores)==length(Fechas))){
    stop("Data error: the number of observations and dates does not match. 
         The most probable culprit is a null observation anywhere in the data. 
         Contact INEGI for more information.")
    
  }
  
  df <- data.frame("Values" = Valores,
                   "Dates" = Fechas_Date)
  if(metadata){
    MetaData<-list(
      Name = s$MetaData$Name,
      LastUpdate = s$MetaData$LastUpdate,
      Region = s$MetaData$Region,
      Units = s$MetaData$Unit,
      Indicators = s$MetaData$Indicator,
      Frequency = s$MetaData$Freq)
    Obj<-list(MetaData = MetaData,
              Data = df)
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