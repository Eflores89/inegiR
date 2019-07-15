#' Returns INEGI data series
#'
#' Returns a data.frame with the time series chosen from INEGI webservice. If the parameter Metadata is TRUE, a list is returned with two objects: data and metadata. 
#'
#' @param series_id an indicator ID. These are obtained via the INEGI API documentation.
#' @param token API token supplied by INEGI.
#' @param geography Geography code of INEGI. Defaults to 00 (National)
#' @param database Is the id from BIE (Banco de Informacion Economica) or BISE (Banco de Indicadores)? Defaults to BIE. To learn more about what database your indicator is stored in, visit INEGI docs.
#' @param metadata Defaults to FALSE, if TRUE, returns a list with metadata information.
#' @param lastonly Do you want only the last observation? Defaults to FALSE.
#' @param as_tt Do you want the output of the data.frame to be a tibble time object? Defaults to FALSE.
#' @param as_compact Do you want the output always as a data.frame or time tibble? If the output contains metadata, each data point will be replicated in a column. If the output does not contain metadata there is no change. Previously, this was achieved with \code{compact_inegi_series}.
#' 
#' @note Adding the entire INEGI URL as a series is deprecated since v3, due to a change of API specifications in INEGI. INEGI docs can be found at: \url{https://www.inegi.org.mx/servicios/api_indicadores.html}. Coercing biweekly indicators to monthly is also deprecated inside this function. Use tibbletime functions to coerce instead.
#' 
#' @return data.frame or list
#'
#' @author Eduardo Flores 
#' 
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @importFrom tibbletime as_tbl_time
#' @importFrom lubridate month
#' @examples
#' \dontrun{
#' # General INPC series
#' token <- "webservice_token"
#' inpc_id <- "216064"
#' INPC <- inegi_series(inpc_id, token)
#' }
#' @name inegi_series
NULL

#' @export
#' @rdname inegi_series
inegi_series <- function(series_id, token, 
                         geography = "00", database = "BIE", 
                         metadata = FALSE, lastonly = FALSE, as_tt = FALSE, as_compact = FALSE)
  { 
  #### stoping mistakes from previous version breaks
  if(grepl(pattern = "[0-9]", x = series_id, ignore.case = TRUE)){}else{
  stop("Series id must only be numbers. Adding the entire URL is deprecated in v3. See documentation.")}
  
  #### Creating the url we are going to call. 
  u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
              series_id, 
              "/en/", 
              geography, "/", 
              ifelse(lastonly, "true", "false"), 
              "/", database, "/2.0/", 
              token, 
              "?type=json")
  
  #### Download.
  s <- jsonlite::fromJSON(u)
  
  #### If INEGI gives us some error, we will show here:
    if(!is.null(s$ErrorInfo)){
      warning(paste0("INEGI error: ", s$ErrorInfo))
      return(NULL)
    }
  
  #### Creating a data.frame with values
  d <- as.data.frame(s$Series$OBSERVATIONS)
  
  #### Dates will have to be fixed, according to frequency.
  #### These frequency codes are obtained by incat_freq(), they might change if inegi decides to change them!
  if(s$Series$FREQ %in% c("1", "2", "3")){
    # Series is anual, then only take out the first four digits, and this will be our year. 
    d_dates <- gsub(pattern = "/[0-9]{2,5}", replacement = "", x = d$TIME_PERIOD)
    d_aux <- rep(x = "Year", times = length(d$TIME_PERIOD))
  }else{
    if(s$Series$FREQ %in% c("6")){
      # series is trimestral
      d_dates <- gsub(pattern = "/04", replacement = "/10", x = d$TIME_PERIOD) #this has to go first
      d_dates <- gsub(pattern = "/03", replacement = "/07", x = d_dates)
      d_dates <- gsub(pattern = "/02", replacement = "/04", x = d_dates)
      
      d_aux <- ifelse(grepl(pattern = "/10", x = d_dates), "Q4", 
                      ifelse(grepl(pattern = "/07", x = d_dates), "Q3", 
                             ifelse(grepl(pattern = "/04", x = d_dates), "Q2", "Q1")))
      d_dates <- as.Date(zoo::as.yearmon(d_dates, "%Y/%m"))
    }else{
      if(s$Series$FREQ %in% c("8")){
        # series is monthly
        d_dates <- as.Date(zoo::as.yearmon(d$TIME_PERIOD, "%Y/%m"))
        d_aux <- paste0("M", lubridate::month(d_dates))
        
      }else{
        if(s$Series$FREQ %in% c("9")){
          # series is biweekly
          
          d_dates <- as.Date(zoo::as.yearmon(d$TIME_PERIOD, "%Y/%m"))
          d_aux <- 1:length(d$TIME_PERIOD)
          warning("Biweekly data is NOT coerced or forced to a date. Instead, the date_shortcut column is an index of the order of observations (larger N with same date is second half of month).")
        }else{
          if(s$Series$FREQ %in% c("4", "5", "7", "10", "11", "12", "13")){
            warning("Time format is not supported. Only Anual, Trimestral, Monthly or Biweekly indicators are supported by this package. Data was downloaded but will be passed on as-is.")
          }
        }
      }
    }
  }  
  
  #### DATA
  data <- data.frame("date" = d_dates, 
                     "date_shortcut" = d_aux,
                     "values" = as.numeric(d$OBS_VALUE), 
                     "notes" = d$OBS_NOTE)
  
  if(as_tt){
    data <- as_tbl_time(x = data, index = "date")
  }
  
  if(metadata){
    warning("Metadata will be passed as-is. Use incat_ functions to download catalogs.")
    
    metadata <- data.frame(
       "source" = s$Series$SOURCE,
       "topic" = s$Series$TOPIC,
       "notes" = s$Series$NOTE,
        "last_update" = s$Series$LASTUPDATE,
        "region" = geography,
        "units" = s$Series$UNIT,
        "indicator_ID" = s$Series$INDICADOR,
        "frequency" = s$Series$FREQ, 
       "call_local_time" = Sys.time(),
       "call_unmasked" = u
       )
    
    if(as_compact){
      data$meta_source <- metadata$source[1]
      data$meta_topic <- metadata$topic[1]
      data$meta_notes <- metadata$notes[1]
      data$meta_lastupdate <- metadata$last_update[1]
      data$meta_region <- metadata$region[1]
      data$meta_units <- metadata$units[1]
      data$meta_indicatorid <- metadata$indicator_ID[1]
      data$meta_frequency <- metadata$frequency[1]
      data$meta_calltime <- metadata$call_local_time[1]
      data$meta_call <- u
      
      return(data)
    }else{
      l <- list("metadata" = metadata, "data" = data)
      return(l)
    }
   
  }else{
    return(data)
  }
}
#' Returns multiple INEGI data series
#'
#' Returns a data.frame with multiple time series chosen from INEGI webservice. The output will always be a data.frame (not tibble) with compacted metadata. (See \code{inegi_series} to understand as_tt = FALSE and as_compact = TRUE).
#'
#' @param series_id A vector of indicator ID's. These are obtained via the INEGI API documentation.
#' @param token API token supplied by INEGI.
#' @param names Optional vector of names to assign to each id. If NULL, a numerical index is assigned.
#' @param geography Geography code of INEGI. Defaults to 00 (National)
#' @param database Is the id from BIE (Banco de Informacion Economica) or BISE (Banco de Indicadores). Defaults to BIE. To learn more about what database your indicator is stored in, visit INEGI docs.
#' 
#' 
#' @note Adding the entire INEGI URL as a series is deprecated since v3, due to a change of API specifications in INEGI. INEGI docs can be found at: \url{https://www.inegi.org.mx/servicios/api_indicadores.html}. Coercing biweekly indicators to monthly is also deprecated inside this function. Use tibbletime functions to coerce instead.
#' 
#' @return data.frame 
#'
#' @author Eduardo Flores 
#' 
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @importFrom tibbletime as_tbl_time
#' @importFrom lubridate month
#' @examples
#' \dontrun{
#' # General INPC series
#' token <- "webservice_token"
#' some_series <- c("216064", "216097")
#' result <- inegi_series_multiple(some_series, token)
#' }
#' @name inegi_series_multiple
NULL

#' @export
#' @rdname inegi_series_multiple
inegi_series_multiple <- function(series_id, token, names = NULL, geography = "00", database = "BIE"){
  if(is.null(names)){
    index <- 1:length(series_id)
  }else{
    if(length(names) == length(series_id)){
      index <- names
    }else{
      stop("Names and series_id vectors must be same length")
    }
  }
  
  d <- NULL
  for(i in 1:length(series_id)){
    suppressWarnings(
   tmp <- inegiR::inegi_series(series_id = series_id[i], 
                               token = token,
                               geography = geography,
                               database = database, 
                               metadata = TRUE, 
                               lastonly = FALSE, 
                               as_tt = FALSE, 
                               as_compact = TRUE)
    )
    tmp$index_name <- index[i]
    
   d <- rbind.data.frame(tmp, d)
  }
  d
}