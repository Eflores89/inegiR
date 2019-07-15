#' Get INEGI Catalogs
#'
#' Allows you to download the catalogs of frequencies, sources, notes, topics and indicator names. Called in the background in some functions.
#' 
#' @param token INEGI API token
#' @param id Optional id. If NULL, will download entire catalog. 
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' 
#' # Get the corresponding frequency for frequency id #8 (monthly)
#' \dontrun{
#' token <- "webservice token"
#' incat_freq(token, id = "8")
#' }
#' # Get all of the note descriptions
#' \dontrun{
#' token <- "webservice token"
#' incat_notes(token)
#' }
#' #' # Get all of the sources descriptions
#' \dontrun{
#' token <- "webservice token"
#' incat_source(token)
#' }
#' @name incat_
NULL

#' @export
#' @rdname incat_
incat_freq <- function(token, id = NULL){

  if(is.null(id)){
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_FREQ/null/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("frequency", "frequency_description")
    s$CODE
  }else{
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_FREQ/",id,"/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("frequency", "frequency_description")
    s$CODE
  }
}
#' @export
#' @rdname incat_
incat_source <- function(token, id = NULL){
  if(is.null(id)){
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_SOURCE/null/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("source", "source_description")
    s$CODE
  }else{
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_SOURCE/", id, "/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("source", "source_description")
    s$CODE
  }
}
#' @export
#' @rdname incat_
incat_notes <- function(token, id = NULL){
  if(is.null(id)){
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_NOTE/null/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("notes", "notes_description")
    s$CODE
  }else{
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_NOTE/", id, "/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("notes", "notes_description")
    s$CODE
  }
}
#' @export
#' @rdname incat_
incat_topic <- function(token, id = NULL){
  if(is.null(id)){
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_TOPIC/null/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("topic_description", "topic")
    s$CODE
  }else{
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_TOPIC/", id, "/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("topic_description", "topic")
    s$CODE
  }
}
#' @export
#' @rdname incat_
incat_indicator <- function(token, id = NULL){
  if(is.null(id)){
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR/null/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("indicator", "indicator_description")
    s$CODE
  }else{
    u <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR/", id, "/en/BIE/2.0/", token, "?type=json")
    s <- jsonlite::fromJSON(u)
    names(s$CODE) <- c("indicator", "indicator_description")
    s$CODE
  }
}
#' Obtains the catalog of economic data neumonics
#'
#' Neumonics are shorthand names for series of economic data, akin to the Fed FRED names. This catalog downloads the neumonic with the indicator code, to easily match with an API call. This catalog is maintained by INEGI.
#'
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' catalog <- incat_neumonics()
#' }
#' @export
incat_neumonics <- function(){
  u <- "http://www2.inegi.org.mx/servicios/xml/nemonicos.xml"
  s <- XML::xmlToList(u)
  s <- plyr::ldply(s$CodeLists$CodeList, data.frame)
  s <- s[s$.id == "Code", ]
  names(s) <- c("code_type", "text", "neumonic", "series_id", "variable_series_id", "description_en", "lang1", "description_es", "lang2")
  s <- s[,1:9]
  # adding desc
  s$data_type <- ifelse(grepl(pattern = "D$", x = s$neumonic, ignore.case = TRUE), "Desestacionalizada", "Original")
  s
}

