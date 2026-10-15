#' Get INEGI Catalogs
#'
#' Allows you to download the catalogs of frequencies, sources, notes, topics and indicator names. Called in the background in some functions.
#' 
#' @param token INEGI API token
#' @param id Optional id. If NULL, will download entire catalog. 
#' @author Eduardo Flores 
#' @details The five legacy catalog adapters retain English/BIE defaults and
#'   their two-column schemas. incat_topic() retains column order but repairs the
#'   upstream value swap: topic_description contains descriptions and topic contains IDs.
#'   Use \code{inegi_catalog()} for language and bank selection.
#' @seealso \code{\link{inegi_catalog}}
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
  inegi_legacy_catalog("CL_FREQ", token, id, c("frequency", "frequency_description"))
}
#' @export
#' @rdname incat_
incat_source <- function(token, id = NULL){
  inegi_legacy_catalog("CL_SOURCE", token, id, c("source", "source_description"))
}
#' @export
#' @rdname incat_
incat_notes <- function(token, id = NULL){
  inegi_legacy_catalog("CL_NOTE", token, id, c("notes", "notes_description"))
}
#' @export
#' @rdname incat_
incat_topic <- function(token, id = NULL){
  # Retain legacy column order, but map by meaning instead of JSON field order.
  data <- inegi_catalog("CL_TOPIC", id, token, language = "en", database = "BIE")
  data.frame(topic_description = data$description, topic = data$id,
    stringsAsFactors = FALSE)
}
#' @export
#' @rdname incat_
incat_indicator <- function(token, id = NULL){
  inegi_legacy_catalog("CL_INDICATOR", token, id, c("indicator", "indicator_description"))
}

inegi_legacy_catalog <- function(catalog, token, id, columns) {
  data <- inegi_catalog(catalog, id, token, language = "en", database = "BIE")
  result <- data[, c("id", "description"), drop = FALSE]
  names(result) <- columns
  result
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

