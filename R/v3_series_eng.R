#' Returns INEGI data series
#'
#' Returns a data.frame with the time series chosen from INEGI webservice. If the parameter Metadata is TRUE, a list is returned with two objects: data and metadata. 
#'
#' @param series_id an indicator ID. These are obtained via the INEGI API documentation.
#' @param token API token supplied by INEGI; defaults to INEGI_TOKEN.
#' @param geography Geography code of INEGI. Defaults to 00 (National)
#' @param database Is the id from BIE (Banco de Informacion Economica) or BISE (Banco de Indicadores)? Defaults to BIE. To learn more about what database your indicator is stored in, visit INEGI docs.
#' @param metadata Defaults to FALSE, if TRUE, returns a list with metadata information.
#' @param lastonly Do you want only the last observation? Defaults to FALSE.
#' @param as_tt Do you want the output of the data.frame to be a tibble time object? Defaults to FALSE.
#' @param as_compact Do you want the output always as a data.frame or time tibble? If the output contains metadata, each data point will be replicated in a column. If the output does not contain metadata there is no change. Previously, this was achieved with \code{compact_inegi_series}.
#' 
#' @note Adding the entire INEGI URL as a series is deprecated since v3, due to a change of API specifications in INEGI. INEGI docs can be found at: \url{https://www.inegi.org.mx/servicios/api_indicadores.html}. Coercing biweekly indicators to monthly is also deprecated inside this function. Use tibbletime functions to coerce instead.
#' 
#' @details This legacy English-language interface retains date, date_shortcut,
#'   values, and notes columns. Annual dates are character years; quarterly and
#'   monthly dates are Date interval starts. Unsupported periods pass through as
#'   text with a warning. Ambiguous biweekly periods retain the legacy display;
#'   as_tt requires valid unambiguous Date output. Metadata lists contain
#'   metadata then data. call_unmasked and meta_call retain redacted URLs.
#'   For raw periods, values, and flags use \code{inegi_data()}.
#' @seealso \code{\link{inegi_data}}, \code{\link{inegi_metadata}}
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
inegi_series <- function (series_id, token = inegi_get_token(), geography = "00", database = "BIE",
                          metadata = FALSE, lastonly = FALSE, as_tt = FALSE, as_compact = FALSE)
{
  path <- inegi_indicator_path(series_id, "en", geography, database, lastonly)
  inegi_validate_token(token)
  inegi_validate_flag(metadata, "metadata")
  inegi_validate_flag(as_tt, "as_tt")
  inegi_validate_flag(as_compact, "as_compact")
  u <- inegi_request_url(path)
  payload <- inegi_request(path, token)
  normalized <- inegi_normalize(payload, inegi_validate_id(series_id), database, "en")
  if (length(payload[["Series"]]) > 1L) {
    inegi_abort("Expected one indicator series in the legacy API response.", "response")
  }
  series <- if (length(payload[["Series"]])) payload[["Series"]][[1L]] else list()
  d <- data.frame(TIME_PERIOD = normalized$period, OBS_VALUE = normalized$value,
                  OBS_NOTE = normalized$obs_note, stringsAsFactors = FALSE)

  frequency <- inegi_response_field(series, "FREQ")
  periods <- inegi_period(d$TIME_PERIOD, frequency)
  d_dates <- periods$date
  d_aux <- periods$date_shortcut
  if (frequency %in% c("1", "2", "3") && all(!is.na(d_dates))) {
    # Annual legacy output has always been a character year.
    d_dates <- substr(periods$period, 1L, 4L)
  } else if (frequency %in% "9") {
    # Retain the legacy month display and positional shortcut; canonical
    # biweekly dates remain NA because the half-month is ambiguous.
    d_dates <- inegi_period(periods$period, "8")$date
    warning("Biweekly data is NOT coerced or forced to a date. Instead, the date_shortcut column is an index of the order of observations (larger N with same date is second half of month).", call. = FALSE)
  } else if (!(frequency %in% c("1", "2", "3", "6", "8")) || anyNA(d_dates)) {
    d_dates <- periods$period
    d_aux <- periods$period
    warning("Time format is not supported. Data was downloaded but will be passed on as-is.", call. = FALSE)
  }
  data <- data.frame(date = d_dates, date_shortcut = d_aux,
                     values = as.numeric(d$OBS_VALUE), notes = d$OBS_NOTE)
  if (as_tt) {
    if (!inherits(data$date, "Date") || anyNA(data$date) || anyNA(normalized$date)) {
      inegi_abort("as_tt requires valid Date periods; use as_tt = FALSE to retain raw periods.", "input")
    }
    data <- as_tbl_time(x = data, index = "date")
  }
  if (metadata) {
    warning("Metadata will be passed as-is. Use incat_ functions to download catalogs.", call. = FALSE)
    metadata <- data.frame(source = inegi_response_field(series, "SOURCE"),
                           topic = inegi_response_field(series, "TOPIC"),
                           notes = inegi_response_field(series, "NOTE"),
                           last_update = inegi_response_field(series, "LASTUPDATE"),
                           region = geography,
                           units = inegi_response_field(series, "UNIT"),
                           indicator_ID = inegi_response_field(series, "INDICADOR"),
                           frequency = frequency,
                           call_local_time = Sys.time(), call_unmasked = u,
                           stringsAsFactors = FALSE)
    if (as_compact) {
      columns <- c("meta_source", "meta_topic", "meta_notes", "meta_lastupdate",
        "meta_region", "meta_units", "meta_indicatorid", "meta_frequency", "meta_calltime", "meta_call")
      for (i in seq_along(columns)) data[[columns[i]]] <- rep(metadata[[i]][1L], nrow(data))
      return(data)
    }
    else {
      l <- list(metadata = metadata, data = data)
      return(l)
    }
  }
  else {
    return(data)
  }
}
#' Returns multiple INEGI data series
#'
#' Returns a data.frame with multiple time series chosen from INEGI webservice. The output will always be a data.frame (not tibble) with compacted metadata. (See \code{inegi_series} to understand as_tt = FALSE and as_compact = TRUE).
#'
#' @param series_id A vector of indicator ID's. These are obtained via the INEGI API documentation.
#' @param token API token supplied by INEGI; defaults to INEGI_TOKEN.
#' @param names Optional vector of names to assign to each id. If NULL, a numerical index is assigned.
#' @param geography Geography code of INEGI. Defaults to 00 (National)
#' @param database Is the id from BIE (Banco de Informacion Economica) or BISE (Banco de Indicadores). Defaults to BIE. To learn more about what database your indicator is stored in, visit INEGI docs.
#' 
#' 
#' @note Adding the entire INEGI URL as a series is deprecated since v3, due to a change of API specifications in INEGI. INEGI docs can be found at: \url{https://www.inegi.org.mx/servicios/api_indicadores.html}. Coercing biweekly indicators to monthly is also deprecated inside this function. Use tibbletime functions to coerce instead.
#' 
#' @details Rows follow requested series order. Repeated legacy series IDs
#'   retain separate labeled outputs. Canonical \code{inegi_data()} instead
#'   deduplicates IDs and returns the shared provider schema.
#' @seealso \code{\link{inegi_data}}, \code{\link{inegi_series}}
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
inegi_series_multiple <- function(series_id, token = inegi_get_token(), names = NULL, geography = "00", database = "BIE") {
  series_id <- inegi_validate_id(series_id, multiple = TRUE)
  if (!is.null(names) && (length(names) != length(series_id) || !is.atomic(names))) {
    inegi_abort("Names and series_id vectors must have the same length.", "input")
  }
  index <- if (is.null(names)) seq_along(series_id) else names
  rows <- lapply(seq_along(series_id), function(i) {
    tmp <- suppressWarnings(inegi_series(series_id = series_id[i], token = token,
      geography = geography, database = database, metadata = TRUE, as_compact = TRUE))
    tmp$index_name <- rep(index[i], nrow(tmp))
    tmp
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
