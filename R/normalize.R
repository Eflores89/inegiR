# JSON arrays use unnamed lists; records use named lists. Never simplify arrays
# before validating them, because simplification can discard null fields.
inegi_is_array <- function(x) is.list(x) && is.null(names(x))
inegi_is_record <- function(x) is.list(x) && !is.null(names(x)) &&
  !anyDuplicated(names(x))

inegi_response_field <- function(record, field) {
  x <- record[[field]]
  if (is.null(x)) return(NA_character_)
  if (!is.atomic(x) || length(x) != 1L || is.object(x)) {
    inegi_abort("Expected scalar fields in the API response.", "response")
  }
  as.character(x)
}

inegi_empty_data <- function() {
  out <- as.data.frame(stats::setNames(rep(list(character()), 22L),
    c("provider", "series_id", "title", "period", "date", "value", "value_raw",
      "geography", "frequency", "unit", "unit_multiplier", "topic", "source",
      "note", "last_updated", "series_status", "obs_status", "obs_exception",
      "obs_source", "obs_note", "database", "language")), stringsAsFactors = FALSE)
  out$date <- as.Date(character())
  out$value <- double()
  out
}

inegi_normalize <- function(payload, series_id, database, language) {
  if (!inegi_is_record(payload) || !inegi_is_array(payload[["Series"]])) {
    inegi_abort("Expected an indicator series array in the API response.", "response")
  }
  result <- inegi_empty_data()
  for (series in payload[["Series"]]) {
    if (!inegi_is_record(series) || !inegi_is_array(series[["OBSERVATIONS"]])) {
      inegi_abort("Expected a series record and observations array in the API response.", "response")
    }
    id <- inegi_response_field(series, "INDICADOR")
    if (is.na(id) || !identical(id, series_id)) {
      inegi_abort("The API response does not match the requested indicator.", "response")
    }
    # Validate series metadata even for an empty observations array.
    fields <- c(title = "TITLE", frequency = "FREQ", unit = "UNIT",
      unit_multiplier = "UNIT_MULT", topic = "TOPIC", source = "SOURCE", note = "NOTE",
      last_updated = "LASTUPDATE", series_status = "STATUS")
    metadata <- lapply(fields, function(field) inegi_response_field(series, field))
    rows <- lapply(series[["OBSERVATIONS"]], function(obs) {
      if (!inegi_is_record(obs) || !all(c("TIME_PERIOD", "OBS_VALUE") %in% names(obs))) {
        inegi_abort("Observations are missing required fields in the API response.", "response")
      }
      out <- inegi_empty_data()
      out[1L, ] <- NA
      out$provider <- "inegi"; out$series_id <- id
      out$period <- inegi_response_field(obs, "TIME_PERIOD")
      if (is.na(out$period)) {
        inegi_abort("Observations require a nonmissing period field in the API response.", "response")
      }
      out$value_raw <- inegi_response_field(obs, "OBS_VALUE")
      for (name in names(metadata)) out[[name]] <- metadata[[name]]
      observation_fields <- c(geography = "COBER_GEO", obs_status = "OBS_STATUS",
        obs_exception = "OBS_EXCEPTION", obs_source = "OBS_SOURCE", obs_note = "OBS_NOTE")
      for (name in names(observation_fields)) {
        out[[name]] <- inegi_response_field(obs, observation_fields[[name]])
      }
      out$date <- inegi_period(out$period, out$frequency)$date
      number <- suppressWarnings(as.numeric(out$value_raw))
      out$value <- if (is.finite(number)) number else NA_real_
      missing <- is.na(out$value_raw) || trimws(out$value_raw) %in% c("", "NA", "N/A", "NaN")
      exception <- !is.na(out$obs_exception) && nzchar(out$obs_exception)
      if (is.na(out$value) && !missing && !exception) {
        warning("inegiR: Unexpected nonnumeric observation value; raw value retained.", call. = FALSE)
      }
      out$database <- database; out$language <- language
      out
    })
    if (length(rows)) result <- rbind(result, do.call(rbind, rows))
  }
  rownames(result) <- NULL
  result
}
