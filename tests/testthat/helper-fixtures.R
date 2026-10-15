# Synthetic responses only. The network boundary is mocked before legacy calls.
fixture_series <- function(period = c("2024/01", "2024/02"), frequency = "8",
                           value = c("100.25", "101.50")) {
  observations <- data.frame(
    TIME_PERIOD = period, OBS_VALUE = rep_len(value, length(period)),
    OBS_EXCEPTION = rep("", length(period)), OBS_STATUS = rep("", length(period)),
    OBS_SOURCE = rep("1", length(period)), OBS_NOTE = rep("", length(period)),
    COBER_GEO = rep("00", length(period)), stringsAsFactors = FALSE
  )
  series <- data.frame(
    INDICADOR = "216064", FREQ = frequency, TOPIC = "1", UNIT = "1",
    UNIT_MULT = "1", NOTE = "", SOURCE = "1", LASTUPDATE = "2024-03-01",
    STATUS = "", stringsAsFactors = FALSE
  )
  series$OBSERVATIONS <- list(observations)
  list(Header = list(Name = "Synthetic INEGI fixture"), Series = series)
}

legacy_fixture_data <- function(fixture, ...) {
  testthat::local_mocked_bindings(inegi_http_perform = function(req) fixture_http_series(fixture))
  inegi_series("216064", "synthetic-token", ...)
}

fixture_http_series <- function(fixture = fixture_series()) {
  httr2::response(status_code = 200,
    url = "https://www.inegi.org.mx/synthetic-token", body = charToRaw(
      jsonlite::toJSON(fixture, auto_unbox = TRUE, dataframe = "rows", null = "null")))
}

canonical_payload <- function(id = "216064", geography = "00") {
  p <- jsonlite::fromJSON(jsonlite::toJSON(fixture_series(), auto_unbox = TRUE,
    dataframe = "rows"), simplifyVector = FALSE)
  p$Series[[1]]$INDICADOR <- id
  for (i in seq_along(p$Series[[1]]$OBSERVATIONS))
    p$Series[[1]]$OBSERVATIONS[[i]]$COBER_GEO <- geography
  p
}

mock_data_payload <- function(payload, env = parent.frame()) {
  testthat::local_mocked_bindings(inegi_http_perform = function(req)
    fixture_http_series(payload), .env = env)
}

