canonical_columns <- c("provider", "series_id", "title", "period", "date", "value",
  "value_raw", "geography", "frequency", "unit", "unit_multiplier", "topic",
  "source", "note", "last_updated", "series_status", "obs_status", "obs_exception",
  "obs_source", "obs_note", "database", "language")

test_that("canonical data preserves typed schema, raw values and provider fields", {
  p <- canonical_payload()
  p$Series[[1]]$UNIT_MULT <- "3"
  p$Series[[1]]$OBSERVATIONS[[1]]$OBS_VALUE <- "00100.250"
  p$Series[[1]]$OBSERVATIONS[[2]]$OBS_VALUE <- "ND"
  p$Series[[1]]$OBSERVATIONS[[2]]$OBS_EXCEPTION <- "9"
  p$Series[[1]]$OBSERVATIONS[[2]]$OBS_NOTE <- NULL
  mock_data_payload(p)
  d <- inegi_data("216064", token = "synthetic-token", geography = "00")
  expect_identical(names(d), canonical_columns)
  expect_identical(d$provider, rep("inegi", 2))
  expect_identical(d$series_id, rep("216064", 2))
  expect_identical(d$title, rep(NA_character_, 2))
  expect_identical(d$period, c("2024/01", "2024/02"))
  expect_identical(d$date, as.Date(c("2024-01-01", "2024-02-01")))
  expect_identical(d$value, c(100.25, NA_real_))
  expect_identical(d$value_raw, c("00100.250", "ND"))
  expect_identical(d$unit_multiplier, c("3", "3"))
  expect_identical(d$geography, c("00", "00"))
  expect_identical(d$obs_exception, c("", "9"))
  expect_identical(d$obs_note, c("", NA_character_))
  expect_true(all(vapply(d[setdiff(names(d), c("date", "value"))], is.character, logical(1))))
  expect_identical(d$database, c("BIE", "BIE"))
  expect_identical(d$language, c("es", "es"))
})

test_that("canonical requests deduplicate IDs and retain API observation and geography order", {
  calls <- character()
  local_mocked_bindings(inegi_http_perform = function(req) {
    id <- strsplit(req$url, "/", fixed = TRUE)[[1]][10]
    calls <<- c(calls, id)
    p <- canonical_payload(id, "01001")
    p$Series[[2]] <- canonical_payload(id, "02001")$Series[[1]]
    p$Series[[1]]$OBSERVATIONS <- rev(p$Series[[1]]$OBSERVATIONS)
    fixture_http_series(p)
  })
  d <- inegi_data(c("2", "1", "2"), token = "synthetic-token")
  expect_identical(calls, c("2", "1"))
  expect_identical(d$series_id, rep(c("2", "1"), each = 4))
  expect_identical(d$geography, rep(c("01001", "01001", "02001", "02001"), 2))
  expect_identical(d$period[1:4], c("2024/02", "2024/01", "2024/01", "2024/02"))
})

test_that("unexpected values warn safely and missing observations are retained", {
  p <- canonical_payload()
  p$Series[[1]]$OBSERVATIONS[[1]]$OBS_VALUE <- "synthetic-token"
  p$Series[[1]]$OBSERVATIONS[[2]]["OBS_VALUE"] <- list(NULL)
  mock_data_payload(p)
  warning <- NULL
  d <- withCallingHandlers(inegi_data("216064", token = "synthetic-token"),
    warning = function(w) { warning <<- w; invokeRestart("muffleWarning") })
  expect_match(conditionMessage(warning), "nonnumeric")
  expect_false(grepl("synthetic-token", conditionMessage(warning), fixed = TRUE))
  expect_identical(d$value, c(NA_real_, NA_real_))
  expect_identical(d$value_raw, c("synthetic-token", NA_character_))
})

test_that("empty arrays produce typed empty data and malformed containers fail", {
  for (p in list(list(Series = list()), within(canonical_payload(),
      Series[[1]]$OBSERVATIONS <- list()))) {
    mock_data_payload(p)
    d <- inegi_data("216064", token = "synthetic-token")
    expect_identical(names(d), canonical_columns)
    expect_identical(nrow(d), 0L)
    expect_s3_class(d$date, "Date")
    expect_type(d$value, "double")
  }
  p <- canonical_payload()
  cases <- list(list(), list(Series = NULL), list(Series = "bad"),
    list(Series = list(list(INDICADOR = "216064"))),
    list(Series = list(list(INDICADOR = "216064", OBSERVATIONS = list(TIME_PERIOD = "2024")))))
  bad <- p; bad$Series[[1]]$OBSERVATIONS[[1]]$TIME_PERIOD <- NULL
  cases <- c(cases, list(bad))
  for (bad in cases) {
    mock_data_payload(bad)
    expect_error(inegi_data("216064", token = "synthetic-token"), class = "inegi_response_error")
  }
  p$Series[[1]]$INDICADOR <- "99"
  mock_data_payload(p)
  expect_error(inegi_data("216064", token = "synthetic-token"), class = "inegi_response_error")
})

test_that("date bounds validate before HTTP and filter interval starts inclusively", {
  mock_data_payload(canonical_payload())
  d <- inegi_data("216064", "2024-02-01", as.Date("2024-02-01"), token = "synthetic-token")
  expect_identical(d$period, "2024/02")
  expect_identical(nrow(inegi_data("216064", "2025-01-01", token = "synthetic-token")), 0L)
  local_mocked_bindings(inegi_http_perform = function(req) stop("network reached"))
  for (bound in list("2024-2-01", "2024-02-30", "2024-01-01junk", "", NA_character_,
                     c("2024-01-01", "2024-02-01"), 1, as.Date(NA))) {
    expect_error(inegi_data("216064", start_date = bound, token = "synthetic-token"),
      class = "inegi_input_error")
  }
  expect_error(inegi_data("216064", "2025-01-01", "2024-01-01", "synthetic-token"),
    class = "inegi_input_error")
})

test_that("ambiguous periods fail bounded calls but remain intact unbounded", {
  p <- canonical_payload(); p$Series[[1]]$FREQ <- "9"
  mock_data_payload(p)
  expect_true(all(is.na(inegi_data("216064", token = "synthetic-token")$date)))
  expect_error(inegi_data("216064", "2024-01-01", token = "synthetic-token"),
    class = "inegi_response_error")
})

test_that("latest requests recent observations while history requests all observations", {
  local_mocked_bindings(inegi_http_perform = function(req) {
    p <- canonical_payload()
    if (grepl("/true/", req$url, fixed = TRUE)) p$Series[[1]]$OBSERVATIONS <- p$Series[[1]]$OBSERVATIONS[2]
    fixture_http_series(p)
  })
  expect_identical(inegi_latest("216064", "synthetic-token")$period, "2024/02")
  expect_identical(nrow(inegi_data("216064", token = "synthetic-token")), 2L)
})

test_that("combined requests abort atomically on any failed indicator", {
  local_mocked_bindings(inegi_http_perform = function(req) {
    if (grepl("/2/", req$url, fixed = TRUE)) return(httr2::response(status_code = 403))
    fixture_http_series(canonical_payload("1"))
  })
  expect_error(inegi_data(c("1", "2"), token = "synthetic-token"), class = "inegi_authentication_error")
})

test_that("optional null metadata and flags become character NA without hidden lookups", {
  p <- canonical_payload()
  for (field in c("FREQ", "UNIT", "UNIT_MULT", "TOPIC", "SOURCE", "NOTE", "LASTUPDATE", "STATUS"))
    p$Series[[1]][field] <- list(NULL)
  p$Series[[1]]$TITLE <- "Synthetic indicator"
  for (i in seq_along(p$Series[[1]]$OBSERVATIONS))
    for (field in c("COBER_GEO", "OBS_STATUS", "OBS_EXCEPTION", "OBS_SOURCE", "OBS_NOTE"))
      p$Series[[1]]$OBSERVATIONS[[i]][field] <- list(NULL)
  mock_data_payload(p)
  d <- inegi_data("216064", token = "synthetic-token", database = "BISE", language = "en")
  expect_identical(d$title, rep("Synthetic indicator", 2))
  expect_true(all(is.na(d[8:20])))
  expect_true(all(is.na(d$date)))
  expect_identical(d$database, rep("BISE", 2))
  expect_identical(d$language, rep("en", 2))
  expect_identical(d$value, c(100.25, 101.5))
})

test_that("required period nulls and malformed scalar fields fail safely", {
  p <- canonical_payload()
  cases <- list(p, p, p, p)
  cases[[1]]$Series[[1]]$OBSERVATIONS[[1]]["TIME_PERIOD"] <- list(NULL)
  cases[[2]]$Series[[1]]$OBSERVATIONS[[1]]$OBS_VALUE <- list("synthetic-token")
  cases[[3]]$Series[[1]]$FREQ <- c("8", "9")
  cases[[4]]$Series <- list(cases[[4]]$Series[[1]], canonical_payload("99")$Series[[1]])
  for (bad in cases) {
    mock_data_payload(bad)
    e <- tryCatch(inegi_data("216064", token = "synthetic-token"), error = identity)
    expect_s3_class(e, "inegi_response_error")
    expect_false(grepl("synthetic-token", conditionMessage(e), fixed = TRUE))
    expect_identical(names(e), c("message", "call"))
  }
})

test_that("canonical JSON containers require exact Series and OBSERVATIONS keys", {
  for (empty in c(FALSE, TRUE)) {
    p <- canonical_payload()
    if (empty) p[["Series"]][[1]][["OBSERVATIONS"]] <- list()
    series_alias <- p
    names(series_alias)[names(series_alias) == "Series"] <- "SeriesTypo"
    observations_alias <- p
    names(observations_alias[["Series"]][[1]])[
      names(observations_alias[["Series"]][[1]]) == "OBSERVATIONS"] <- "OBSERVATIONSTypo"
    for (bad in list(series_alias, observations_alias)) {
      mock_data_payload(bad)
      expect_error(inegi_data("216064", token = "synthetic-token"),
        class = "inegi_response_error")
      expect_error(inegi_latest("216064", token = "synthetic-token"),
        class = "inegi_response_error")
    }
  }
})
