test_that("legacy list and compact metadata keep names and redact provenance", {
  mock_data_payload(canonical_payload())
  expect_warning(x <- inegi_series("216064", "synthetic-token", "00", "BIE", TRUE), "Metadata")
  expect_identical(names(x), c("metadata", "data"))
  expect_identical(names(x$data), c("date", "date_shortcut", "values", "notes"))
  expect_identical(names(x$metadata), c("source", "topic", "notes", "last_update", "region",
    "units", "indicator_ID", "frequency", "call_local_time", "call_unmasked"))
  expect_false(grepl("synthetic-token", x$metadata$call_unmasked, fixed = TRUE))
  expect_warning(y <- inegi_series("216064", "synthetic-token", metadata = TRUE, as_compact = TRUE), "Metadata")
  expect_identical(names(y), c("date", "date_shortcut", "values", "notes", "meta_source",
    "meta_topic", "meta_notes", "meta_lastupdate", "meta_region", "meta_units", "meta_indicatorid",
    "meta_frequency", "meta_calltime", "meta_call"))
  expect_false(any(grepl("synthetic-token", y$meta_call, fixed = TRUE)))
})

test_that("legacy optional null fields and empty observations retain schemas", {
  p <- canonical_payload(); p$Series[[1]]$NOTE <- NULL
  p$Series[[1]]$OBSERVATIONS[[1]]$OBS_NOTE <- NULL
  mock_data_payload(p)
  expect_warning(x <- inegi_series("216064", "synthetic-token", metadata = TRUE), "Metadata")
  expect_identical(x$data$notes, c(NA_character_, ""))
  expect_identical(x$metadata$notes, NA_character_)
  p$Series[[1]]$OBSERVATIONS <- list(); mock_data_payload(p)
  expect_warning(empty <- inegi_series("216064", "synthetic-token", metadata = TRUE, as_compact = TRUE), "Metadata")
  expect_identical(nrow(empty), 0L)
  expect_length(names(empty), 14L)
  expect_s3_class(empty$date, "Date")
})

test_that("legacy multiple retains requested order and validates empty inputs before HTTP", {
  local_mocked_bindings(inegi_http_perform = function(req) {
    id <- strsplit(req$url, "/", fixed = TRUE)[[1]][10]
    fixture_http_series(canonical_payload(id))
  })
  x <- inegi_series_multiple(c("2", "1"), "synthetic-token", c("second", "first"))
  expect_identical(x$meta_indicatorid, c("2", "2", "1", "1"))
  expect_identical(x$index_name, c("second", "second", "first", "first"))
  y <- inegi_series_multiple(c("2", "1"), "synthetic-token")
  expect_identical(y$index_name, c(1L, 1L, 2L, 2L))
  expect_error(inegi_series_multiple(character(), "synthetic-token"), class = "inegi_input_error")
  expect_error(inegi_series_multiple(c("1", "2"), "synthetic-token", "one"), class = "inegi_input_error")
})

test_that("legacy as_tt succeeds on monthly observations", {
  mock_data_payload(canonical_payload())
  expect_s3_class(inegi_series("216064", "synthetic-token", as_tt = TRUE), "tbl_time")
})

test_that("indicator shortcuts use explicit series_id routing and retain indicator identities", {
  withr::local_options(warnPartialMatchArgs = TRUE)
  local_mocked_bindings(inegi_http_perform = function(req) {
    id <- strsplit(req$url, "/", fixed = TRUE)[[1]][10]
    fixture_http_series(canonical_payload(id))
  })
  shortcuts <- list(inind_commerce, inind_auto, inind_gdp, inind_fx, inind_unemp, inind_prices)
  ids <- c("383160", "15166", "381016", "824", "444612", "216064")
  for (i in seq_along(shortcuts)) {
    warnings <- character()
    x <- withCallingHandlers(shortcuts[[i]]("synthetic-token"), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
    expect_identical(x$metadata$indicator_ID, ids[i])
    expect_s3_class(x$data, "tbl_time")
    expect_false(any(grepl("partial argument", warnings)))
  }
})

test_that("ambiguous biweekly observations cannot become a time tibble", {
  p <- canonical_payload(); p$Series[[1]]$FREQ <- "9"
  mock_data_payload(p)
  expect_error(suppressWarnings(inegi_series("216064", "synthetic-token", as_tt = TRUE)),
    "as_tt.*Date|Date.*as_tt", class = "inegi_input_error")
})

test_that("legacy multiple retains the compact schema for empty indicator responses", {
  p <- canonical_payload(); p$Series[[1]]$OBSERVATIONS <- list()
  mock_data_payload(p)
  d <- inegi_series_multiple("216064", "synthetic-token")
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), c("date", "date_shortcut", "values", "notes", "meta_source",
    "meta_topic", "meta_notes", "meta_lastupdate", "meta_region", "meta_units", "meta_indicatorid",
    "meta_frequency", "meta_calltime", "meta_call", "index_name"))
  expect_type(d$values, "double")
  expect_s3_class(d$date, "Date")
})

test_that("legacy JSON containers reject partial matches for required array keys", {
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
      expect_error(inegi_series("216064", "synthetic-token"),
        class = "inegi_response_error")
      expect_error(inegi_series_multiple("216064", "synthetic-token"),
        class = "inegi_response_error")
    }
  }
})
