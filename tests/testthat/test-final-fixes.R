test_that("validated IDs and scalar controls discard harmless names", {
  expect_identical(inegi_validate_id(c(INPC = "216064")), "216064")
  expect_identical(inegi_validate_id(c(first = 2, second = 1), multiple = TRUE), c("2", "1"))
  expect_identical(inegi_validate_token(c(token = "synthetic-token")), "synthetic-token")
  expect_identical(inegi_validate_geography(c(national = "00")), "00")
  expect_identical(inegi_validate_language(c(language = "es")), "es")
  expect_identical(inegi_validate_database(c(bank = "BIE")), "BIE")
  expect_identical(inegi_validate_flag(c(recent = FALSE)), FALSE)
  path <- c(endpoint = "INDICATOR", id = "216064", language = "es", geography = "00",
    recent = "false", bank = "BIE", version = "2.0")
  expect_identical(inegi_validate_path(path),
    c("INDICATOR", "216064", "es", "00", "false", "BIE", "2.0"))
  expect_identical(inegi_request_url(path),
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/216064/es/00/false/BIE/2.0/[REDACTED]?type=json")
})

test_that("named series and query controls produce equivalent indicator results", {
  local_mocked_bindings(inegi_http_perform = function(req) {
    id <- strsplit(req$url, "/", fixed = TRUE)[[1]][10]
    fixture_http_series(canonical_payload(id))
  })
  expect_identical(inegi_series(c(INPC = "216064"), "synthetic-token"),
    inegi_series("216064", "synthetic-token"))
  named <- inegi_series_multiple(c(second = "2", first = "1"), "synthetic-token",
    geography = c(national = "00"), database = c(bank = "BIE"))
  plain <- inegi_series_multiple(c("2", "1"), "synthetic-token")
  expect_identical(named[setdiff(names(named), "meta_calltime")],
    plain[setdiff(names(plain), "meta_calltime")])
  named <- inegi_data(c(second = "2", first = "1"), token = c(token = "synthetic-token"),
    language = c(language = "es"), geography = c(national = "00"), database = c(bank = "BIE"))
  expect_identical(named, inegi_data(c("2", "1"), token = "synthetic-token"))
  expect_identical(inegi_latest(c(INPC = "216064"), "synthetic-token",
    language = c(language = "es"), geography = c(national = "00"), database = c(bank = "BIE")),
    inegi_latest("216064", "synthetic-token"))
})

test_that("named catalog and metadata query controls preserve unnamed output", {
  mock_data_payload(list(CODE = list(list(value = "2", Description = "two"),
    list(value = "1", Description = "one"))))
  expect_identical(inegi_catalog(c(catalog = "CL_INDICATOR"),
    c(second = "2", first = "1"), c(token = "synthetic-token"),
    c(language = "en"), c(bank = "BISE")),
    inegi_catalog("CL_INDICATOR", c("2", "1"), "synthetic-token", "en", "BISE"))
  expect_identical(inegi_metadata(c(second = "2", first = "1"), c(token = "synthetic-token"),
    c(language = "en"), c(bank = "BISE")),
    inegi_metadata(c("2", "1"), "synthetic-token", "en", "BISE"))
})

test_that("literal tokens are absent from complete legacy warning conditions and printing", {
  cases <- list(
    list(frequency = "8", metadata = TRUE,
      message = "Metadata will be passed as-is. Use incat_ functions to download catalogs."),
    list(frequency = "9", metadata = FALSE,
      message = paste0("Biweekly data is NOT coerced or forced to a date. Instead, the date_shortcut ",
        "column is an index of the order of observations (larger N with same date is second half of month).")),
    list(frequency = "10", metadata = FALSE,
      message = "Time format is not supported. Data was downloaded but will be passed on as-is."))
  for (case in cases) {
    p <- canonical_payload(); p[["Series"]][[1]][["FREQ"]] <- case$frequency
    mock_data_payload(p)
    literal_call <- as.call(list(as.name("inegi_series"), "216064",
      "synthetic-literal-warning-token", metadata = case$metadata))
    captured <- list()
    withCallingHandlers(eval(literal_call), warning = function(w) {
      captured[[length(captured) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
    expect_length(captured, 1L)
    w <- captured[[1L]]
    expect_identical(conditionMessage(w), case$message)
    expect_null(conditionCall(w))
    expect_false(grepl("synthetic-literal-warning-token",
      paste(capture.output(dput(unclass(w))), collapse = "\n"), fixed = TRUE))
    expect_false(grepl("synthetic-literal-warning-token",
      paste(capture.output(print(w)), collapse = "\n"), fixed = TRUE))
  }
})
