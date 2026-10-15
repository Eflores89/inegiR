test_that("tokens are scalar safe strings and session changes stay in environment", {
  withr::local_envvar(INEGI_TOKEN = NA)
  expect_error(inegi_get_token(), class = "inegi_input_error")
  for (token in list(NULL, character(), "", "  ", NA_character_, c("a", "b"), 123, "x/y", "x?y", "x\ny")) {
    expect_error(inegi_set_token(token), class = "inegi_input_error")
    expect_error(inegi_validate_token(token), class = "inegi_input_error")
  }
  expect_invisible(inegi_set_token("environment-token"))
  expect_identical(inegi_get_token(), "environment-token")
  expect_identical(Sys.getenv("INEGI_TOKEN"), "environment-token")
})

test_that("ID validation rejects unsafe or inexact IDs and preserves digit strings", {
  expect_identical(inegi_validate_id(c("0001", "216064"), multiple = TRUE), c("0001", "216064"))
  expect_identical(inegi_validate_id(c(1, 216064), multiple = TRUE), c("1", "216064"))
  for (id in list(NULL, character(), NA_character_, "x1", "1/2", "1?x", " 1", "https://x/1", c("1", "2"), 0, -1, 1.5, Inf, NA_real_, 2^53, TRUE)) {
    expect_error(inegi_validate_id(id), class = "inegi_input_error")
  }
})

test_that("geography bank language and flags are strictly validated before HTTP", {
  expect_identical(inegi_validate_geography("00001"), "00001")
  for (geo in list(0, 1, "", "00/01", NA_character_, c("00", "01"))) {
    expect_error(inegi_validate_geography(geo), class = "inegi_input_error")
  }
  for (bank in list("bie", "BISE2", "", NA_character_, c("BIE", "BISE"))) {
    expect_error(inegi_validate_database(bank), class = "inegi_input_error")
  }
  expect_identical(inegi_validate_database("BISE"), "BISE")
  expect_identical(inegi_validate_language("en"), "en")
  for (language in list("ES", "fr", "", NA_character_, c("es", "en"))) {
    expect_error(inegi_validate_language(language), class = "inegi_input_error")
  }
  for (flag in list(1, "true", NULL, NA, c(TRUE, FALSE))) {
    expect_error(inegi_validate_flag(flag, "lastonly"), class = "inegi_input_error")
  }
  expect_identical(inegi_validate_flag(FALSE, "lastonly"), FALSE)
  testthat::local_mocked_bindings(inegi_http_perform = function(...) stop("HTTP must not run"))
  expect_error(inegi_series("1x", "safe-token"), class = "inegi_input_error")
  expect_error(inegi_series("1", "safe-token", geography = 0), class = "inegi_input_error")
  for (flag in c("metadata", "lastonly", "as_tt", "as_compact")) {
    args <- c(list(series_id = "1", token = "safe-token"), setNames(list(NA), flag))
    expect_error(do.call(inegi_series, args), class = "inegi_input_error")
  }
})

test_that("explicit legacy token wins over environment and default uses environment", {
  withr::local_envvar(INEGI_TOKEN = "environment-token")
  seen <- character()
  testthat::local_mocked_bindings(inegi_http_perform = function(req) {
    seen <<- c(seen, req$url)
    fixture_http_series()
  })
  inegi_series("216064", "explicit-token")
  inegi_series("216064")
  expect_match(seen[1], "/explicit-token\\?type=json$", fixed = FALSE)
  expect_match(seen[2], "/environment-token\\?type=json$", fixed = FALSE)
})
