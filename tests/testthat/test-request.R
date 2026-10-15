request_path <- c("INDICATOR", "216064", "es", "00", "false", "BIE", "2.0")
request_secret <- "do-not-leak-this-token"

expect_safe_condition <- function(expr, category) {
  err <- tryCatch(force(expr), error = identity)
  expect_s3_class(err, paste0("inegi_", category, "_error"))
  expect_s3_class(err, "inegi_error")
  expect_null(err$call)
  expect_null(err$parent)
  expect_false(grepl(request_secret, paste(capture.output(str(err)), collapse = ""), fixed = TRUE))
  expect_false(grepl(request_secret, conditionMessage(err), fixed = TRUE))
  invisible(err)
}

fixture_http <- function(status = 200L, body = '{"Series":[]}', headers = list()) {
  httr2::response(status_code = status,
    url = paste0("https://www.inegi.org.mx/", request_secret),
    headers = headers, body = charToRaw(body))
}

test_that("requests use fixed origin escaped token path timeout and no redirects", {
  req <- inegi_build_request(request_path, "safe-token")
  expect_identical(req$url, paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/",
                                  "INDICATOR/216064/es/00/false/BIE/2.0/safe-token?type=json"))
  expect_equal(req$options$timeout_ms, 30000)
  expect_false(req$options$followlocation)
  expect_match(inegi_request_url(request_path), "/\\[REDACTED\\]\\?type=json$")
  for (path in list("https://bad.example/x", c("INDICATOR", "../x", "2.0"), c("INDICATOR", "x?y", "2.0"))) {
    expect_error(inegi_request(path, "safe-token"), class = "inegi_input_error")
  }
})

test_that("transport returns unsimplified JSON and constructs indicator paths", {
  expect_identical(inegi_indicator_path(216064), request_path)
  expect_identical(inegi_indicator_path("216064", "en", "01", "BISE", TRUE),
                   c("INDICATOR", "216064", "en", "01", "true", "BISE", "2.0"))
  testthat::local_mocked_bindings(inegi_http_perform = function(req) fixture_http(body = '{"Series":[{"INDICADOR":"216064","OBSERVATIONS":[{"OBS_VALUE":"1"}]}]}'))
  result <- inegi_request(request_path, request_secret)
  expect_type(result$Series, "list")
  expect_identical(result$Series[[1]]$OBSERVATIONS[[1]]$OBS_VALUE, "1")
})

test_that("authentication and permanent HTTP errors never retry or expose responses", {
  for (status in c(401L, 403L, 400L, 404L, 302L, 501L, 505L)) {
    attempts <- 0L
    testthat::local_mocked_bindings(inegi_http_perform = function(req) {
      attempts <<- attempts + 1L
      fixture_http(status, paste0('{"error":"', request_secret, '"}'), list(location = paste0("https://evil/", request_secret)))
    })
    expect_safe_condition(inegi_request(request_path, request_secret), if (status %in% c(401L, 403L)) "authentication" else "http")
    expect_equal(attempts, 1L)
  }
})

test_that("only transient statuses retry up to three attempts with bounded delays", {
  for (status in c(429L, 500L, 502L, 503L, 504L)) {
    attempts <- 0L
    delays <- numeric()
    testthat::local_mocked_bindings(
      inegi_http_perform = function(req) {
        attempts <<- attempts + 1L
        fixture_http(status, request_secret, list(`retry-after` = "999999"))
      },
      inegi_retry_sleep = function(seconds) delays <<- c(delays, seconds)
    )
    expect_safe_condition(inegi_request(request_path, request_secret), "http")
    expect_equal(attempts, 3L)
    expect_length(delays, 2L)
    expect_true(all(is.finite(delays) & delays >= 0 & delays <= 5))
  }
})

test_that("a later successful response is parsed after transient HTTP failures", {
  attempts <- 0L
  testthat::local_mocked_bindings(
    inegi_http_perform = function(req) {
      attempts <<- attempts + 1L
      if (attempts < 3L) fixture_http(503L) else fixture_http(body = '{"CODE":[]}')
    }, inegi_retry_sleep = function(seconds) NULL
  )
  expect_identical(inegi_request(request_path, request_secret), list(CODE = list()))
  expect_equal(attempts, 3L)
})

test_that("transport errors suppress unsafe conditions and do not retry", {
  attempts <- 0L
  testthat::local_mocked_bindings(inegi_http_perform = function(req) {
    attempts <<- attempts + 1L
    warning(request_secret)
    stop(structure(list(message = request_secret, call = req$url, response = req), class = c("unsafe_error", "error", "condition")))
  })
  expect_silent(expect_safe_condition(inegi_request(request_path, request_secret), "connection"))
  expect_equal(attempts, 1L)
})

test_that("malformed JSON and ErrorInfo payloads yield safe response errors", {
  for (body in c(paste0("{", request_secret), paste0('{"ErrorInfo":"', request_secret, '"}'),
                 paste0('{"ErrorInfo":{"Code":"', request_secret, '"}}'), 'null', '[]', '1')) {
    testthat::local_mocked_bindings(inegi_http_perform = function(req) fixture_http(body = body))
    expect_safe_condition(inegi_request(request_path, request_secret), "response")
  }
})

test_that("legacy metadata and compact output retain fields with redacted URLs", {
  testthat::local_mocked_bindings(inegi_http_perform = function(req) fixture_http_series())
  expect_warning(result <- inegi_series("216064", request_secret, metadata = TRUE), "Metadata")
  expect_named(result, c("metadata", "data"))
  expect_match(result$metadata$call_unmasked, "/\\[REDACTED\\]\\?type=json$")
  expect_false(grepl(request_secret, paste(capture.output(str(result)), collapse = ""), fixed = TRUE))
  expect_warning(compact <- inegi_series("216064", request_secret, metadata = TRUE, as_compact = TRUE), "Metadata")
  expect_match(compact$meta_call[1], "/\\[REDACTED\\]\\?type=json$")
  expect_false(grepl(request_secret, paste(capture.output(str(compact)), collapse = ""), fixed = TRUE))
})
