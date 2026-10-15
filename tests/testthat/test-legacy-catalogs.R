test_that("legacy catalog schemas retain positional token and id and English BIE defaults", {
  helpers <- list(incat_freq = c("frequency", "frequency_description"),
    incat_source = c("source", "source_description"),
    incat_notes = c("notes", "notes_description"),
    incat_topic = c("topic_description", "topic"),
    incat_indicator = c("indicator", "indicator_description"))
  endpoints <- c(incat_freq = "CL_FREQ", incat_source = "CL_SOURCE",
    incat_notes = "CL_NOTE", incat_topic = "CL_TOPIC", incat_indicator = "CL_INDICATOR")
  for (helper in names(helpers)) {
    fn <- get(helper)
    expect_identical(names(formals(fn)), c("token", "id"))
    expect_null(formals(fn)$id)
    for (id in list(NULL, c("2", "1"))) {
      url <- NULL
      local_mocked_bindings(inegi_http_perform = function(req) {
        url <<- req$url
        fixture_http_series(list(CODE = list(list(Description = "Producción", value = "2"))))
      })
      d <- fn("synthetic-token", id)
      expect_identical(names(d), helpers[[helper]])
      expect_s3_class(d, "data.frame")
      expect_identical(nrow(d), 1L)
      if (helper == "incat_topic") {
        expect_identical(d$topic, "2")
        expect_identical(d$topic_description, "Producción")
      } else {
        expect_identical(d[[1]], "2")
        expect_identical(d[[2]], "Producción")
      }
      segment <- if (is.null(id)) "null" else "2,1"
      expect_match(url, paste0("/", endpoints[[helper]], "/", segment, "/en/BIE/2.0/"), fixed = TRUE)
    }
  }
})

test_that("topic maps semantic fields regardless of provider JSON field order", {
  for (record in list(list(value = "1000", Description = "Actividad económica"),
                      list(Description = "Actividad económica", Value = "1000"))) {
    local_mocked_bindings(inegi_http_perform = function(req)
      fixture_http_series(list(CODE = list(record))))
    expect_identical(incat_topic("synthetic-token"),
      data.frame(topic_description = "Actividad económica", topic = "1000"))
  }
})

test_that("legacy catalogs share empty, malformed, and authentication handling", {
  for (fn in list(incat_freq, incat_source, incat_notes, incat_topic, incat_indicator)) {
    local_mocked_bindings(inegi_http_perform = function(req) fixture_http_series(list(CODE = list())))
    expect_identical(nrow(fn("synthetic-token")), 0L)
    local_mocked_bindings(inegi_http_perform = function(req) fixture_http_series(list(CODETypo = list())))
    expect_error(fn("synthetic-token"), class = "inegi_response_error")
    local_mocked_bindings(inegi_http_perform = function(req) httr2::response(status_code = 403))
    expect_error(fn("synthetic-token"), class = "inegi_authentication_error")
  }
})
