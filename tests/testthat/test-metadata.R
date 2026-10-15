catalog_fixture <- function(ids = c("2", "1"), descriptions = c("Producción y café", "Índice"),
                            value_field = "value") {
  list(CODE = lapply(seq_along(ids), function(i) {
    record <- list(value = ids[[i]], Description = descriptions[[i]])
    names(record)[1] <- value_field
    record
  }))
}

mock_catalog <- function(payload, env = parent.frame()) {
  testthat::local_mocked_bindings(inegi_http_perform = function(req)
    fixture_http_series(payload), .env = env)
}

catalog_names <- c("CL_INDICATOR", "CL_FREQ", "CL_TOPIC", "CL_UNIT",
                   "CL_UNIT_MULT", "CL_NOTE", "CL_SOURCE", "CL_GEO_AREA")

test_that("catalogs support every documented endpoint and preserve UTF-8 text", {
  for (catalog in catalog_names) {
    url <- NULL
    local_mocked_bindings(inegi_http_perform = function(req) {
      url <<- req$url
      fixture_http_series(catalog_fixture())
    })
    d <- inegi_catalog(catalog, token = "synthetic-token")
    expect_identical(names(d), c("catalog", "id", "description", "database", "language"))
    expect_identical(d$id, c("2", "1"))
    expect_identical(d$description, c("Producción y café", "Índice"))
    expect_identical(d$catalog, rep(catalog, 2))
    expect_identical(d$database, rep("BIE", 2))
    expect_identical(d$language, rep("es", 2))
    expect_true(all(vapply(d, is.character, logical(1))))
    expect_match(url, paste0("/", catalog, "/null/es/BIE/2.0/"), fixed = TRUE)
  }
})

test_that("catalog IDs join validated vectors and handle Value case exactly", {
  url <- NULL
  local_mocked_bindings(inegi_http_perform = function(req) {
    url <<- req$url
    fixture_http_series(catalog_fixture(value_field = "Value"))
  })
  d <- inegi_catalog("CL_INDICATOR", c(2, 1), "synthetic-token", "en", "BISE")
  expect_identical(d$id, c("2", "1"))
  expect_identical(d$database, rep("BISE", 2))
  expect_identical(d$language, rep("en", 2))
  expect_match(url, "/CL_INDICATOR/2,1/en/BISE/2.0/", fixed = TRUE)
  inegi_catalog("CL_GEO_AREA", "001", "synthetic-token")
  expect_match(url, "/CL_GEO_AREA/001/es/BIE/2.0/", fixed = TRUE)
})

test_that("empty catalogs have a typed zero-row schema", {
  mock_catalog(list(CODE = list()))
  d <- inegi_catalog("CL_FREQ", token = "synthetic-token")
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), c("catalog", "id", "description", "database", "language"))
  expect_true(all(vapply(d, is.character, logical(1))))
})

test_that("catalogs reject malformed containers and records with safe conditions", {
  cases <- list(list(), list(CODE = NULL), list(CODETypo = list()),
    list(CODE = list(value = "1", Description = "x")), list(CODE = list(NULL)),
    list(CODE = list(list(value = "1"))),
    list(CODE = list(list(valueTypo = "1", Description = "x"))),
    list(CODE = list(list(value = "1", DescriptionTypo = "x"))),
    list(CODE = list(list(value = "1", Description = NULL))),
    list(CODE = list(list(value = "1", Description = list("synthetic-token")))),
    list(CODE = list(list(value = c("1", "2"), Description = "x"))),
    list(CODE = list(list(value = "1", Value = "2", Description = "x"))),
    list(CODE = list(list(value = "../1", Description = "x"))))
  for (payload in cases) {
    mock_catalog(payload)
    e <- tryCatch(inegi_catalog("CL_FREQ", token = "synthetic-token"), error = identity)
    expect_s3_class(e, "inegi_response_error")
    expect_identical(names(e), c("message", "call"))
    expect_false(grepl("synthetic-token", conditionMessage(e), fixed = TRUE))
  }
})

test_that("catalog inputs reject paths and invalid controls before networking", {
  local_mocked_bindings(inegi_http_perform = function(req) stop("network reached"))
  for (catalog in list("CL_OTHER", "CL_FREQ/other", "cl_freq", NA_character_, c("CL_FREQ", "CL_NOTE")))
    expect_error(inegi_catalog(catalog, token = "synthetic-token"), class = "inegi_input_error")
  for (id in list(character(), NA_character_, "1,2", "1/2", "1?x=2", c("1", "../2"), -1, 1.1))
    expect_error(inegi_catalog("CL_FREQ", id, "synthetic-token"), class = "inegi_input_error")
  expect_error(inegi_catalog("CL_FREQ", token = "synthetic-token", language = "fr"), class = "inegi_input_error")
  expect_error(inegi_catalog("CL_FREQ", token = "synthetic-token", database = "OTHER"), class = "inegi_input_error")
})

test_that("path validation allows only numeric comma lists without injection", {
  expect_identical(inegi_validate_path(c("CL_INDICATOR", "2,1", "es", "BIE", "2.0")),
    c("CL_INDICATOR", "2,1", "es", "BIE", "2.0"))
  for (id in c("1,", ",1", "1,,2", "a,b", "1,2/other", "1,2?x=3", "1,2#x", "1,2%2F"))
    expect_error(inegi_validate_path(c("CL_INDICATOR", id, "es", "BIE", "2.0")), class = "inegi_input_error")
})

test_that("metadata provides ordered common core and retains raw indicator fields", {
  payload <- catalog_fixture()
  payload$CODE[[1]]$FREQ <- "8"
  payload$CODE[[1]]$extra <- list(code = "001", labels = list("uno", "dos"))
  mock_catalog(payload)
  d <- inegi_metadata(c("1", "2", "1"), "synthetic-token", "en", "BISE")
  expect_identical(names(d), c("provider", "series_id", "title", "database", "language", "raw_metadata"))
  expect_identical(d$provider, rep("inegi", 2))
  expect_identical(d$series_id, c("1", "2"))
  expect_identical(d$title, c("Índice", "Producción y café"))
  expect_identical(d$database, rep("BISE", 2))
  expect_identical(d$language, rep("en", 2))
  expect_identical(d$raw_metadata[[2]], payload$CODE[[1]])
  expect_false(grepl("synthetic-token", paste(capture.output(str(d)), collapse = ""), fixed = TRUE))
})

test_that("metadata rejects missing, duplicated, and unrequested response IDs atomically", {
  for (ids in list("1", character(), c("1", "1", "2"), c("1", "2", "3"), c("1", "3"))) {
    mock_catalog(catalog_fixture(ids, rep("title", length(ids))))
    expect_error(inegi_metadata(c("1", "2"), "synthetic-token"), class = "inegi_response_error")
  }
  mock_catalog(catalog_fixture(value_field = "Value"))
  expect_identical(inegi_metadata(c(2, 1), "synthetic-token")$series_id, c("2", "1"))
})

test_that("canonical catalog and metadata use the session token default", {
  withr::local_envvar(INEGI_TOKEN = "synthetic-token")
  mock_catalog(catalog_fixture())
  expect_identical(inegi_catalog("CL_FREQ")$id, c("2", "1"))
  expect_identical(inegi_metadata(c("1", "2"))$series_id, c("1", "2"))
})
