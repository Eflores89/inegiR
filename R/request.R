# A path is a character vector of token-free API segments. No URL, slash,
# query or credential is accepted. Endpoint-specific builders validate meaning.
inegi_validate_path <- function(path) {
  path <- unname(path)
  if (!is.character(path) || length(path) < 2L || anyNA(path) ||
      any(!(grepl("^[A-Za-z0-9_.-]+$", path) |
            grepl("^[0-9]+(,[0-9]+)+$", path))) ||
      any(path %in% c(".", "..")) ||
      !identical(utils::tail(path, 1L), "2.0")) {
    inegi_abort("Use token-free API path segments ending in version 2.0.", "input")
  }
  path
}

inegi_indicator_path <- function(series_id, language = "es", geography = "00",
                                 database = "BIE", lastonly = FALSE) {
  c("INDICATOR", inegi_validate_id(series_id),
    inegi_validate_language(language), inegi_validate_geography(geography),
    if (inegi_validate_flag(lastonly, "lastonly")) "true" else "false",
    inegi_validate_database(database), "2.0")
}

# Omitting token creates a safe provenance URL; callers must never return a
# build_request object or a token-bearing URL to users.
inegi_request_url <- function(path, token = NULL) {
  path <- inegi_validate_path(path)
  credential <- if (is.null(token)) "[REDACTED]" else inegi_validate_token(token)
  paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/",
         paste(c(path, credential), collapse = "/"), "?type=json")
}

inegi_build_request <- function(path, token) {
  req <- httr2::request(inegi_request_url(path, token))
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_options(req, followlocation = FALSE, maxredirs = 0L)
  # Classify statuses ourselves, without httr2's token-bearing parent errors.
  httr2::req_error(req, is_error = function(resp) FALSE)
}

inegi_http_perform <- function(req) httr2::req_perform(req)
inegi_retry_sleep <- function(seconds) Sys.sleep(seconds)

inegi_retry_delay <- function(response, attempt) {
  delay <- suppressWarnings(as.numeric(httr2::resp_header(response, "retry-after")))
  if (length(delay) != 1L || !is.finite(delay) || delay < 0) {
    delay <- 2^(attempt - 1L)
  }
  min(delay, 5)
}

inegi_request <- function(path, token) {
  # Keep every unsafe operation inside the suppression boundary. The outward
  # condition deliberately retains no parent, call, request, response or body.
  req <- inegi_build_request(path, token)
  for (attempt in seq_len(3L)) {
    response <- tryCatch(suppressWarnings(suppressMessages(inegi_http_perform(req))),
                         error = function(e) NULL)
    if (is.null(response)) {
      inegi_abort("Connection failed; verify connectivity and retry.", "connection")
    }
    status <- tryCatch(httr2::resp_status(response), error = function(e) NA_integer_)
    if (length(status) != 1L || is.na(status)) {
      inegi_abort("The server returned an invalid HTTP response.", "response")
    }
    if (status %in% c(401L, 403L)) {
      inegi_abort("Authentication failed; verify the INEGI token.", "authentication")
    }
    if (status %in% c(429L, 500L, 502L, 503L, 504L) && attempt < 3L) {
      delay <- tryCatch(inegi_retry_delay(response, attempt), error = function(e) 1)
      inegi_retry_sleep(delay)
      next
    }
    if (status < 200L || status >= 300L) {
      inegi_abort("HTTP request failed or was redirected.", "http")
    }
    body <- tryCatch(suppressWarnings(suppressMessages(
      jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE))),
      error = function(e) NULL)
    if (!is.list(body) || is.null(names(body)) || "ErrorInfo" %in% names(body)) {
      inegi_abort("The server returned malformed JSON or an API error.", "response")
    }
    return(body)
  }
}
