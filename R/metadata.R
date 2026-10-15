inegi_catalog_path <- function(catalog, id, language, database) {
  allowed <- c("CL_INDICATOR", "CL_FREQ", "CL_TOPIC", "CL_UNIT",
    "CL_UNIT_MULT", "CL_NOTE", "CL_SOURCE", "CL_GEO_AREA")
  if (!inegi_scalar_character(catalog) || !catalog %in% allowed) {
    inegi_abort("catalog must name a supported CL_ catalog.", "input")
  }
  ids <- if (is.null(id)) "null" else
    paste(unique(inegi_validate_id(id, multiple = TRUE)), collapse = ",")
  c(catalog, ids, inegi_validate_language(language),
    inegi_validate_database(database), "2.0")
}

inegi_parse_catalog <- function(payload, catalog, database, language) {
  if (!inegi_is_record(payload) || !inegi_is_array(payload[["CODE"]])) {
    inegi_abort("Expected a catalog code array in the API response.", "response")
  }
  records <- payload[["CODE"]]
  result <- data.frame(catalog = character(), id = character(),
    description = character(), database = character(), language = character(),
    stringsAsFactors = FALSE)
  rows <- lapply(records, function(record) {
    if (!inegi_is_record(record) ||
        sum(c("value", "Value") %in% names(record)) != 1L ||
        !"Description" %in% names(record)) {
      inegi_abort("Catalog records require an ID and description.", "response")
    }
    id <- inegi_response_field(record, if ("value" %in% names(record)) "value" else "Value")
    description <- inegi_response_field(record, "Description")
    if (is.na(id) || !grepl("^[0-9]+$", id) || is.na(description)) {
      inegi_abort("Catalog records require a digit ID and nonmissing description.", "response")
    }
    data.frame(catalog = catalog, id = id, description = description,
      database = database, language = language, stringsAsFactors = FALSE)
  })
  if (length(rows)) result <- do.call(rbind, rows)
  rownames(result) <- NULL
  list(data = result, records = records)
}

inegi_fetch_catalog <- function(catalog, id, token, language, database) {
  path <- inegi_catalog_path(catalog, id, language, database)
  inegi_validate_token(token)
  inegi_parse_catalog(inegi_request(path, token), catalog, database, language)
}

#' Download an INEGI indicator catalog
#'
#' Catalogs retain provider ordering and leading zeros in identifiers.
#' @param catalog One of CL_INDICATOR, CL_FREQ, CL_TOPIC, CL_UNIT, CL_UNIT_MULT,
#'   CL_NOTE, CL_SOURCE, or CL_GEO_AREA.
#' @param id Optional vector of numeric or digit-string IDs. NULL requests all.
#' @param token INEGI token, defaulting to INEGI_TOKEN.
#' @param language API language, es or en.
#' @param database API bank, BIE or BISE.
#' @return A data.frame with character columns catalog, id, description,
#'   database, and language. Empty catalogs return a typed zero-row frame.
#' @details Uses the authenticated JSON API. Request duplicate IDs are removed
#'   in first-seen order. No credential or request URL is added to the output.
#' @seealso \code{\link{inegi_metadata}}, \code{\link{incat_}}
#' @examples
#' \dontrun{
#' inegi_catalog("CL_FREQ")
#' inegi_catalog("CL_INDICATOR", id = c("216064", "216097"))
#' }
#' @export
inegi_catalog <- function(catalog, id = NULL, token = inegi_get_token(),
                          language = "es", database = "BIE") {
  inegi_fetch_catalog(catalog, id, token, language, database)$data
}

#' Download INEGI indicator metadata
#'
#' Fetches CL_INDICATOR explicitly. Missing, duplicate, and unrequested response
#' IDs are errors. Request duplicates are removed in their first-seen order.
#' @param series Indicator IDs in requested order; duplicates are removed.
#' @inheritParams inegi_catalog
#' @return A data.frame with character provider, series_id, title, database,
#'   and language columns, plus raw_metadata, a list column retaining each
#'   complete provider catalog record without adding request URLs or credentials.
#' @details The metadata record is not expanded into columns, avoiding collisions
#'   between canonical column names and provider keys. Use the same database and
#'   language as an observation request when joining by series_id.
#' @seealso \code{\link{inegi_data}}, \code{\link{inegi_catalog}}
#' @examples
#' \dontrun{
#' x <- inegi_data("216064")
#' meta <- inegi_metadata("216064")
#' x$title <- meta$title[match(x$series_id, meta$series_id)]
#' meta$raw_metadata[[1L]]
#' }
#' @export
inegi_metadata <- function(series, token = inegi_get_token(), language = "es",
                           database = "BIE") {
  series <- unique(inegi_validate_id(series, multiple = TRUE))
  fetched <- inegi_fetch_catalog("CL_INDICATOR", series, token, language, database)
  data <- fetched$data
  if (anyDuplicated(data$id) || length(data$id) != length(series) ||
      !setequal(data$id, series)) {
    inegi_abort("Metadata response IDs must match every requested indicator exactly once.", "response")
  }
  order <- match(series, data$id)
  out <- data.frame(provider = rep("inegi", length(series)), series_id = series,
    title = data$description[order], database = rep(database, length(series)),
    language = rep(language, length(series)), stringsAsFactors = FALSE)
  out$raw_metadata <- fetched$records[order]
  out
}
