inegi_validate_date <- function(x) {
  if (is.null(x)) return(NULL)
  valid <- length(x) == 1L && !anyNA(x)
  if (inherits(x, "Date")) {
    valid <- valid && is.finite(unclass(x)) && unclass(x) == trunc(unclass(x))
    if (valid) return(x)
  } else if (is.character(x) && valid && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    date <- suppressWarnings(tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) as.Date(NA)))
    if (!is.na(date) && format(date, "%Y-%m-%d") == x) return(date)
  }
  inegi_abort("Dates must be one nonmissing Date or a valid ISO YYYY-MM-DD string.", "input")
}

inegi_fetch_data <- function(series, token, language, geography, database, lastonly = FALSE) {
  series <- unique(inegi_validate_id(series, multiple = TRUE))
  inegi_validate_token(token)
  inegi_validate_language(language)
  inegi_validate_geography(geography)
  inegi_validate_database(database)
  result <- inegi_empty_data()
  for (id in series) {
    path <- inegi_indicator_path(id, language, geography, database, lastonly)
    normalized <- inegi_normalize(inegi_request(path, token), id, database, language)
    result <- rbind(result, normalized)
  }
  rownames(result) <- NULL
  result
}

#' Download canonical INEGI observations
#'
#' Dates represent interval starts. Unknown or ambiguous periods retain their
#' raw text and have missing dates. Bounds are inclusive and require dated rows.
#' @param series Indicator IDs in requested order; duplicates are removed.
#' @param start_date,end_date Optional Date or strict ISO YYYY-MM-DD bounds.
#' @param token INEGI token, defaulting to INEGI_TOKEN.
#' @param language API language, es or en.
#' @param geography Geography code as text, retaining leading zeros.
#' @param database API bank, BIE or BISE.
#' @return A data.frame beginning with character provider, series_id, title,
#'   period; Date date; double value; and character value_raw. Additional
#'   character columns are geography, frequency, unit, unit_multiplier, topic,
#'   source, note, last_updated, series_status, obs_status, obs_exception,
#'   obs_source, obs_note, database, and language. Empty results keep these types.
#' @details Raw periods and values are retained. Missing or nonnumeric values
#'   remain rows with numeric NA; unexpected nonnumeric values without an
#'   exception flag warn. Values are not rescaled by UNIT_MULT or interpolated.
#'   Optional absent provider fields become character NA. Titles are missing
#'   unless supplied by the observation response; use \code{inegi_metadata()}
#'   for explicit catalog retrieval.
#'
#'   Annual frequency codes 1, 2, and 3 accept YYYY or YYYY/01 and use January 1.
#'   Quarterly code 6 accepts YYYY/01 through YYYY/04 and uses quarter starts.
#'   Monthly code 8 accepts YYYY/01 through YYYY/12 and uses month starts.
#'   Unknown or invalid formats and ambiguous biweekly periods are undated.
#'   Bounds filter interval starts locally and reject any undated response row.
#'   Without bounds every period is retained. Requested series are deduplicated
#'   in first-seen order; provider row order is preserved within each series.
#'   Combined calls fail atomically. Both BIE and BISE and es/en are supported.
#' @seealso \code{\link{inegi_latest}}, \code{\link{inegi_metadata}},
#'   \code{\link{inegi_catalog}}, \code{\link{inegi_series}}
#' @examples
#' \dontrun{
#' # Configure INEGI_TOKEN locally before making authenticated API calls.
#' x <- inegi_data(c("216064", "216097"), start_date = "2025-01-01",
#'   end_date = "2025-12-31", language = "es", geography = "00")
#' meta <- inegi_metadata(unique(x$series_id))
#' x$title <- meta$title[match(x$series_id, meta$series_id)]
#' }
#' @export
inegi_data <- function(series, start_date = NULL, end_date = NULL,
                       token = inegi_get_token(), language = "es", geography = "00",
                       database = "BIE") {
  start_date <- inegi_validate_date(start_date)
  end_date <- inegi_validate_date(end_date)
  if (!is.null(start_date) && !is.null(end_date) && start_date > end_date) {
    inegi_abort("start_date must not be later than end_date.", "input")
  }
  result <- inegi_fetch_data(series, token, language, geography, database)
  if (!is.null(start_date) || !is.null(end_date)) {
    if (anyNA(result$date)) {
      inegi_abort("Date bounds require unambiguous dated periods; request without bounds to retain raw periods.", "response")
    }
    keep <- rep(TRUE, nrow(result))
    if (!is.null(start_date)) keep <- keep & result$date >= start_date
    if (!is.null(end_date)) keep <- keep & result$date <= end_date
    result <- result[keep, , drop = FALSE]
    rownames(result) <- NULL
  }
  result
}

#' Download the most recent INEGI observations
#' @inheritParams inegi_data
#' @return A typed data.frame in the same schema as \code{inegi_data()}.
#' @details Sets the API recent-observation flag; it does not select a maximum
#'   date from downloaded history. Provider ordering and all raw fields survive.
#' @seealso \code{\link{inegi_data}}
#' @examples
#' \dontrun{
#' inegi_latest("216064")
#' }
#' @export
inegi_latest <- function(series, token = inegi_get_token(), language = "es",
                         geography = "00", database = "BIE") {
  inegi_fetch_data(series, token, language, geography, database, lastonly = TRUE)
}
