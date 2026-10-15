# Decode only known, validated calendar periods. Dates describe interval starts.
# Raw API period text is retained even when no date can safely be assigned.
# This helper is internal; frequency is the scalar FREQ from one API series.
inegi_period <- function(period, frequency) {
  period <- as.character(period)
  n <- length(period)
  date <- as.Date(rep(NA_character_, n))
  shortcut <- period
  frequency <- if (length(frequency) == 1L) as.character(frequency) else NA_character_

  if (frequency %in% c("1", "2", "3")) {
    valid <- !is.na(period) & grepl("^[0-9]{4}(/01)?$", period)
    year <- suppressWarnings(as.integer(substr(period, 1L, 4L)))
    valid <- valid & !is.na(year) & year >= 1L
    if (any(valid)) {
      date[valid] <- as.Date(paste0(substr(period[valid], 1L, 4L), "-01-01"))
    }
    shortcut[valid] <- "Year"
  } else if (frequency %in% c("6", "8")) {
    valid <- !is.na(period) & grepl("^[0-9]{4}/[0-9]{2}$", period)
    year <- suppressWarnings(as.integer(substr(period, 1L, 4L)))
    interval <- suppressWarnings(as.integer(substr(period, 6L, 7L)))
    limit <- if (frequency == "6") 4L else 12L
    valid <- valid & !is.na(year) & year >= 1L & !is.na(interval) &
      interval >= 1L & interval <= limit
    month <- if (frequency == "6") (interval[valid] - 1L) * 3L + 1L else interval[valid]
    date[valid] <- as.Date(sprintf("%04d-%02d-01", year[valid], month))
    shortcut[valid] <- paste0(if (frequency == "6") "Q" else "M", interval[valid])
  } else if (frequency %in% "9") {
    # YYYY/MM alone does not identify which half-month an observation represents.
    shortcut <- seq_len(n)
  }

  data.frame(period = period, date = date, date_shortcut = shortcut,
             stringsAsFactors = FALSE)
}
