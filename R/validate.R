inegi_scalar_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

inegi_validate_token <- function(token) {
  if (!inegi_scalar_character(token) || !grepl("^[A-Za-z0-9_-]+$", token)) {
    inegi_abort("Supply one nonblank API token containing only letters, digits, hyphens or underscores.", "input")
  }
  unname(token)
}

inegi_validate_id <- function(x, arg = "series", multiple = FALSE) {
  valid_length <- length(x) > 0L && (multiple || length(x) == 1L)
  if (!valid_length || is.object(x)) {
    inegi_abort("IDs must be digit strings or safe positive integer-valued numbers.", "input")
  }
  if (is.numeric(x)) {
    if (anyNA(x) || any(!is.finite(x) | x <= 0 | x != trunc(x) | x > 2^53 - 1)) {
      inegi_abort("Numeric IDs must be finite positive integers no larger than 2^53 - 1.", "input")
    }
    return(unname(format(x, scientific = FALSE, trim = TRUE, digits = 16L)))
  }
  if (!is.character(x) || anyNA(x) || any(!grepl("^[0-9]+$", x))) {
    inegi_abort("IDs must contain digits only; URLs and path fragments are not accepted.", "input")
  }
  unname(x)
}

inegi_validate_geography <- function(geography) {
  if (!inegi_scalar_character(geography) || !grepl("^[0-9]+$", geography)) {
    inegi_abort("geography must be one digit string; retain leading zeros.", "input")
  }
  unname(geography)
}

inegi_validate_database <- function(database) {
  if (!inegi_scalar_character(database) || !database %in% c("BIE", "BISE")) {
    inegi_abort("database must be BIE or BISE.", "input")
  }
  unname(database)
}

inegi_validate_language <- function(language) {
  if (!inegi_scalar_character(language) || !language %in% c("es", "en")) {
    inegi_abort("language must be es or en.", "input")
  }
  unname(language)
}

inegi_validate_flag <- function(x, arg = "flag") {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    inegi_abort("Flags must be one TRUE or FALSE value.", "input")
  }
  unname(x)
}
