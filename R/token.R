#' Read the INEGI token for this R session
#'
#' @return A character token from the INEGI_TOKEN environment variable.
#' @details Obtain a token from
#'   \url{https://www.inegi.org.mx/servicios/api_indicadores.html}.
#'   Set it for the session with \code{inegi_set_token()}. To persist it yourself,
#'   put INEGI_TOKEN=your-token in a private personal .Renviron and restart R.
#'   The getter errors on a missing or blank token. Printing its returned value
#'   displays the credential; the helper does not print it as a side effect.
#' @seealso \code{\link{inegi_set_token}}
#' @examples
#' \dontrun{
#' # Returns a credential: avoid printing or logging the result.
#' token <- inegi_get_token()
#' }
#' @export
inegi_get_token <- function() {
  inegi_validate_token(Sys.getenv("INEGI_TOKEN", unset = ""))
}

#' Set the INEGI token for this R session
#'
#' Sets INEGI_TOKEN without writing credentials to disk or printing the token.
#' @param token A scalar API token.
#' @return NULL, invisibly.
#' @seealso \code{\link{inegi_get_token}}
#' @examples
#' \dontrun{
#' inegi_set_token("your-token")
#' }
#' @export
inegi_set_token <- function(token) {
  token <- inegi_validate_token(token)
  Sys.setenv(INEGI_TOKEN = token)
  invisible(NULL)
}
