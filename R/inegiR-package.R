#' Integrate INEGI's API with R
#'
#' Retrieve authenticated indicator observations and metadata using the JSON
#' API v2.0. The canonical inegi_data, inegi_latest, inegi_metadata, and
#' inegi_catalog methods preserve raw provider fields and typed empty results.
#' Existing legacy exports are retained.
#' @details This local development repair builds on Eduardo Flores's CC0
#'   package at upstream commit 64d33221507cce2f86151a96fffc3283526a49cf.
#'   It does not claim a published release or upstream acceptance. Authenticated
#'   live behavior and unrelated legacy services have not been verified.
#' @author Eduardo Flores <eduardo@enelmargen.org>
#' @seealso \code{\link{inegi_data}}, \code{\link{inegi_metadata}},
#'   \code{\link{inegi_set_token}}
#' @references \url{https://www.inegi.org.mx/servicios/api_indicadores.html}
#' @name inegiR-package
#' @aliases inegiR
#' @keywords package
"_PACKAGE"
