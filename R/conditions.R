# Only static, package-authored messages belong here. Never pass server text,
# request/response objects or an underlying condition to this function.
inegi_abort <- function(message, category = "response") {
  stop(structure(list(message = paste0("inegiR: ", message), call = NULL),
                 class = c(paste0("inegi_", category, "_error"),
                           "inegi_error", "error", "condition")))
}
