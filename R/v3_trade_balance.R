#' Trade balance
#'
#' Returns exports, imports and trade balance (all products, services and countries) in a data.frame.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' external_com <- inegi_tradebal(token)
#' }
#' @name inegi_tradebal
NULL

#' @export
#' @rdname inegi_tradebal
inegi_tradebal <- function(token)
{ #balanza comercial as-is (no YoY)
  x <- "33223"
  m <- "33226"
  
  x_val <- inegiR::inegi_series(x, token)
  x_val <- x_val[, 1:3]
  names(x_val) <- c("dates","dates_shortcut", "exports")
  
  m_val <- inegiR::inegi_series(m, token)
  m_val <- m_val[, 1:3]
  names(m_val) <- c("dates","dates_shortcut", "imports")
  
  df <- Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$trade_balance <- df$exports - df$imports
  
  return(df)
}