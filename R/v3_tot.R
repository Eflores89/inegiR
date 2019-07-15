#' Terms of trade for Mexico
#'
#' Returns the terms of trade for Mexico, defined as the price index of exports over the price index of imports.
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame 
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' tot <- inegi_tot(token)
#' } 
#' @name inegi_tot
NULL

#' @export
#' @rdname inegi_tot
inegi_tot <- function(token)
{ #(Terms-Of-Trade)
  x_val<-inegiR::inegi_series("37502", token)
  names(x_val) <- c("dates", "dates_shortcut", "export_prices", "notes")
  x_val <- x_val[, 1:3]
  
  m_val<-inegiR::inegi_series("37503", token)
  names(m_val) <- c("dates", "dates_shortcut", "import_prices", "notes")
  m_val <- m_val[, 1:3]
  
  df <- Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$terms_of_trade <- df$export_prices/df$import_prices
  
  return(df)
}