#' Balance of Payments for Mexico
#'
#' Returns Current Account revenue, expenses and total and Financial Account total, errors, reservs and adjustments for Mexico.  
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return Data.frame
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' balance_of_payments <- inegi_bop(token)
#' }
#' @name inegi_bop
NULL

#' @export
#' @rdname inegi_bop
inegi_bop <- function(token)
{ 
  
  #Current account
  cc_ing <- inegiR::inegi_series("214053", token)
  names(cc_ing) <- c("dates", "dates_shortcut", "Current Account - Revenue", "notes")
  cc_ing <- cc_ing[, 1:3]
  
  cc_egr <- inegiR::inegi_series("214069", token)
  names(cc_egr) <- c("dates", "dates_shortcut", "Current Account - Expense", "notes")
  cc_egr <- cc_egr[, 1:3]
  
  cc_tot <- inegiR::inegi_series("214052", token)
  names(cc_tot) <- c("dates", "dates_shortcut", "Cuenta Corriente (Total)", "notes")
  cc_tot <- cc_tot[, 1:3]
  
  #Financial account
  cf_tot<-inegiR::inegi_series("214088", token)
  names(cf_tot) <-  c("dates", "dates_shortcut", "Financial Account (Total)", "notes") 
  cf_tot <- cf_tot[, 1:3]
  
  cf_eyo<-inegiR::inegi_series("214113", token)
  names(cf_eyo) <- c("dates", "dates_shortcut", "Financial Account - Errors and Omisions", "notes")
  cf_eyo <- cf_eyo[, 1:3]
  
  cf_res<-inegiR::inegi_series("214114", token)
  names(cf_res) <- c("dates", "dates_shortcut", "Financial Account - Reserves", "notes")
  cf_res <- cf_res[, 1:3]
  
  cf_ajv<-inegiR::inegi_series("214115", token)
  names(cf_ajv) <- c("dates", "dates_shortcut", "Financial Account - Value adjustments", "notes")
  cf_ajv <- cf_ajv[, 1:3]
  
  #union
  df <- Reduce(function(...) merge(..., all=TRUE),list(cc_tot,
                                                       cc_ing,
                                                       cc_egr,
                                                       cf_tot,
                                                       cf_res,
                                                       cf_eyo,
                                                       cf_ajv))
  return(df)
}
