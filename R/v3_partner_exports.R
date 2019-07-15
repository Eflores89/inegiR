#' Exports of Mexico to trade partners
#'
#' Returns exports to main trading partners of all products. Regions are the following: United States, Canada, China, CentralAmerica, SouthAmerica
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' xbycountry <- inegi_partner_exports(token)
#' }
#' @name inegi_partner_exports
NULL

#' @export
#' @rdname inegi_partner_exports
inegi_partner_exports <- function(token)
{ #exports por pais
  usa <- "133172"
  can <- "133171"
  chn <- "133285"
  cam <- "133173"
  sur <- "133183"
  
  match_new <- function(d, id){
    names(d) <- c("dates", "dates_shortcut", id, "notes")
    d <- d[, 1:3]
    d
  }
  
  usa_v <- inegiR::inegi_series(usa, token)
  usa_v <- match_new(usa_v, "usa")
  
  can_v <- inegiR::inegi_series(can, token)
  can_v <- match_new(can_v, "canada")
  
  chn_v <- inegiR::inegi_series(chn, token)
  chn_v <- match_new(chn_v, "china")
  
  cam_v <- inegiR::inegi_series(cam, token)
  cam_v <- match_new(cam_v, "central_america")
  
  sur_v <- inegiR::inegi_series(sur, token)
  sur_v <- match_new(sur_v, "south_america")
  
  df <- Reduce(function(...) merge(..., all = TRUE),
             list(usa_v,can_v,chn_v,cam_v,sur_v))
  return(df)
}