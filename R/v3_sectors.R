#' Sectorial activity indices
#'
#' Returns indexes of economic sector as defined in INEGI (subsectors of IGAE). 
#' None of the series are seasonally adjusted. 
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' sectors <- inegi_sectors(token)
#' }
#' @name inegi_sectors
NULL

#' @export
#' @rdname inegi_sectors
inegi_sectors <- function(token)
{#traer sectores
  
  match_new <- function(d, id){
    names(d) <- c("dates", "dates_shortcut", id, "notes")
    d <- d[, 1:3]
    d
  }
  
  #primay
  s1 <- "383153"
  #secundary
  s2 <- "383154"
  #terciary
  s3 <- "383159"
  
  i1 <- inegiR::inegi_series(s1, token)
  i1 <- match_new(i1, "primary")
  
  i2 <- inegiR::inegi_series(s2, token)
  i2 <- match_new(i2, "secondary")
  
  i3 <- inegiR::inegi_series(s3, token)
  i3 <- match_new(i3, "terciary")

  #union
  df<-Reduce(function(...) merge(...,all=TRUE),list(i1, i2, i3))
  return(df)
}