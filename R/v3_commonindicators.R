#' INEGI Direct Indicators
#'
#' Returns common indicators, for simplicity. Will return as a list, with metadata and tibble time dataframe. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores
#' @return Data.frame
#' @details inind_commerce = terciary industrial activity (commercial activity monthly). 
#' inind_auto = auto production. 
#' innind_gpd = Gross Domestic Product.
#' inind_fx = USDMXN Exchange rate.
#' inind_unemp = Unemployment rate.
#' inind_prices = National price index (for inflation).
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' commerce_rate <- inind_commerce(token)
#' }
#' @name inind_
NULL

#' @export
#' @rdname inind_
inind_commerce <- function(token)
{ 
  inegiR::inegi_series(serie = "383160", token, metadata = TRUE, as_tt = TRUE)
}
#' @export
#' @rdname inind_
inind_auto <- function(token)
{ 
  inegiR::inegi_series(serie = "15166", token, metadata = TRUE, as_tt = TRUE)
}
#' @export
#' @rdname inind_
inind_gdp <- function(token)
{ 
  inegiR::inegi_series(serie = "381016", token, metadata = TRUE, as_tt = TRUE)
}
#' @export
#' @rdname inind_
inind_fx <- function(token)
{ 
  inegiR::inegi_series(serie = "824", token, metadata = TRUE, as_tt = TRUE)
}
#' @export
#' @rdname inind_
inind_unemp <- function(token)
{ 
  inegiR::inegi_series(serie = "444612", token, metadata = TRUE, as_tt = TRUE)
}

#' @export
#' @rdname inind_
inind_prices <- function(token)
{ 
  inegiR::inegi_series(serie = "216064", token, metadata = TRUE, as_tt = TRUE)
}
