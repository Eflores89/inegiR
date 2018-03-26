#' GDP growth rate
#'
#' Returns GDP growth rate vs. same period a year earlier. Wrapper of \code{serie_inegi()} and \code{YoY()}.
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @note BIE route: Indicadores económicos de coyuntura ... Producto interno bruto trimestral, base 2008 ... Series originales ... Valores a precios de 2008 ... Producto interno bruto, a precios de mercado 
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' # interchangeable
#' gdp <- rate_GDP(token)
#' gdp <- tasa_PIB(token)
#' }
#' @name GDP
NULL

#' @export
#' @rdname GDP
rate_GDP <- function (token){
  #Serie de PIB;
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i <- inegiR::inegi_series(series = s, token)
  t <- inegiR::YoY(serie = i$Valores, lapso = 4, decimal = FALSE)
  d <- cbind.data.frame(Values=t, Dates = i$Fechas)
  return(d)
}
#' @export
#' @rdname GDP
tasa_PIB<-function (token){
  #Serie de PIB;
  s <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/381016/00000/es/false/xml/"
  i <- inegiR::serie_inegi(s, token)
  t <- inegiR::YoY(serie = i$Valores, lapso = 4, decimal = FALSE)
  d <- cbind.data.frame(Fechas=i$Fechas, Valores=t)
  
  warning("This function is not being maintained. Use rate_GDP() instead.")
  return(d)
}