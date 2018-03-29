#' Calculates growth
#'
#' Year over year growth (or versus any period) 
#'
#' @param serie numeric vector or series
#' @param lapso period separations (12 = for monthly data, 4 = quaterly data)
#' @param decimal Should result be in decimals? Default = TRUE. False returns percents x 100.
#'
#' @author Eduardo Flores 
#' @return Vector numeric
#'
#' @note 
#' Vector must be in ascending order (oldest to newest). The inegi_series() function returns in that order.
#'
#' @examples
#' # Calculate inflation
#' \dontrun{
#' token <- "webservice_token"
#' INPC <- serie_inegi(INPC, token)
#' Inflation <- YoY(INPC$Valores, 12)
#' }
#' @export
YoY <- function(serie, lapso, decimal = TRUE){
  if(NROW(serie) <= lapso){
    stop("Not enough rows for lapso parameter")
  }
  if(!("numeric"==class(serie)))
  {
    stop("No a numeric vector")
  } 
  else{
    indexes<-1:(NROW(serie)-lapso)
    s<-c(rep(NA, lapso),(serie[indexes+lapso]-serie[indexes])/serie[indexes])
      if(decimal) {return(s)}
          else    {return(s*100)}
  }
}
#' Order factors by count
#'
#' Wrapper for rapid ordering in a data.frame. This is a legacy function.
#'
#' @param df Data.frame
#' @param col Columna with factor. (Bare, no parenthesis).
#'
#' @author Eduardo Flores 
#' @return data.frame
#' @seealso denue_varios_stats 
#' @examples
#' df <- data.frame(factors=c("A","A","B","C","C","D","A","A"),
#'                  others=c(1,3,2,4,5,1,2,7))
#' #order by count
#' ByCount <- ordenar_porconteo(df, factors)
#' 
#' @export
ordenar_porconteo <- function(df,col)
{ #para poner solamente el nombre de columna
  columna<-as.character(eval(substitute(col), df, parent.frame()))
  
  # agrupar
  set <- stats::aggregate(x = df, by = list(columna), FUN = length)
  set <- set[,names(set)[1:2]]
  # ordenar mayor a menor
  ordenado <- set[order(set[,names(set[2])], decreasing=TRUE),]
  # export
  return(ordenado)
}

#' Returns n most recient data points
#'
#' Wrapper for other functions
#' 
#' @param serie serie in data.frame
#' @param col Column with dates
#' @param n amount of periods
#'
#' @author Eduardo Flores 
#' @return data.frame
#' @seealso denue_varios_stats 
#' @examples
#' #return last 13 months
#' \dontrun{
#' Ultimos <- ultimos(Inflation, n = 12)
#' }
#' 
#' @export
ultimos <- function(serie, col = "Fechas", n = 12)
{ #para poner solamente el nombre de columna
  if(col=="Fechas")
  {columna<-"Fechas"} else {
  columna<-as.character(eval(substitute(col), serie, parent.frame()))
  }
  
  if(class(serie[,columna])=="Date"){} else {stop(print("Columna is not a date"))}
  
  #ordenar tiempos
  orden<-order(serie[,columna])
  ordenado<-serie[orden,]

  # ultimas 13
  n_1<-length(ordenado[,1])
  n_2<-n_1-n
    if(n_2<1){stop(print("Series is shorter then n"))} else {}
  set<-ordenado[n_2:n_1,]
  # export
  return(set)
}
#' Grows a series by a set rate 
#'
#' When specifying an initial starting value, this "grows" the value by a vector of growth rates. This is a legacy function. 
#' 
#' @param tasas vector with rates
#' @param comienzo initial value
#'
#' @author Eduardo Flores 
#' @return numeric
#' @seealso series_crecimiento_regiones
#' @examples
#' rates <- c(1.10,1.20,1.05,1.02,1.10)
#' 
#' # Grow by that rate
#' Results <- crecer(tasas = rates, comienzo = 100)
#' 
#' @export
crecer <- function(tasas, comienzo)
{
  m<-tasas
  r<-0:length(m)
  n<-comienzo
  
  for (i in 1:length(m))
  { 
    r[i]<-m[i]*n
    n<-r[i]
  }
  
  salida<-r[1:length(m)]
  return(salida)
}

#' Compacts metadata into a data.frame 
#'
#' Returns data.frame with metadata and data from \code{inegi_series()} in data.frame form. Each metadata data is replicated in its corresponding column. 
#'
#' @param series INEGI series as passed to \code{inegi_series()}
#' @param token INEGI API token
#'
#' @author Eduardo Flores 
#' @examples
#' \dontrun{
#' df <- compact_inegi_series(GDP_seriescode, token)
#' }
#' 
#' @export
compact_inegi_series <- function(series, token){
  d <- inegiR::inegi_series(series = series, token = token, metadata = TRUE)
  dat <- d$Data
  dat$Name <- d$MetaData$Name
  dat$Update <- d$MetaData$LastUpdate
  dat$Region <- d$MetaData$Region
  dat$Units <- d$MetaData$Units
  dat$Indicator <- d$MetaData$Indicators
  dat$Frequency <- d$MetaData$Frequency
  return(dat)
}

#' INEGI code to call 
#'
#' This function is a simple paste0() command that allows you to only pass a unique indicator ID to inegi_series(), instead of the entire URL string.
#'
#' @param id numeric INEGI id number
#' @note Works only for national statistics (00000 geography code)
#' @author Eduardo Flores 
#' @return string
#'
#' @examples
#' # Get the corresponding URL for GDP
#' GPD_ID <- 381016
#' GDP_CALL_URL <- inegi_code(381016)
#' @export
inegi_code <- function(id){
  a <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/"
  b <- "/00000/es/false/xml/"
  
  paste0(a, id, b)
}