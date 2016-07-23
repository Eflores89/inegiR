#' Get rate of inflation
#'
#' Returns anual inflation rate (national, overall rate). Technically, it is the percent anual change of the INPC index.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}. 
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' inflation <- overall_inflation(token)
#' }
#' @export
#' 
overall_inflation <- function (token){
  #Serie de INPC general
  s<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
  i<-inegiR::inegi_series(s, token)
  t<-inegiR::YoY(serie = i$Values, lapso = 12, decimal = FALSE)
  d<-cbind.data.frame(Dates=i$Dates, Values=t)
  return(d)
}
#' Terms of trade
#'
#' Returns the terms of trade for Mexico, defined as the price index of exports over the price index of imports.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}.
#'
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame 
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' tot <- trade_terms(token)
#' } 
#' @export
#'

trade_terms <- function(token)
{ #calcular terminos de intercambio (Terms-Of-Trade)
  x<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37502/00000/es/false/xml/"
  m<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/37503/00000/es/false/xml/"
  
  x_val<-inegiR::inegi_series(x,token)
  names(x_val)<-c("x","Dates")
  m_val<-inegiR::inegi_series(m,token)
  names(m_val)<-c("m","Dates")
  
  df<-Reduce(function(...) merge(...,all=TRUE), list(m_val,x_val))
  df$ToT<-df$x/df$m
  
  d<-cbind.data.frame(Dates=df$Dates, Values=df$ToT)
  return(d)
}

#' Inflation by city
#'
#' Returns monthly year-over-year inflation rates for 46 main cities in Mexico.
#' Wrapper for \code{serie_inegi()} and \code{YoY()}.
#'
#' @param token token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return data.frame 
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' cities <- city_inflation(token)
#' }
#' @export
city_inflation <- function(token){
  #Series de INPC;
  SeriesDf<-
    data.frame(
  "Ciudad"=c(
      "DF","Merida","Morelia","Guadalajara","Monterrey",
      "Mexicali","CdJuarez","Acapulco","Culiacan","Leon",
      "Puebla","SanLuisPotosi","Tapachula","Toluca","Torreon",
      "Veracruz","Villahermosa","Tampico","Chihuahua","Hermosillo","Monclova",
      "Cordoba","Ags","Tijuana","Matamoros","Colima","LaPaz","Chetumal",
      "Jacona","Fresnillo","Iguala","Huatabampo","Tulancingo","Cortazar",
      "CdJimenez","Durango","Tepic","Oaxaca","Queretaro","Cuernavaca",
      "Tlaxcala","SanAndres","Campeche","Tepatitlan","Tehuantepec","CdAcuna"), 
  "Data"=c(
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216095/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216096/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216097/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216098/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216099/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216100/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216101/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216102/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216103/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216104/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216105/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216106/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216107/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216108/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216109/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216110/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216111/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216112/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216113/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216114/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216115/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216116/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216117/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216118/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216119/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216120/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216121/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216122/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216123/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216124/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216125/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216126/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216127/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216128/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216129/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216130/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216131/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216132/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216133/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216134/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216135/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216136/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216137/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216138/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216139/00000/en/false/xml/",
  "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216140/00000/en/false/xml/"
    ), 
  stringsAsFactors = FALSE)
  
  # download
  dloads<-list()
  for(i in 1:46)
  {
    s<-SeriesDf$Data[i]
    dloads[[i]]<-inegiR::serie_inegi(serie = s, 
                                     token)
  }
  
  # names
  names(dloads)<-as.character(SeriesDf$Ciudad)
  for(i in 1:46)
  {
    names(dloads[[i]])<-c(names(dloads[i]),"Dates")
  }
  
  #join
  df<-Reduce(function(...) merge(..., all=TRUE), dloads)
  
  # year over year
  ts<-apply(df[,2:47],
            2, function(x){
              inegiR::YoY(serie = x, lapso = 12, decimal = FALSE)})
  ts<-as.data.frame(ts)
  # bind
  ts$Dates<-df$Dates
  return(ts)
}
