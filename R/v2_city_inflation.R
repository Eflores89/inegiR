
#' Inflation by city
#'
#' Returns monthly year-over-year inflation rates for 46 main cities in Mexico.
#' Wrapper for \code{inegi_series()} and \code{YoY()}.
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
#' @name city_inflation
NULL

#' @export
#' @rdname city_inflation
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

#' @export
#' @rdname city_inflation
inflacion_ciudades<-function(token){
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
    names(dloads[[i]])<-c(names(dloads[i]),"Fechas")
  }
  
  #join
  df<-Reduce(function(...) merge(..., all=TRUE), dloads)
  
  # year over year
  ts<-apply(df[,2:47],
            2, function(x){
              inegiR::YoY(serie = x, lapso = 12, decimal = FALSE)})
  ts<-as.data.frame(ts)
  # bind
  ts$Fechas<-df$Fechas
  warning("This function is not being maintained. Use city_inflation() instead.")
  return(ts)
}