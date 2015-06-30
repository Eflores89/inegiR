#' Calcular tasas de crecimiento
#'
#' Calcula tasas de crecimiento de una serie. 
#'
#' @param serie vector o serie de tiempo con datos númericos
#' @param lapso separaciones por año a contemplar (12 = datos mensuales, 4 = datos trimestrales)
#' @param decimal ¿Quieres que el resultado este en decimales? Default = TRUE. False obtiene el decimal x 100.
#'
#' @author Eduardo Flores 
#' @return Vector numerico
#'
#' @note 
#' La serie debe estar en orden asciendiente (Posición inicial es la más antigua). La función de Serie_Inegi() guarda en ese orden.
#'
#' @examples
#' #Calcular la inflación (Ver Inflacion_Inegi() para un método más directo)
#' INPC<-Serie_Inegi(INPC,token)
#' Inflacion<-YoY(INPC$Valores,12)
#' @export
#' 

YoY<-function (serie,lapso,decimal=TRUE){
  if(NROW(serie)<=lapso){
    stop("Muy pocos renglones o mal especificado el lapso")
  }
  if(!("numeric"==class(serie)))
  {
    stop("No es un vector numerico")
  } 
  else{
    indexes<-1:(NROW(serie)-lapso)
    s<-c(rep(NA,lapso),(serie[indexes+lapso]-serie[indexes])/serie[indexes])
      if(decimal) {return(s)}
          else    {return(s*100)}
  }
}

#' Unir series por fecha
#'
#' Permite unir tres o más series de indicadores en un data.frame con fechas. Requiere de una lista y diferentes nombres por indicador.
#'
#' @param serie vector o serie de tiempo con datos númericos
#' @param lapso separaciones por año a contemplar (12 = datos mensuales, 4 = datos trimestrales)
#' @param decimal ¿Quieres que el resultado este en decimales? Default = TRUE. False obtiene el decimal x 100.
#'
#' @author Eduardo Flores 
#' @return Vector numerico
#'
#' @note 
#' Las series deben de tener diferentes nombres, excepto columna a unir (Fechas). El argumento es de tipo Lista. El argumento debe ser explícitamente anunciad
#' 
#' @examples
#' #Unir Crecimiento, Inflación y Balanza comercial
#' token<-"abc123"
#' Inflacion<-Inflacion_General(token)
#' Balanza<-Balanza_Comercial(token)
#' Actividad<-Tasa_IGAE(token)
#' ReporteUnido<-UnirFechas(lista=list(Inflacion,Balanza,Actividad))
#' @export
#' 

UnirFechas<-function (lista){
  #asegurar que es lista
  if(class(lista)=="list"){} else {stop("Argumento debe ser una lista de Data.frames")}
  
  #asegurar que no hay nombres repetidos
  #for (i in 1:length(lista)) {x<-names(lista[[i]])}
  #names(lista)
  
df<-Reduce(function(...) merge(...,all=T),lista)
return(df)
}


