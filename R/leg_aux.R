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
#' \dontrun{
#' token<-"webservice_token"
#' INPC<-serie_inegi(INPC, token)
#' Inflacion<-YoY(INPC$Valores,12)
#' }
#' @export
#' 

YoY<-function (serie, lapso, decimal = TRUE){
  if(NROW(serie)<=lapso){
    stop("Muy pocos renglones o mal especificado el lapso")
  }
  if(!("numeric"==class(serie)))
  {
    stop("No es un vector numerico")
  } 
  else{
    indexes<-1:(NROW(serie)-lapso)
    s<-c(rep(NA, lapso),(serie[indexes+lapso]-serie[indexes])/serie[indexes])
      if(decimal) {return(s)}
          else    {return(s*100)}
  }
}
#' Ordenar por conteo de factores
#'
#' Wrapper para ordenar rapidamente de mayor a menor por grupos un data.frame. 
#'
#' @param df Data.frame a condensar
#' @param col Columna con factores. Se pone sin parentesis.
#'
#' @author Eduardo Flores 
#' @return Data.frame
#' @seealso denue_varios_stats 
#' @examples
#' df<-data.frame(factores=c("A","A","B","C","C","D","A","A"),otros=c(1,3,2,4,5,1,2,7))
#' 
#' #Ordenar, de mayor a menor, por conteo de factores
#' PorConteo<-ordenar_porconteo(df, factores)
#' 
#' @export
#' 

ordenar_porconteo<-function(df,col)
{ #para poner solamente el nombre de columna
  columna<-as.character(eval(substitute(col), df, parent.frame()))
  
  # agrupar
  set<-stats::aggregate(x = df, by = list(columna), FUN = length)
  set<-set[,names(set)[1:2]]
  # ordenar mayor a menor
  ordenado<-set[order(set[,names(set[2])], decreasing=TRUE),]
  # export
  return(ordenado)
}

#' Traer n datos mas recientes
#'
#' Wrapper para ordenar de mayor a menor serie y traer solamente últimos 13 periodos. Prefente para series mensuales.
#' 
#' 
#' @param serie serie en data.frame
#' @param col Columna con fechas
#' @param n cantidad de periodos a traer
#'
#' @author Eduardo Flores 
#' @return Data.frame
#' @seealso denue_varios_stats 
#' @examples
#' #Ver solamente ultimos 13 meses
#' \dontrun{
#' Ultimos<-ultimos(Inflacion, n = 12)
#' }
#' 
#' @export
#' 

ultimos<-function(serie, col = "Fechas", n = 12)
{ #para poner solamente el nombre de columna
  if(col=="Fechas")
  {columna<-"Fechas"} else {
  columna<-as.character(eval(substitute(col), serie, parent.frame()))
  }
  
  if(class(serie[,columna])=="Date"){} else {stop(print("Columna no es fecha"))}
  
  #ordenar tiempos
  orden<-order(serie[,columna])
  ordenado<-serie[orden,]

  # ultimas 13
  n_1<-length(ordenado[,1])
  n_2<-n_1-n
    if(n_2<1){stop(print("Serie es mas corta que 13 observaciones"))} else {}
  set<-ordenado[n_2:n_1,]
  # export
  return(set)
}

#' Crece una serie por tasas 
#'
#' Al especificar un dato inicial, "crece" una serie de datos usando un vector de tasas de crecimiento. La tasa se hace de periodo en periodo. 
#'
#' @param tasas vector con tasas de crecimiento
#' @param comienzo número inicial
#'
#' @author Eduardo Flores 
#' @return Vector numerico
#' @seealso series_crecimiento_regiones
#' @examples
#' tasas_crecimiento<-c(1.10,1.20,1.05,1.02,1.10)
#' 
#' # Crecer por esas tasas (en cada periodo) el 100:
#' Resultados<-crecer(tasas = tasas_crecimiento, comienzo = 100)
#' 
#' @export
#' 

crecer<-function(tasas, comienzo)
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