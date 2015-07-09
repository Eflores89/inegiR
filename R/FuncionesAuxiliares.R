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
#' INPC<-serie_inegi(INPC,token)
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
#' #MWE
#' df<-data.frame(factores=c("A","A","B","C","C","D","A","A"),otros=c(1,3,2,4,5,1,2,7))
#' 
#' #Ordenar, de mayor a menor, por conteo de factores
#' PorConteo<-ordenar_porconteo(df,factores)
#' 
#' @export
#' 

ordenar_porconteo<-function(df,col)
{ #para poner solamente el nombre de columna
  columna<-as.character(eval(substitute(col),df, parent.frame()))
  
  # agrupar
  set<-aggregate(x = df,by = list(columna),FUN = length)
  set<-set[,names(set)[1:2]]
  # ordenar mayor a menor
  ordenado<-set[order(set[,names(set[2])],decreasing=TRUE),]
  # export
  return(ordenado)
}