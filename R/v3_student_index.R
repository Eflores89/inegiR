#' Student Price Index
#'
#' Returns the student price index. See \url{http://enelmargen.org/ds/ipe/} for more information. 
#' 
#' @param token API token supplied by INEGI
#' @author Eduardo Flores 
#' @return Data.frame
#'
#' @examples
#' \dontrun{
#' token <- "webservice_token"
#' studentinflation <- inegi_stind(token)
#' }
#' @name inegi_stind
NULL

#' @export
#' @rdname inegi_stind
inegi_stind <- function (token){
  
  match_new <- function(d, id){
    names(d) <- c("dates", "dates_shortcut", id, "notes")
    d <- d[, 1:3]
    d
  }
  
  #Series of INPC;
  s1 <- inegiR::inegi_series("216065",token)
  s1 <- match_new(s1, "s1")
  
  s2 <- inegiR::inegi_series("216066",token)
  s2 <- match_new(s2, "s2")
  
  s3 <- inegiR::inegi_series("216067",token)
  s3 <- match_new(s3, "s3")
  
  s4 <- inegiR::inegi_series("216068",token)
  s4 <- match_new(s4, "s4")
  
  s5 <- inegiR::inegi_series("216069",token)
  s5 <- match_new(s5, "s5")
  
  s6 <- inegiR::inegi_series("216070",token)
  s6 <- match_new(s6, "s6")
  
  s7 <- inegiR::inegi_series("216071",token)
  s7 <- match_new(s7, "s7")
  
  s8 <- inegiR::inegi_series("216072",token)
  s8 <- match_new(s8, "s8")
  
  df <- Reduce(function(...) merge(...,all=T), list(s1,s2,s3,s4,s5,s6,s7,s8))
  df$student_index <- (df$s1*0.331417)+(df$s2*0.032764)+(df$s3*0.077735)+(df$s4*0.00378)+(df$s5*0.028353177)+(df$s6*0.199190)+(df$s7*0.0606992)+(df$s8*0.266067)
  
  df <- df[, c("dates", "dates_shortcut", "student_index")]
  return(df)
}
