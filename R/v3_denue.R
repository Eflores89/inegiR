#' Returns DENUE businesses
#'
#' Returns data.frame with businesses registered in DENUE in the vicinity of supplied coordinates. 
#'
#' @param latitud Character vector with latitud (in decimals)
#' @param longitud Character vector with longitud (in decimals)
#' @param token API token supplied by INEGI
#' @param meters Meters to search in a circle from coordinates. Defaults to 250 
#' @param keyword Keyword to search in business description (in spanish). Defaults to all (todos). 
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores 
#' 
#' @examples
#' # All businesses in a 1 km radius from the Macroplaza in Monterrey, Mex.
#' \dontrun{
#' token<-"webservice_token"
#' latitud<- 25.669194
#' longitud<- -100.30990
#' businesses <- inegi_denue(latitud, longitud, token, meters = 1000)
#' }
#'
#' @importFrom XML xmlToList
#' @importFrom XML xmlParse
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom plyr ldply
#' @name inegi_denue
NULL

#' @export
#' @rdname inegi_denue
inegi_denue <- function(latitud, longitud, token, meters = 250, keyword = "todos")
{ 
  
#Revisar que coordenadas esten en Mexico---
.EstaEnMexico<-function(latitud,longitud){
  if(as.numeric(latitud)<14.559507 | as.numeric(latitud)> 32.757120 | as.numeric(longitud)> -86.708301 | as.numeric(longitud)< -118.312155)
  {FALSE} else {TRUE}}
  
  if(.EstaEnMexico(latitud, longitud)){}else{stop("Coordinates are not in Mexico")}

#configurar consulta
  url <- "http://www3.inegi.org.mx/sistemas/api/denue/v1/consulta/buscar/"
  coordenadas <- paste0(latitud, ",", longitud)
  consulta <- paste0(url, keyword, "/", coordenadas, "/", meters, "/", token)
  
      # extraccion inicial
         #codigo de xmlparse usa print() como "warnings". Esto lo suprime,
          invisible(
            utils::capture.output(
                s<-xmlToList(xmlParse(consulta, isHTML = TRUE, encoding = "UTF-8"))
                  ))
      l <- strsplit(x = as.character(s$body), split = "\",\"|}")
      l <- as.list(l[[1]])
          largo <- length(l)-1
      l <- l[1:largo]
      
      # limpia
      l_limpia <- gsub(pattern = "\"", replacement = "",l)  
      l_limpia <- gsub(pattern = "{", replacement = "", l_limpia, perl=TRUE)
    
      # Revisar que sea divisible
        if(length(l_limpia)%%18==0) {} else {
          stop(print("Error in data definition. Number of columns for one or more businesses is != 18"))}
      
      # dividir
      l_split <- split(x = l_limpia, f = 1:18)
  
      # Hacer en un data.frame 
      LimpiarRapido <- function(pat, elemento)
        {
          exit <- substr(elemento, regexpr(pattern = pat, text = elemento)+1, stop = 4000)
          return(exit)
        }
      df <- data.frame(
          id = LimpiarRapido(":",l_split$'1'),
          name = LimpiarRapido(":",l_split$'2'),
          legal_name = LimpiarRapido(":",l_split$'3'),
          activity = LimpiarRapido(":",l_split$'4'),
          economic_class = LimpiarRapido(":",l_split$'5'),
          roads = LimpiarRapido(":",l_split$'6'),
          street = LimpiarRapido(":",l_split$'7'),
          exterior_num = LimpiarRapido(":",l_split$'8'),
          interior_num = LimpiarRapido(":",l_split$'9'),
          colonia = LimpiarRapido(":",l_split$'10'),
          postal_code = LimpiarRapido(":",l_split$'11'),
          location = LimpiarRapido(":",l_split$'12'),
          tel = LimpiarRapido(":",l_split$'13'),
          email = LimpiarRapido(":",l_split$'14'),
          website = LimpiarRapido(":",l_split$'15'),
          type = LimpiarRapido(":",l_split$'16'),
          longitud = LimpiarRapido(":",l_split$'17'),
          latitud = LimpiarRapido(":",l_split$'18'),
          stringsAsFactors = FALSE
        )
  return(df)
}
#' Find businesses in a grid larger than 5 kms
#'
#' Returns data.frame with businesses registered in the DENUE in spaces larger than 5 kilometers.
#' Calls \code{make_grid}. Functions contributed by Arturo Cardenas \url{https://github.com/arturocm}. 
#' 
#' @details 
#' Makes a loop for each pair of coordinates, creating a grid to extract businesses inside. Uses maximum and minimum coordinate pairs to draw frame.
#' @param lat1 First corner (latitud)
#' @param lat2 Second corner (latitud)
#' @param lon1 First corner (longitud)
#' @param lon2 Second corner (longitud)
#' @param token API token supplied by INEGI
#' @param meters Distance in meters to search by coordinate
#' @param keyword Keyword of businesses to include. Defaults to all ("todos")
#' @param space_lat Space between latitud coordinates defaults to 0.07 degrees
#' @param space_lon Space between longitud coordinates defaults to 0.07 degrees
#' @param uniqueonly Default = TRUE, eliminates duplicate businesses
#' 
#' 
#' @return Data.frame
#'
#' @author Arturo Cardenas
#' 
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' latitud1 <- 25.669194
#' latitud2 <- 25.169194
#' longitud1 <- -100.30990
#' longitud2 <- -101.20102
#' businesses <- inegi_denue_grid(latitud1, latitud2, longitud1, longitud2, token)
#' }
#'
#' @export
inegi_denue_grid <- function(lat1, lat2, lon1, lon2, token, 
                             meters = 5000, 
                             keyword = "todos", 
                             space_lat = 0.07, 
                             space_lon = 0.07, 
                             uniqueonly = TRUE){
  #antes de correr, revisar datos de columnas
  if(class(lat1)=="numeric"){} else {stop(print("Latitud 1 is not numeric"))}
  if(class(lat2)=="numeric"){} else {stop(print("Latitud 2 is not numeric"))}
  if(class(lon1)=="numeric"){} else {stop(print("Longitud 1 is not numeric"))}
  if(class(lon2)=="numeric"){} else {stop(print("Longitud 2 is not numeric"))}
  
  grid <- inegiR::make_grid(lat1 = lat1, 
                            lat2 = lat2, 
                            lon1 = lon1, 
                            lon2 = lon2, 
                            space_lat = space_lat, 
                            space_lon = space_lon)
  output <- data.frame()
  for (i in 1:nrow(grid)){
    mapa <- tryCatch(inegiR::inegi_denue(latitud = grid[i,1], 
                                         longitud = grid[i,2],
                                         token = token,
                                         meters = meters,
                                         keyword = keyword),
                     # skip si no hay algo en DENUE
                     error = function(e){})
    output <- rbind.data.frame(output, mapa)
  }
  if(uniqueonly){
    df <- unique(output)
  }else{
    df <- output
  }
  return(df)
}
#' Returns statistics of coordinate 
#' 
#' Returns basic statistics of businesses, using DENUE, in the vecinity of coordinates. 
#' 
#' @details Some columns, like employee numbers are experimental (the employees are added considering size of company).
#' 
#'
#' @param latitud_vector number of column in data with latitud column
#' @param longitud_vector number of column in data with longitud column
#' @param token API token supplied by INEGI
#' @param meters Distance in meters to search by coordinate
#' @param keyword Keyword of businesses to include. Defaults to all ("todos")
#'
#' 
#' @return Data.frame
#'
#' @author Eduardo Flores
#'
#' @examples
#' \dontrun{
#' token<-"webservice_token"
#' df <- as.data.frame(latitud  = c(25.669194, 25.121194),
#'                   longitud = c(-100.30990, -99.81923))
#' stats <- denue_varios_stats(data     = df,
#'                           col_lat  = 1,
#'                           col_long = 2,
#'                           metros   = 500)
#'}
#' @export
inegi_denue_stats <- function(latitud_vector, longitud_vector, token, meters = 250, keyword = "todos")
{ #cuantas coordenadas a revisar
  n <- length(latitud_vector)
  if(length(latitud_vector) == length(longitud_vector)){}else{stop("Latitud and Longitud vectors must be of same length")}
  
  #antes de correr, revisar datos de columnas
  if(class(latitud_vector)=="numeric"){} else {stop(print("Latitud vector must be numeric"))}
  if(class(longitud_vector)=="numeric"){} else {stop(print("Longitud vector must be numeric"))}
  
  # inicial data.frame
  d <- data.frame(
    businesses = 1:n,
    unique_streets = 1:n,
    unique_activities = 1:n,
    are_fixed = 1:n,
    have_legalname = 1:n,
    have_phone = 1:n,
    have_website = 1:n,
    in_avenue = 1:n,
    estimated_employees = 1:n,
    employee_stdev  = 1:n
  )
  
  #loop
  for (i in 1:n)
  { tryCatch({
    
    #crear matriz por negocio
    m <- inegiR::inegi_denue(latitud  = latitud_vector[i],
                             longitud = longitud_vector[i],
                             token    = token,
                             meters   = meters,
                             keyword  = keyword
    )
    
    # add approximate number of employees 
    for (j in 1:length(m[,1]))
    {
      m$employees[j]<-if(grepl(x = m["economic_class"][j,],pattern = "^11")){as.numeric(20)} else {
        if(grepl(x = m["economic_class"][j,],pattern = "^0")){as.numeric(3)} else {
          if(grepl(x = m["economic_class"][j,],pattern = "^6")){as.numeric(8)} else {
            if(grepl(x = m["economic_class"][j,],pattern = "^31")){as.numeric(40)} else {
              if(grepl(x = m["economic_class"][j,],pattern = "^101")){as.numeric(220)} else {
                if(grepl(x = m["economic_class"][j,],pattern = "^51")){as.numeric(75)}  else {
                  #mas de 250
                  as.numeric(300)
                } } } } } } }
    
    #cantidad de negocios
    d[i,]$businesses <- if(is.null(length(m[,1]))){0} else {length(m[,1])}
    #cantidad de calles diferentes
    d[i,]$unique_streets <- if(is.null(length(unique(m[,"street"])))){0} else {length(unique(m[,"street"]))}
    #cantidad de actividades diferentes
    d[i,]$unique_activities <- if(is.null(length(unique(m[,"activity"])))){0} else {length(unique(m[,"activity"]))}
    #negocios con telefono
    d[i,]$have_phone <- if(is.null(length(m[!grepl(pattern = "^$",m["tel"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["tel"][,1]),][,1])}
    #negocios con sitios de internet - no WWW, 
    d[i,]$have_website <- if(is.null(length(m[!grepl(pattern = "^$",m["website"][,1]),][,1]))) {0} else {length(m[!grepl(pattern = "^$",m["website"][,1]),][,1])}
    #negocios con razon social (la que sea)
    d[i,]$have_legalname <- if(is.null(length(m[!grepl(pattern = "^$",m["legal_name"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["legal_name"][,1]),][,1])}
    #negocios ubicados en avenida
    d[i,]$in_avenue <- if(is.null(length(subset(m,m$roads == "AVENIDA")[,1]))){0} else {length(subset(m,m$roads == "AVENIDA")[,1])}
    #negocios fijos
    d[i,]$are_fixed <- if(is.null(length(subset(m,m$type == "Fijo")[,1]))){0} else {length(subset(m,m$type == "Fijo")[,1])}
    #estimacion de empleados en base a columna de estrato
    d[i,]$estimated_employees <- if(is.null(sum(as.numeric((m[,"employees"]))))){0} else {sum(as.numeric((m[,"employees"])))}
    #desviacion standar de empleados en comercios
    d[i,]$employee_stdev <- if(is.null(stats::sd(x = as.numeric((m[,"employees"]))))) {0} else {stats::sd(x = as.numeric((m[,"employees"])))}
  
    #termina instancia de tryCatch
  }, error = function(e){
    #continuar en caso de error en una de las coordenadas
  }
  #fin trycatch
  )
  }
  
  #calculos sobres data frame
  d$businesses_per_street <- d$businesses/d$unique_streets
  d$businesses_per_meter <- d$businesses/meters
  
  d$employees_per_business <- d$estimated_employees/d$businesses
  d$employees_per_meter   <- d$estimated_employees/meters
  

  #exportar
  return(d)
}