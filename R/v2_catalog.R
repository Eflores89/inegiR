#' INEGI Catalog
#'
#' A dataset containing some INEGI codes from the most common data requests. 
#' This has been collected by manually searching the BIE Website in INEGI, as no official catalog exists.
#' @format A data frame with 54 rows and 10 variables:
#' \describe{
#'   \item{NAME}{Name of data indicador, possibly in spanish}
#'   \item{LEVEL_2}{Level 2 of desagregation}
#'   \item{LEVEL_3}{Level 3 of desagregation}
#'   \item{LEVEL_4}{Level 4 of desagregation}
#'   \item{UNITS}{Units}
#'   \item{BASE}{If they are constant units this is the base year}
#'   \item{FREQUENCY}{Frequency}
#'   \item{INEGI_CODE}{Numeric INEGI code}
#'   \item{GROUP}{Group of codes, manually updated}
#'   \item{INEGI_SERIES}{Series in the form of URL to pass to inegi_series()}
#' }
#' @source INEGI. Accesed Jan 2018.
#' @author Eduardo Flores 
#' @return data.frame 
#' @name inegi_catalog
"inegi_catalog"