\name{inegiR-package}
\alias{inegiR-package}
\alias{inegiR}
\docType{package}
\title{
Funciones para API de INEGI
}
\description{
Funciones para obtener, interactuar y gráficar datos del Instituto Nacional de Estadística y Geografía (México) via el servicio web de SDMX. La implementación se basa en wrappers de xml, rcurl y rsdmx. 
}
\details{
\tabular{ll}{
Package: \tab inegiR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-06-19\cr
}
Para obtener una serie, la función más primitiva es Serie_Inegi()
}
\author{
Eduardo Flores
<eduardo@enelmargen.org>
}
\references{
Se puede encontrar más referencias sobre el API del INEGI, incluyendo generación de un token personal y rutas a más indicadores aquí: 
http://www.inegi.org.mx/desarrolladores/indicadores/apiindicadores.aspx
}
\keyword{ package }
\examples{
#traer el PIB real y guardar en un data frame
PIB<-Serie_Inegi(PIB_real)
}
