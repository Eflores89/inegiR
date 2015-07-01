# inegiR
_______
Paquete de R para interactuar con dos API's de datos de INEGI (Instituto de estadísticas oficiales de México). Las dos funciones principales, `Serie_Inegi()` y `Denue_Inegi()` son *wrappers* de los paquetes "xml", "zoo" y transformaciones usando "plyr". 
- `Serie_Inegi()` - Obtiene una serie de datos del API de indicadores, parsea y retorna un data.frame con valores y fechas o una lista con un data.frame y valores de metadatos.
- `Denue_Inegi()` - Obtiene negocios registrados en el DENUE (Registro de unidades económicas) en un radio específico de coordenadas. 

A grandes razgos, el paquete se divide en 3:
- Funciones básicas (las dos previamente descritas).
- Funciones auxiliares. Por ejemplo, `YoY()`para calcular cambios anuales.
- Funciones *wrappers* para simplificar algunas tareas comunes sin necesidad de consultar directamente la documentación del INEGI. Por ejemplo `Tasa_PIB()` para obtener la tasa de crecimiento del PIB.

_________
__________

# inegiR (English)
R Package to interact with two API's from INEGI (Oficial statistics agency of Mexico). Both main functions `Serie_Inegi()` and `Denue_Inegi()` are wrappers of functions in packages "xml", "zoo" and some tidy data transformations using "plyr"
- `Serie_Inegi()` - Queries a data series from the INEGI API and returns the values with date and metadata in a data.frame object or a list with data.frame and corresponding metadata information.
- `Denue_Inegi()` - Queries the DENUE API (National economic unit database) and returns businesses in a circle around a given coordinate

The entire package can be summarized in 3:
- Basic query functions (previously described).
- Auxiliary functions. For example, `YoY()`to calculate Year-Over-Year changes.
- *Wrapper* functions to simplify some common tasks without further documentation. For example `Tasa_PIB()` to get Year-over-year growth rate for GDP. 

___________
___________

#Ejemplos
### Instalación
Obtenemos el paquete desde github, usando devtools:
```{r}
#install.packages("devtools")
library(devtools)
install_github("Eflores89/inegiR")
  #Paquetes dependientes: 
    library(zoo)
    library(XML)
    library(plyr)
library(inegiR)
```
### Series Económicas
Para traer la inflación general, existen dos métodos (el más general y el *shortcut* implementado mediante una función *wrapper*).

Primero, el más general, que sirve para todas las series...

Se debe obtener un token del API del INEGI que se otorga mediante un registro gratis de tu correo en esta liga: http://www.inegi.org.mx/desarrolladores/indicadores/apiindicadores.aspx. *En esa liga también están todos los indicadores y su url correspondiente*
```{r}
token<-"abc123"
```
La inflación no es más que el cambio porcentual en el Indice Nacional de Precios al Consumidor (INPC), por lo que primero debemos obtener este indicador. En el sitio del API del INEGI encontramos la serie en XML:
```{r}
urlINPC<-"http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
#Obtener datos
INPC<-Serie_Inegi(urlINPC,token)
#ver datos
tail(INPC)
# Fechas         Valores
# 2014-12-01   116.05900000
# 2015-01-01   115.95400000
# 2015-02-01   116.17400000
# 2015-03-01   116.64700000
# 2015-04-01   116.34500000
# 2015-05-01   115.76400000

```
Si queremos saber todos los metadatos (Nombre de serie, última actualización, región, unidad, número de indicador y frecuencia), retornamos una lista con el argumento `metadata = TRUE`.
```{r}
INPC_Metadata<-Serie_Inegi(urlINPC,token, metadata = TRUE)
class(INPC_Metadata)
# [1] "list"
```
Si queremos acceder a uno de estos datos: 
```{r}
INPC_Metadata$MetaData$UltimaActualizacion
[1] "2015/06/09"
```
Regresando al ejemplo de la inflación, para obtener el cambio porcentual anual, aplicamos la función de `YoY()`
```{r}
Inflacion<-YoY(INPC$Valores,12,decimal=FALSE)
# a df con fechas
Inflacion<-cbind.data.frame(Fechas = INPC$Fechas, Inflacion = Inflacion)
#
tail(Inflacion)
# Fechas        Inflacion
# 2014-12-01    4.081322
# 2015-01-01    3.065642
# 2015-02-01    3.000266
# 2015-03-01    3.137075
# 2015-04-01    3.062327
# 2015-05-01    2.876643
```
El método más directo es usando una función *wrapper*. Estas funciones incluyen la serie y transformaciones para los indicadores más comunes.
```{r}
Inflacion_directa<-Inflacion_General(token)
tail(Inflacion_directa)
# Fechas        Inflacion
# 2014-12-01    4.081322
# 2015-01-01    3.065642
# 2015-02-01    3.000266
# 2015-03-01    3.137075
# 2015-04-01    3.062327
# 2015-05-01    2.876643
```
### Negocios del DENUE
El DENUE es un directorio nacional de unidades económicas recopilado por el INEGI. Es lejos de ser perfecto, pero aún así interesante.

Asumiendo que ya tenemos instalado el paquete, debemos obtener otro token diferente al usado en el API de indicadores, que se encuentra aquí: http://www.inegi.org.mx/desarrolladores/denue/apidenue.aspx.

La función `Denue_Inegi()`, trae un data.frame con 18 campos del DENUE (en la documentación en el sitio del INEGI se pueden ver a detalle).

Para obtener todos los negocios a 250 metros a la redonda de la Macro Plaza, en Monterrey, solamente necesitamos el token y las coordenadas del sitio:
```{r}
token<-"123abc"
latitud_macro<-"25.669194"
longitud_macro<-"-100.309901"
#Obtener negocios
NegociosMacro<-Denue_Inegi(latitud = latitud_macro, longitud = longitud_macro, token)
#ver solo las primeras dos columnas y 6 observaciones
head(NegociosMacro)[,1:2]
#     id                                       Nombre
# 2918696                   ESTACIONAMIENTO GRAN PLAZA
# 2918698             TEATRO DE LA CIUDAD DE MONTERREY
# 2918723                           CONGRESO DE ESTADO
# 2918793               SECRETARIA DE SALUD DEL ESTADO
# 2974150                           BIBLIOTECA CENTRAL
# 2974215      SOTANO RECURSOS HUMANOS Y ADQUISICIONES
```
La función acepta cambios en el tamaño de la redonda mediante el argumento `metros = 250` y en el tipo de establecimiento, haciendo un match en la descripción del negocio, cambiando el argumento `keyword = todos`.

#Desarrollo a Futuro
Bienvenidas las sugerencias, issues o reporte de bugs, así como extensiones en funcionalidad en: eduardo@enelmargen.org.
