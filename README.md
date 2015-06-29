# inegiR
_______
Paquete de R para interactuar con dos API's de datos de INEGI (Instituto de estadísticas oficiales de México). Las dos funciones principales, `Serie_Inegi()` y `Denue_Inegi()` son *wrappers* de los paquetes "xml", "zoo" y transformaciones usando "plyr". 
- `Serie_Inegi()` - Obtiene una serie de datos del API de indicadores, parsea y retorna un data.frame con valores y fechas o una lista con un data.frame y valores de metadatos.
- `Denue_Inegi()` - Obtiene negocios registrados en el DENUE (Registro de unidades económicas) en un radio específico de coordenadas. 

A grandes razgos, el paquete se divide en 4:
- Funciones básicas (las dos previamente descritas).
- Funciones auxiliares. Por ejemplo, `YoY()`para calcular cambios anuales.
- Funciones *wrappers* para simplificar algunas tareas comunes sin necesidad de consultar directamente la documentación del INEGI. Por ejemplo `Tasa_PIB()` para obtener la tasa de crecimiento del PIB.
- `Catalogo_Inegi()` data set que facilita encontrar la liga a las series económicas. Esto **no** es una lista exhaustiva, por lo que aún así se recomienda revisar directamente la documentación del API en el sitio oficial.

_________
__________

# inegiR (English)
R Package to interact with two API's from INEGI (Oficial statistics agency of Mexico). Both main functions `Serie_Inegi()` and `Denue_Inegi()` are wrappers of functions in packages "xml", "zoo" and some tidy data transformations using "plyr"
- `Serie_Inegi()` - Queries a data series from the INEGI API and returns the values with date and metadata in a data.frame object or a list with data.frame and corresponding metadata information.
- `Denue_Inegi()` - Queries the DENUE API (National economic unit database) and returns businesses in a circle around a given coordinate

The entire package can be summarized in 4:
- Basic query functions (previously described).
- Auxiliary functions. For example, `YoY()`to calculate Year-Over-Year changes.
- *Wrapper* functions to simplify some common tasks without further documentation. For example `Tasa_PIB()` to get Year-over-year growth rate for GDP. 
- `Catalogo_Inegi()` dataset intended to help the user find common series urls easier than the official INEGI documentation. This is **not** a complete listing of all available data series. For a more experienced user, taking a look at the oficial documentation is still the best option. 
