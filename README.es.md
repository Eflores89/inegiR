# inegiR — reparación local en desarrollo

Esta versión **3.0.0.9000** repara el acceso a indicadores de INEGI por su API
JSON v2.0. Parte de [inegiR de Eduardo Flores](https://github.com/Eflores89/inegiR),
commit [`64d33221507cce2f86151a96fffc3283526a49cf`](https://github.com/Eflores89/inegiR/commit/64d33221507cce2f86151a96fffc3283526a49cf).
Conserva su autoría y licencia **CC0**. Es un artefacto local: no se ha publicado
en CRAN ni aceptado en el proyecto original. [English](README.md).

## Instalación y token

Instala la contribución comunitaria desde el fork personal de GitHub:

```r
install.packages("remotes")
remotes::install_github("jadrk040507/inegiR")
```

El fork es una contribución comunitaria y no implica aceptación por el proyecto
original. También puedes instalar el artefacto local desde esta carpeta,
con las dependencias instaladas:

```r
install.packages("dist/inegiR_3.0.0.9000.tar.gz", repos = NULL, type = "source")
library(inegiR)
```

Obtén un token en la [página de la API de INEGI](https://www.inegi.org.mx/servicios/api_indicadores.html).
Para la sesión actual usa `inegi_set_token("tu-token")`: configura `INEGI_TOKEN`
sin imprimirlo ni guardarlo en disco. Si quieres conservarlo entre sesiones,
agrega por tu cuenta `INEGI_TOKEN=tu-token` a tu archivo personal `.Renviron`
y reinicia R. Mantén ese archivo privado y fuera del control de versiones.
`inegi_get_token()` devuelve el token y, si lo ejecutas en la consola, R mostrará
ese valor. Un token ausente o vacío produce `inegi_input_error`.

## Inicio rápido

Estos ejemplos requieren token y conexión. Los identificadores provienen de la
documentación anterior; comprueba su definición actual y banco en INEGI.

```r
x <- inegi_data(c("216064", "216097"),
  start_date = "2020-01-01", end_date = "2025-12-31",
  language = "es", geography = "00", database = "BIE")
ultimo <- inegi_latest("216064")
meta <- inegi_metadata(unique(x$series_id))
frecuencias <- inegi_catalog("CL_FREQ")

# Consulta explícita de títulos; conserva el orden de las observaciones.
# Usa el mismo banco e idioma en ambas consultas.
x$title <- meta$title[match(x$series_id, meta$series_id)]
meta$raw_metadata[[1L]]  # Registro completo del proveedor
```

`series` admite identificadores como texto numérico o enteros positivos seguros.
Los métodos nuevos eliminan duplicados conservando la primera aparición y el
orden de las filas del proveedor dentro de cada serie. Idiomas: `"es"` y `"en"`;
bancos: `"BIE"` y `"BISE"`. La geografía se pasa como texto para conservar ceros
iniciales como `"00"`. Las consultas fallan completas ante errores de respuesta.

Las primeras siete columnas son `provider`, `series_id`, `title`, `period`,
`date`, `value` y `value_raw`. `date` tiene clase Date y `value` es double;
las demás son character. `provider` vale `"inegi"`. Sin título en la respuesta,
`title` es `NA`; no se consulta metadata en segundo plano. Las columnas
adicionales conservan geografía, frecuencia, unidad, multiplicador, fuentes,
notas, estados, excepciones, banco e idioma. Las respuestas vacías conservan
las columnas y sus tipos.

`period` y `value_raw` conservan el texto original. Los valores ausentes o no
numéricos permanecen en las filas con `value = NA_real_`; un valor inesperado
sin bandera de excepción genera advertencia. Un null de JSON se representa
como `NA_character_` en `value_raw`. No se interpolan ni se reescalan valores.
Las fechas representan el **inicio del intervalo**: enero 1 para años, inicio
del trimestre o del mes para frecuencias reconocidas. Periodos desconocidos,
inválidos o quincenales ambiguos conservan el texto con fecha `NA`.
Los límites aceptan un Date o texto ISO `YYYY-MM-DD` y filtran fechas de inicio
de forma inclusiva. Si una respuesta acotada contiene cualquier periodo sin
fecha, se produce un error; consulta sin límites para examinarlo.
`inegi_latest()` solicita la bandera de observación reciente de la API.

`inegi_metadata()` consulta CL_INDICATOR explícitamente: devuelve `provider`,
`series_id`, `title`, `database`, `language` y una columna lista `raw_metadata`
con los registros completos. Rechaza IDs faltantes, repetidos o no solicitados.
`inegi_catalog()` devuelve `catalog`, `id`, `description`, `database`, `language`.

## Compatibilidad y banxicoR

Se conservan las 28 funciones exportadas originales y los argumentos
posicionales de `inegi_series()` e `inegi_series_multiple()`. Ambas aceptan el
token de entorno por omisión. Las series antiguas usan inglés; los catálogos
antiguos usan inglés/BIE y aún requieren `token`. Se mantienen los formatos
frame/lista/compacto y las columnas `date`, `date_shortcut`, `values`, `notes`.
Las URLs de metadata se redactan. Los periodos no admitidos se devuelven como
texto con advertencia; `as_tt = TRUE` requiere fechas válidas y no ambiguas.
La salida múltiple sigue ahora el orden solicitado. `incat_topic()` coloca la
descripción en `topic_description` y el código en `topic`, corrigiendo la
inversión anterior sin cambiar el orden de columnas. Los IDs de los atajos
se conservan, pero no se verificó su interpretación económica actual.

```r
anterior <- inegi_series(series_id = "216064")
# Con ambos paquetes locales instalados y ambos tokens configurados:
i <- inegiR::inegi_data("216064", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
b <- banxicoR::banxico_data("SF43718", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
inegiR::inegi_latest("216064")
banxicoR::banxico_latest("SF43718")
inegiR::inegi_metadata("216064")
banxicoR::banxico_metadata("SF43718")
```

Los métodos comparten argumentos y siete columnas, sin implicar equivalencia de
unidades, indicadores, metadata u orden. Consulta el [contrato compartido](docs/shared-interface.md).

## Alcance de la verificación

El acceso a indicadores usa la API, sin extraer páginas HTML. Las pruebas son
sin conexión, con JSON y tokens sintéticos. **No se verificó el funcionamiento
autenticado contra INEGI en vivo.** DENUE, SAKBE/rutas, XML de nemónicos,
indicadores derivados y definiciones económicas actuales quedan fuera de esta
validación. Consulta [cambios](NEWS.md), [procedencia](docs/upstream-baseline.md)
y [resultados de verificación](docs/verification.md).
