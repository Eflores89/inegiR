# inegiR — submission candidate

inegiR retrieves INEGI indicator data through the authenticated JSON API v2.0.
This contribution prepares version **3.1.0**, a submission candidate repairing
[Eduardo Flores’s inegiR](https://github.com/Eflores89/inegiR), pinned to
[`64d33221507cce2f86151a96fffc3283526a49cf`](https://github.com/Eflores89/inegiR/commit/64d33221507cce2f86151a96fffc3283526a49cf).
Eduardo Flores’s authorship and the upstream **CC0** license are retained.
This artifact has not been published to CRAN or accepted upstream.
See [baseline and scope](docs/upstream-baseline.md), [changes](NEWS.md), and
[verification](docs/verification.md). [Inicio en español](README.es.md).

## Install this repair

Install the community contribution from the personal GitHub fork:

```r
install.packages("remotes")
remotes::install_github("jadrk040507/inegiR")
```

This fork is a community contribution and does not imply upstream acceptance.
For a local build, install the supplied source package from this workspace
(with its dependencies already installed):

```r
install.packages("dist/inegiR_3.1.0.tar.gz", repos = NULL, type = "source")
library(inegiR)
```

Installing from CRAN or the upstream GitHub repository does not install these
local changes. Indicator access uses the API; it does not scrape HTML pages.
Legacy service entry points remain available. DENUE grid/statistics coordinate
guards now accept integer and classed numeric inputs; their live responses
remain unverified.

## Token and quick start

Register for an indicator API token through
[INEGI’s API page](https://www.inegi.org.mx/servicios/api_indicadores.html).
For one R session, call `inegi_set_token("your-token")`; it sets `INEGI_TOKEN`
invisibly and writes no file. To persist it yourself, add this line to your
personal `.Renviron`, then restart R:

```text
INEGI_TOKEN=your-token
```

Keep that file private and outside version control. `inegi_get_token()` returns
the configured token; calling it at the console displays the returned value.
The helpers never print credentials as a side effect. A missing or blank token
raises `inegi_input_error`.

The following calls require your token and network access. IDs are examples
from legacy documentation; check their current definition and bank with INEGI.

```r
x <- inegi_data(c("216064", "216097"),
  start_date = "2020-01-01", end_date = "2025-12-31",
  language = "es", geography = "00", database = "BIE")
latest <- inegi_latest("216064")
meta <- inegi_metadata(unique(x$series_id))
freq <- inegi_catalog("CL_FREQ")

# Explicit title join that preserves observation order and avoids suffixes.
# Use the same database and language for data and metadata.
x$title <- meta$title[match(x$series_id, meta$series_id)]
# Complete CL_INDICATOR records, including provider-specific fields:
meta$raw_metadata[[1L]]
```

`series` accepts digit strings or safe positive integer IDs. Canonical calls
remove duplicate IDs in first-seen order, preserve provider row order within
each requested series, and fail atomically on an invalid response or failed
request. `language` accepts `"es"` and `"en"`; `database` accepts `"BIE"` and
`"BISE"`. Geography is text so codes such as `"00"` retain leading zeros.

## Data you receive

The first seven columns match the shared banxicoR convention:

| Column | R type | Meaning |
| --- | --- | --- |
| provider | character | `"inegi"` |
| series_id | character | Indicator ID |
| title | character | Response title, or `NA` when absent |
| period | character | Original API period text |
| date | Date | Validated interval start, or `NA` |
| value | double | Numeric observation, or `NA_real_` |
| value_raw | character | Original value text; JSON null becomes `NA_character_` |

Additional character columns retain geography, frequency, unit,
unit_multiplier, topic, source, note, last_updated, series_status, obs_status,
obs_exception, obs_source, obs_note, database, and language. Empty responses
return zero rows with the same types. No hidden metadata lookup fills titles.
`inegi_metadata()` explicitly retrieves CL_INDICATOR and rejects missing,
duplicate, or unrequested response IDs. Its `raw_metadata` list column retains
each complete parsed provider record. Catalog results have character `catalog`,
`id`, `description`, `database`, and `language` columns.

Raw periods, values, flags, and codes remain available. Missing or nonnumeric
values remain rows with numeric `NA`; unexpected nonnumeric values without an
exception flag warn. Values are never interpolated or rescaled by UNIT_MULT.
Annual frequency codes 1/2/3 map `YYYY` or `YYYY/01` to January 1; quarterly code
6 maps `YYYY/01`–`YYYY/04` to quarter starts; monthly code 8 maps
`YYYY/01`–`YYYY/12` to month starts. Unknown, invalid, or ambiguous periods,
including biweekly `YYYY/MM`, retain their text and receive missing dates.
Date bounds are inclusive, accept scalar `Date` or valid ISO `YYYY-MM-DD`
strings, and filter interval starts locally. A bounded response containing any
undated period errors; request without bounds to inspect those periods.
`inegi_latest()` uses the API’s recent-observation flag.

## Existing code and paired providers

All 28 baseline exports remain. Existing `inegi_series(series_id, token, ...)`
and `inegi_series_multiple(series_id, token, names, ...)` retain positional
arguments and frame/list/compact schemas; both now default to `INEGI_TOKEN`.
Legacy series calls use English; legacy catalogs use English/BIE and still
require their `token` argument. The legacy frame has `date`, `date_shortcut`,
`values`, and `notes`; metadata lists have `metadata` then `data`.
Supported annual character-year and quarterly/monthly Date displays remain.
Unsupported periods pass through with a warning; `as_tt = TRUE` requires valid,
unambiguous Date output. Request URLs in `call_unmasked` and `meta_call` are
redacted. Multiple-series output now follows requested order. `incat_topic()`
now places descriptions in `topic_description` and IDs in `topic`, repairing the
upstream swap while retaining column order. Shortcut IDs are preserved; their
current economic meaning has not been verified.

```r
old_shape <- inegi_series(series_id = "216064")
old_multi <- inegi_series_multiple(c("216064", "216097"),
  names = c("series_a", "series_b"))

# With both local packages installed and both provider tokens configured:
i <- inegiR::inegi_data("216064", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
b <- banxicoR::banxico_data("SF43718", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
common <- c("provider", "series_id", "title", "period", "date", "value", "value_raw")
combined <- rbind(i[common], b[common])
inegiR::inegi_latest("216064")
banxicoR::banxico_latest("SF43718")
inegiR::inegi_metadata("216064")
banxicoR::banxico_metadata("SF43718")
```

The shared interface does not imply equivalent indicators, frequencies, units,
metadata layouts, or ordering. See the [shared contract](docs/shared-interface.md).

## Verification limits

The tests use synthetic JSON, mocked HTTP, and synthetic tokens. They cover the
indicator/catalog repair without external API access. Authenticated live INEGI
behavior has **not** been verified. DENUE, SAKBE/routing, mnemonic XML, derived
indicators, and current economic definitions are outside this repair’s validation
scope. Build, install, test, and check results are in [verification](docs/verification.md).
