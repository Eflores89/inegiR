# Shared inegiR / banxicoR interface

The local development packages share a small public convention. This contract
was checked against the banxicoR workspace’s `R/api.R`, `R/parse.R`, and
`tests/testthat/test-shared-contract.R` on 2026-10-14, plus the coordinated design
and implementation reports. This records local source behavior, not a released
cross-package standard. No runtime dependency between the packages is required.

| Purpose | inegiR | banxicoR |
| --- | --- | --- |
| History | `inegi_data(series, start_date = NULL, end_date = NULL, token = inegi_get_token(), language = "es", geography = "00", database = "BIE")` | `banxico_data(series, start_date = NULL, end_date = NULL, token = banxico_get_token(), language = "es")` |
| Recent | `inegi_latest(series, token = inegi_get_token(), language = "es", geography = "00", database = "BIE")` | `banxico_latest(series, token = banxico_get_token(), language = "es")` |
| Metadata | `inegi_metadata(series, token = inegi_get_token(), language = "es", database = "BIE")` | `banxico_metadata(series, token = banxico_get_token(), language = "es")` |
| Session token | `inegi_set_token(token)` / `inegi_get_token()` | `banxico_set_token(token)` / `banxico_get_token()` |
| Environment | `INEGI_TOKEN` | `BANXICO_TOKEN` |

Argument names and order match through `language`. Provider-specific arguments
follow them. Both accept `es`/`en`, explicit metadata retrieval, inclusive Date
or ISO date bounds, and first-seen deduplication of requested series. A getter
returns a credential, so avoid printing it; setters are invisible and do not
persist it. Network examples in package help are wrapped in `\dontrun{}`.

Observation frames begin with these columns in this order:

| Column | R storage / class |
| --- | --- |
| provider | character |
| series_id | character |
| title | character |
| period | character |
| date | double / Date |
| value | double |
| value_raw | character |

Empty frames retain the schema. Original period and value text survive;
unavailable numeric observations remain rows with `NA_real_`. Titles may be
missing. Metadata begins with character `provider`, `series_id`, `title`.
Input, authentication, HTTP, connection, and response conditions inherit the
provider’s base error (`inegi_error` or `banxico_error`). No unsafe HTTP objects
are attached by the repaired paths.

## Paired calls

With both packages installed and their tokens configured, run manually:

```r
i <- inegiR::inegi_data("216064", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
b <- banxicoR::banxico_data("SF43718", start_date = "2025-01-01",
  end_date = "2025-12-31", language = "es")
inegiR::inegi_latest("216064")
banxicoR::banxico_latest("SF43718")
inegiR::inegi_metadata("216064")
banxicoR::banxico_metadata("SF43718")
core <- c("provider", "series_id", "title", "period", "date", "value", "value_raw")
combined <- rbind(i[core], b[core])
```

These example IDs identify provider records; they do not establish economic
comparability. The examples were not authenticated against either live service.

## Deliberate provider differences

- INEGI IDs are digit strings (safe positive numeric integers also accepted);
  Banxico IDs are alphanumeric strings. INEGI supports BIE/BISE and text geography.
- INEGI preserves response row order within requested series order; banxicoR
  sorts dates ascending within requested series order.
- INEGI dates are validated interval starts. Unknown frequencies and ambiguous
  biweekly text have missing dates; any undated row makes a bounded call fail.
  Banxico parses exact day/month/year observation dates and rejects invalid dates.
- INEGI always fetches history then filters bounds locally. Banxico uses its
  range endpoint for two bounds and local filtering for a single bound.
- INEGI adds 15 character provenance/metadata/flag columns after the common
  seven. `title` comes only from response TITLE; otherwise request metadata.
  Banxico observations have only the common seven and use response titulo.
- INEGI metadata appends database, language, and a `raw_metadata` list column
  containing each complete CL_INDICATOR record. Banxico appends provider fields
  as scalar or list columns. Consumers must handle provider metadata separately.
- INEGI keeps OBS_VALUE text (null becomes character NA) and parses numeric text
  directly; Banxico also recognizes its provider’s comma-grouped numbers and
  unavailable-value markers. Neither rescales or interpolates values.
- `*_latest()` uses each API’s recent endpoint/flag; the providers determine
  the meaning of “recent.” INEGI has a separate `inegi_catalog()` interface.
- Legacy functions keep their provider-specific names and schemas. Only the
  canonical methods promise this shared core.
