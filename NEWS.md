# inegiR 3.0.0.9000 (local development)

This is an unreleased repair of Eduardo Flores’s CC0 package, pinned to
Eflores89/inegiR commit 64d33221507cce2f86151a96fffc3283526a49cf. It does not claim
CRAN publication or upstream acceptance. All 28 upstream exports remain.

- Add `inegi_data()`, `inegi_latest()`, `inegi_metadata()`, `inegi_catalog()`,
  `inegi_set_token()`, and `inegi_get_token()`. Shared verbs, argument order,
  and seven observation columns align with the local banxicoR contract.
- Use validated HTTPS JSON requests for indicators and the five legacy catalog
  adapters; reject redirects, use 30-second timeouts, and attempt at most three
  times for rate limits and transient server errors. Validate IDs, language,
  bank, geography, flags, and dates before requesting data. Fail combined calls
  atomically. Classify input, authentication, HTTP, connection, and response errors.
- Redact credentials in legacy call_unmasked/meta_call provenance and avoid
  unsafe HTTP objects or server response bodies in package conditions. Session
  token helpers use INEGI_TOKEN, with no disk persistence or print side effects.
- Preserve raw periods, values, flags, codes, and UNIT_MULT without rescaling;
  retain missing observations as rows. Empty results keep typed columns.
- Decode frequencies exactly instead of substring matching. Canonical dates
  represent interval starts. Unknown, malformed, and ambiguous biweekly periods
  remain undated with original text; inclusive bounds reject undated responses.
  Unsupported legacy dates pass through with warnings; as_tt requires valid,
  unambiguous Date output. Supported legacy displays remain.
- Keep legacy frame/list/compact schemas and argument order. Repair optional
  null metadata/notes, zero-row compact frames, reversed multiple-series order,
  unsafe empty/mismatched inputs, and partial argument matching in shortcuts.
  Shortcut IDs are unchanged; their current economic meaning is unverified.
- Keep legacy catalog names, schemas, and English/BIE defaults. Correct the
  upstream incat_topic() ID/description swap: topic_description is now the
  description, topic is the ID; column order stays unchanged. This intentionally
  changes values for callers relying on the mislabeled upstream fields.
- Retain complete CL_INDICATOR records in the metadata raw_metadata list column;
  reject missing, duplicate, wrong-ID, or malformed metadata/catalog responses.
- Add English/Spanish quick starts, shared-provider and compatibility guidance,
  mocked offline tests, regenerated help, and source-package verification.
  Namespace-qualify stats::setNames and utils::tail for namespace-only loading. No authenticated
  live verification or validation of unrelated legacy services is claimed.
