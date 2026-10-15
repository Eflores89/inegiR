## Local preparation only

This is an unreleased development repair (3.0.0.9000), prepared for review by the
registered maintainer. It has not been submitted to CRAN. Eduardo Flores’s
upstream authorship, maintainer metadata, and CC0 declaration are retained;
those fields do not imply review or endorsement of these local changes.

## Checks and scope

Local Linux (Ubuntu 26.04.1, R 4.6.1): the initial R CMD check --no-manual
was OK, 0 errors, 0 warnings, 0 notes. A stronger controller --as-cran run,
including the PDF manual, had 0 errors, 0 warnings, 3 notes. One note concerned
nonstandard docs and README.es.md, now excluded from package builds while kept
in the repository. The other notes concern six inherited DENUE class() string
comparisons (runtime unchanged outside this repair) and missing HTML tidy in
the check environment. The documentation/build-exclusion revision awaits the
controller's final --as-cran rerun; no revised count is asserted here. All 673 testthat assertions passed with no warnings
or skips. Source build, installation, and namespace/token/mock-data smoke passed.
Detailed evidence is recorded in docs/verification.md. GitHub Actions is configured for release R on Linux,
Windows, and macOS, but no remote platform result is claimed. Network and
incoming CRAN checks are disabled for the local R CMD check run.

Indicator/catalog tests use synthetic JSON and mocked HTTP, with no real token
or external API request. Authenticated live INEGI behavior, current economic
meaning of legacy indicator IDs, and unrelated DENUE, SAKBE/routing, mnemonic
XML, and derived services remain unverified. Network help examples use dontrun.

## Final contributor rerun

The final artifact was rechecked after named-input and warning-call redaction
regressions were repaired. Full local Linux `R CMD check --as-cran`, including
PDF manual generation: 0 errors, 0 warnings, 2 notes. Notes concern unchanged
DENUE class comparisons and unavailable HTML Tidy. Incoming/remote checks and
repository lookups were disabled. All 703 assertions pass without failures,
warnings or skips. Independent broad review and scoped fix re-review approved
the contributor fork/PR. Authenticated live service behavior remains unverified.
