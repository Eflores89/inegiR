# Verification of local inegiR development repair

Version **3.0.0.9000**, checked 2026-10-14 (America/Mexico_City) on Ubuntu
26.04.1 LTS, x86_64-pc-linux-gnu, R 4.6.1. This is a local development artifact;
it has not been released to CRAN or accepted upstream.

| Verification | Result |
| --- | --- |
| Roxygen regeneration and complete testthat suite | 673 assertions; 0 failures, 0 warnings, 0 skips |
| Source tarball build | Successful |
| Install source tarball into a temporary library | Successful |
| Installed namespace/token/mock-data smoke | Passed with only base attached initially |
| Export and help coverage | All 28 baseline exports retained; 6 new exports; all 34 documented |
| Shared banxicoR contract | Argument order/defaults (provider token helper excepted), core column order/types/classes match |
| R CMD check --no-manual (before documentation-only revision) | Status: OK; 0 errors, 0 warnings, 0 notes |
| Controller R CMD check --as-cran (before guide exclusions) | 0 errors, 0 warnings, 3 notes; PDF manual OK |

The final installable artifact is `dist/inegiR_3.0.0.9000.tar.gz`. It excludes
dist itself, internal plans/reports, protected tool placeholders, Git metadata,
workflow files, and draft CRAN comments. NEWS, the English README, and regenerated
Rd help are included. The repository retains README.es.md and docs/ guides;
these are excluded from the package to avoid nonstandard top-level-file notes
under --as-cran. Installed help covers token setup and canonical/legacy APIs. Internal command transcripts are retained in the workspace
under `.superpowers/sdd/inegir/task-5-*.log`; they are excluded from the tarball.

## Reproduce locally

Install the DESCRIPTION dependencies first. The local run used
`R_LIBS=/tmp/inegir-r-library` and `TZ=America/Mexico_City` for all R commands.

```sh
Rscript -e 'roxygen2::roxygenise(); testthat::test_local()'
# From /tmp, so build artifacts stay outside the source tree:
R CMD build /path/to/inegiR
mkdir -p /tmp/inegir-installed
R CMD INSTALL --library=/tmp/inegir-installed /tmp/inegiR_3.0.0.9000.tar.gz
```

For the check run, create a temporary R profile containing
`options(repos = character())` to disable repository index lookups. From `/tmp`:

```sh
R_PROFILE_USER=/tmp/inegir-offline.Rprofile \
_R_CHECK_CRAN_INCOMING_=false _R_CHECK_CRAN_INCOMING_REMOTE_=false \
_R_CHECK_FORCE_SUGGESTS_=false _R_CHECK_SYSTEM_CLOCK_=false \
R CMD check --no-manual /tmp/inegiR_3.0.0.9000.tar.gz
```

The installed smoke run starts with `R_DEFAULT_PACKAGES=base`, checks the typed
empty schema and every installed export/help topic, exercises token validation
and the invisible setter, then mocks the HTTP boundary for canonical data/latest
requests. Only synthetic credentials and JSON are used. The shared-contract
check reads the local banxicoR API and parser source; provider differences in
ordering, dates, IDs, metadata, and transport are recorded in
[shared interface](shared-interface.md).

## Diagnostics inspected and resolved

The initial check reported one code-analysis NOTE for unqualified `tail()` in
the new validator. It is now `utils::tail()`. The empty schema uses
`stats::setNames()`, allowing namespace-only execution. Both dependencies are
declared. An initial roxygen deprecation was resolved with `_PACKAGE`-style
package documentation. Draft help and token examples were reconciled with the
actual missing-token `inegi_input_error` behavior.

Early checks attempted repository index URLs despite incoming checks being
disabled; the final profile disables those lookups. No API request was made.
The no-manual check had no unresolved package diagnostics. Its PDF manual
step was omitted; the controller subsequently verified the PDF manual in a full
`R CMD check --as-cran` run. That stronger check reported three notes:

- Nonstandard top-level docs and README.es.md: now excluded from package builds;
  both remain in the repository for readers.
- Six inherited DENUE comparisons of class() to "numeric": runtime code remains
  unchanged because this repair validates the indicator/catalog path.
- HTML manual validation skipped because the environment has no tidy command.

The full controller log is retained as
`.superpowers/sdd/inegir/task-5-controller-as-cran-before-guide-exclusions.log`.
Its 0 errors / 0 warnings / 3 notes apply to the earlier artifact. The revised
artifact changes only documentation and build exclusions; its final controller
--as-cran rerun is pending. The remaining DENUE and tidy notes are disclosed
rather than treated as new indicator/catalog failures.

## Limits and publication preparation

**Authenticated live INEGI behavior has not been verified.** Tests mock HTTP;
they cannot establish that current service payloads or example IDs match the
fixtures. Current economic meaning of shortcut IDs, DENUE, SAKBE/routing,
mnemonic XML, and derived-indicator services remain outside validation scope.

The GitHub workflow schedules release-R checks on Linux, Windows, and macOS.
Those remote jobs have not been executed as part of local validation; only the
Linux environment above is claimed. Dependency installation in CI needs network
access, while package tests need no token and mock external HTTP.

`cran-comments.md` prepares review evidence for the registered maintainer; it
is not a submission or a claim of maintainership transfer. Author/maintainer
provenance remains unchanged. Publishing and upstream review are separate from
this local artifact verification.

## Final contributor artifact verification

After the broad independent review, two additional regressions were repaired:
incidental names on R scalar inputs no longer break request/ID validation, and
legacy warnings omit calling expressions that could contain literal tokens.
Thirty new assertions cover these cases. The final controller run passed all
**703 assertions with zero failures, warnings, or skips**.

The rebuilt source artifact has SHA-256
`dba7641d87ec83d9e27d116004aafaead2d008871978f04ee2e0d2d6396c80fd`.
The controller checked that exact artifact with full `R CMD check --as-cran`,
including PDF manual generation: **0 errors, 0 warnings, 2 notes**. Incoming
and remote CRAN checks were disabled and repository lookups were disabled in
a temporary profile. The two remaining notes are the inherited DENUE
`class()` comparisons and unavailable HTML Tidy; no new indicator-path note
remains. PDF manual generation passed. This is local Linux evidence, not a
CRAN submission or cross-platform certification. Scoped independent re-review
approved both corrections for contributor publication.
