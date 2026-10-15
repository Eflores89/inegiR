# inegiR 3.1.0 submission-candidate verification

Checked on 2026-10-14 (America/Mexico_City), Ubuntu 26.04.1 LTS,
x86_64-pc-linux-gnu, R 4.6.1. This is a contribution for registered-maintainer
review, not a claim of CRAN acceptance or a maintainer transfer.

## Current results

| Check | Result |
| --- | --- |
| Complete offline test suite | 760 assertions, zero failures/warnings/skips |
| Source build and package installation | Passed |
| Full R CMD check --as-cran | Zero errors, zero warnings, one incoming feasibility note |
| PDF manual | Passed |
| HTML manual validated with HTML Tidy 5.8.0 | Passed |
| R code analysis, examples, tests, Rd and dependency checks | Passed |
| Incoming metadata and remote URL checks | Enabled; passed except archive/reinstatement status |
| Existing published CI, commit 3cfd319 | Linux, Windows, macOS release-R jobs passed |

The candidate artifact is `dist/inegiR_3.1.0.tar.gz`. Its package contents exclude
internal plans, tool state, distribution files, repository guides and workflows.
The guides remain available in GitHub source; installed Rd help covers every
export. Original author/registered-maintainer identity and CC0 licensing are
retained, with Arturo Cardenas's existing contributor credit represented in
Authors@R. R 4.0 is the minimum required by the current HTTP dependency.

The source check log and test transcript are available in the contribution's
`docs/check-results/` directory. The candidate's GitHub CI status is shown at
https://github.com/jadrk040507/inegiR/actions . The revised workflow checks
release R on Linux, Windows and macOS plus Linux R-devel, treating any code-check
note as a failure. Those jobs disable CRAN incoming checks to isolate portable
package checks. A separate Linux job enables real incoming/remote checks,
installs HTML Tidy and TinyTeX, builds the PDF/HTML manuals, retains all results,
and fails on errors or warnings. Its archive-related incoming note remains
visible for registered-maintainer review.

## Resolved diagnostics

- Six DENUE type guards compared class() to a single string. They now use
  is.numeric(), accepting integers and classed numeric inputs. New public-entry
  regressions cover all six guards, complete mocked statistics/grid behavior,
  quiet errors, and credential-safe condition calls. This is a targeted guard
  repair; live DENUE responses have not been validated.
- HTML Tidy was missing locally. Tidy 5.8.0 and its library were extracted into a
  temporary directory without changing system packages. Full HTML validation
  now passes; the diagnostic was not suppressed. CI installs the same tool.
- Incoming checks found the development version, missing Authors@R, and an old
  redirected article URL. Version 3.1.0 is a submission candidate; structured
  authorship preserves existing credits. The old article is unavailable even
  at its HTTPS redirect, so its documentation now links the pinned original
  implementation and states that historical index weights are unvalidated.

## Remaining incoming note

The full check reports only:

    New submission
    Package was archived on CRAN

CRAN's database records archiving on 2026-06-01 because issues were not corrected
despite reminders. This repository cannot change that record. The registered
maintainer must review the patch and request reinstatement, or a maintainer
transfer must be agreed with CRAN. `cran-comments.md` provides the technical
resubmission evidence. No submission or maintainer consent is claimed.

See https://CRAN.R-project.org/package=inegiR and
https://cran.r-project.org/web/packages/policies.html .

## Reproduce

Install DESCRIPTION dependencies, HTML Tidy >= 5.0, and a working LaTeX setup.
The local temporary tibbletime library was /tmp/inegir-r-library. Build into a
separate temporary directory, then run:

```sh
Rscript -e 'roxygen2::roxygenise(); testthat::test_local()'
R CMD build /path/to/inegiR
_R_CHECK_CRAN_INCOMING_=true _R_CHECK_CRAN_INCOMING_REMOTE_=true \
  R CMD check --as-cran inegiR_3.1.0.tar.gz
```

If Tidy is outside PATH, set R_TIDYCMD to its executable and configure its
shared-library path as needed. The local check used the actual Tidy executable,
real CRAN repository/network lookups without offline repository overrides. Incoming checks,
PDF generation, HTML validation and suggested dependencies were enabled.

## Limits

Observation and DENUE regression tests use synthetic fixtures and mocked HTTP.
Authenticated live INEGI behavior, current economic interpretation of legacy
IDs/weights, and unrelated routing/mnemonic services remain unverified. This
check is technical submission preparation; CRAN and the registered maintainer
make the acceptance and reinstatement decisions.

Final source artifact SHA-256: `ce0479ec71a736ad13381523f71587b5a6ae580f3532694e146339424752e818`.
