## Submission candidate for registered-maintainer review

Version 3.1.0 restores the archived package's indicators API paths and adds
consistent data/metadata/token helpers. It preserves Eduardo Flores as the
registered maintainer, credits original contributions through Authors@R, and
retains CC0. This contribution has not been submitted and does not claim
maintainership transfer or approval from the registered maintainer.

## Checks

Ubuntu 26.04.1, R 4.6.1: full R CMD check --as-cran with incoming and remote
checks enabled, all suggested dependencies available, PDF generation, and
HTML Tidy 5.8.0 validation: 0 errors, 0 warnings, 1 incoming NOTE.
All 760 offline assertions pass with no warnings or skips.

The sole incoming NOTE is "New submission / Package was archived on CRAN".
CRAN records archiving on 2026-06-01 because issues were not corrected despite
reminders. The registered maintainer should confirm the resubmission contact
and request reinstatement. We cannot remove CRAN's archive record in code.
The previous DENUE class-comparison and missing-Tidy notes are resolved;
release-version, Authors@R, and URL findings from real incoming checks are fixed.

The earlier contribution passed release-R CI on Linux, Windows, and macOS.
Candidate CI adds strict CRAN-style portable checks, Linux R-devel, and a
full-manual/incoming job; inspect current commit results before submission.
Logs and detailed reproduction guidance are in docs/verification.md and
repository docs/check-results/ (excluded from the source tarball).

Authenticated live API behavior and current economic meanings of legacy IDs
and weights remain unverified. Automated tests mock external services; network
examples are enclosed in dontrun. No release or upstream acceptance is claimed.
