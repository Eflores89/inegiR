# Upstream repair baseline

This local repair is based on Eduardo Flores's [Eflores89/inegiR](https://github.com/Eflores89/inegiR)
at commit [`64d33221507cce2f86151a96fffc3283526a49cf`](https://github.com/Eflores89/inegiR/commit/64d33221507cce2f86151a96fffc3283526a49cf)
(2024-01-25, `Update README.md`). The commit's complete recursive tree is
`74be1b1535fd26ad151be8eed7707e7fc314e43b`.

The snapshot was fetched through the GitHub connector on 2026-10-14 (Mexico City)
using the pinned commit for every file. All 40 upstream files were imported,
including all 13 R source files, 20 Rd manuals, DESCRIPTION, NAMESPACE, README,
and project/build metadata. The complete tree contains no AGENTS.md. Existing
workspace design documents were retained. The protected `.git` placeholder was
left unchanged; this import is a source snapshot, without Git history.

Upstream declares `License: CC0` in DESCRIPTION and has no separate license
file. That declaration, Eduardo Flores's author and maintainer metadata, and source
author credits are retained. User guides and generated help are updated
for this local repair; original documentation remains in the pinned snapshot. The baseline exports
28 functions; NAMESPACE was imported unchanged before the six local exports. The original file contents and
Git blob identifiers are preserved for comparison in
`.superpowers/sdd/inegir/upstream-snapshot.json`.

Local development version **3.0.0.9000** adds canonical observations, latest,
metadata/catalogs, and session token helpers. The shared indicator/catalog
transport validates inputs, uses HTTPS with 30-second timeouts and at most three
attempts, rejects redirects, and returns safe package conditions. Legacy output
shapes and positional arguments remain. Complete raw catalog records are kept
in a metadata list column. See [NEWS](../NEWS.md) for deliberate behavior repairs,
including period decoding, requested multi-series order, null handling,
redacted request provenance, and semantic correction of the topic catalog.

Canonical dates represent validated interval starts. Exact frequency matching
repairs codes such as 10/18 being mistaken for annual/monthly data. Unknown or
ambiguous periods retain raw text with missing dates; bounded calls reject
undated responses. Legacy unsupported periods pass through with a warning, and
as_tt requires unambiguous Date output. Annual legacy character years and
quarterly/monthly Date displays remain.

The local source artifact is a development build, not a released or upstream
accepted version. The upstream author/maintainer fields identify retained
provenance and do not assert that Eduardo Flores reviewed these local changes.
No Git history was replaced in this workspace. Build exclusions remove internal
plans, tool placeholders, and generated distribution artifacts.

Tests use synthetic fixtures and mocked HTTP without real tokens. Authenticated
live verification, current economic equivalence of shortcut IDs, and unrelated
DENUE, SAKBE/routing, mnemonic XML, and derived-indicator implementations remain
unverified. Their exports are retained; this repair does not attest to their
runtime behavior or credential handling. Package check is not a live-service
validation. Detailed results are recorded in [verification](verification.md).

The submission-preparation follow-up uses candidate version **3.1.0**. It fixes
six DENUE coordinate guards with offline regressions, preserves the registered
maintainer through Authors@R, reflects the HTTP dependency's R 4.0 minimum, and
replaces an unavailable historical documentation link. This candidate has not
been submitted or accepted by CRAN. See the current verification report for
full network/incoming checks and the archived-package reinstatement note.
