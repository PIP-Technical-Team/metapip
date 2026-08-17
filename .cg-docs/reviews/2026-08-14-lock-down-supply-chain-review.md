---
date: 2026-08-14
depth: full
type: standard
plan: .cg-docs/plans/2026-08-14-lock-down-supply-chain.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: skipped
  P3.1: fixed
  P3.2: skipped
  P3.3: fixed
  P3.4: skipped
  P3.5: skipped
  P3.6: fixed
  P3.7: fixed
  P3.8: fixed
  P3.9: fixed
  P3.10: fixed
  P3.11: skipped
  P3.12: fixed
  P3.13: skipped
---

## Review Report

**Review mode**: full (auto-routed from /cg-work review:auto — security-risk: tokens/credentials/install paths)
**Files reviewed**: 25 (R/, tests/, README, NEWS, charter, vignettes, man/)
**Findings**: 21 (P0: 0, P1: 3, P2: 5, P3: 13)

### P0 — BLOCKING (immediate remediation required)

None.

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality] `R/pip_snapshot.R:20-26,59` (+ `R/init_metapip.R:178-182`) — `pip_snapshot()`'s default `path` resolves to `""` (no `inst/` and no `inst/PIP_LOCK.csv` in the repo), and `write.csv(lock_df, "")` silently writes the CSV to stdout on Windows while emitting `Wrote PIP_LOCK to ()`; `update_pip_packages()` no-ops on the same empty path. The milestone's documented workflow ("run `pip_snapshot()` to create a team lock") produces no committable file, and `init_metapip()` can never enter the lock-driven branch (team determinism not realized).
  **Why**: `system.file("PIP_LOCK.csv", package = "metapip")` returns `""` when the file is absent; there is no `inst/` dir and no committed lock.
  **Fix**: Ship a committed `inst/PIP_LOCK.csv` (via `pip_snapshot(path=)` with network) OR abort loudly when the resolved path is `""` (never claim success silently); mirror the guard in `update_pip_packages()`.

- **[P1.2]** [cg-code-quality + cg-testing] `R/pip_snapshot.R:57`, `R/init_metapip.R:177` — `rowbind()` throws `subscript out of bounds` when every SHA resolution fails (all rows `NULL`/empty `lock_rows`). A realistic GitHub-outage path crashes with an opaque error.
  **Why**: `Filter(Negate(is.null), rows)` / `lock_rows` can be empty; `rowbind(list())` fails before the `nrow() > 0` guard.
  **Fix**: Guard before row-binding (warn "No SHAs resolved; lock not written" and return early); add all-skipped unit tests.

- **[P1.3]** [cg-code-quality] `R/init_metapip.R:67-81` — the no-lock fallback installs every core package without consulting `ask`/`answer`, silently bypassing the milestone-2 interactive gate (regression; most users hit this path since no lock ships).
  **Why**: the gate was only honored in the lock-present branch.
  **Fix**: Honor `ask`/`answer` in the fallback branch too.

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `test-init_metapip.R` — seven `update_pip_packages` tests neither set `metapip.lock_path` nor stub `pip_lock_path`; once a committed `inst/PIP_LOCK.csv` exists these will overwrite the source lock on every run (`write.csv` fires before the ask gate).
  **Why**: hermeticity relies on `pip_lock_path()` returning `""`.
  **Fix**: set `withr::local_options(metapip.lock_path = <tempfile>)` in each test (mirror the existing lock-write test).

- **[P2.2]** [cg-documentation] `R/pip_snapshot.R:64-67` — `pip_lock_path()` roxygen has `@return`/`@keywords internal` but no title, so no `man/pip_lock_path.Rd` is generated and the `[pip_lock_path()]` cross-ref stays literal.
  **Fix**: add a title line; regenerate docs.

- **[P2.3]** [cg-documentation] `R/init_metapip.R:91-105` / `man/init_metapip.Rd` — `update_pip_packages()`'s lock-refresh + write side effects are undocumented in its roxygen (shares `@rdname init_metapip`).
  **Fix**: add `@description`/`@details` describing lock refresh and install-at-pinned-SHA behavior; regenerate.

- **[P2.4]** [cg-reproducibility] `R/utils.R:129-139` vs `R/utils.R:92-109` — `check_github_token()` only checks `gitcreds` and aborts even when `GITHUB_PAT` is set (CI/fresh-machine barrier), inconsistent with `gh_token()`'s env-first design.
  **Fix**: consult `gh_token()` first (env vars), fall back to gitcreds/abort only when `gh_token()` returns NULL. [safe_auto]

- **[P2.5]** [cg-version-control] committed knowledge docs (`.cg-docs/work-reports/2026-08-14-lock-down-supply-chain.md`) and pre-existing history retain the literal Codecov badge token `5DMKVN16NM`; shipped/consumed artifacts are tokenless.
  **Why**: protected `.cg-docs` paths; token predates this session (commit `9751fd4`).
  **Fix**: rotate the Codecov token at codecov.io (remediates exposure); do not rewrite history for a badge token. [advisory]

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/install_pip_packages.R:146` — dead `return(invisible(NULL))` after `cli::cli_abort()`. Fix: delete. [advisory]
- **[P3.2]** [cg-code-quality] `R/install_pip_packages.R:40` — `suppressMessages(install_branch(..., force=TRUE))` also hides the inner bypass warning + install info. Fix: targeted suppression or drop. [advisory]
- **[P3.3]** [cg-code-quality] `R/init_metapip.R:50-55` — lock `sha` cell `NA` passes through to `install_github("...@NA")`. Fix: drop rows with `is.na(lock$sha)` before install.
- **[P3.4]** [cg-code-quality] `R/init_metapip.R:112,157` — `update_pip_packages()` resolves each branch SHA twice (`compare_sha` + lock-refresh loop), doubling GitHub API calls under the rate-limit guard. Fix: reuse a precomputed SHA map. [advisory]
- **[P3.5]** [cg-code-quality] `R/pip_snapshot.R` + `R/init_metapip.R` — duplicated lock-building logic (DRY). Fix: extract an internal `resolve_lock_rows()` helper. [advisory]
- **[P3.6]** [cg-testing] `test-get_branches.R:187-204` — SHA-override test not hermetic (missing `packageDescription` stub). Fix: add stub. [safe_auto]
- **[P3.7]** [cg-testing] `test-init_metapip.R:159-173` — "unknown" test never asserts the warning. Fix: capture `cli_alert_warning` and assert. [safe_auto]
- **[P3.8]** [cg-testing] `test-get_branches.R:60-64` — assertion inside the mock body. Fix: capture `token_seen` and assert after. [safe_auto]
- **[P3.9]** [cg-testing] `test-pip_snapshot.R:50-53` — `pip_lock_path` test is tautological. Fix: assert concrete invariant or drop. [advisory]
- **[P3.10]** [cg-testing] coverage gaps: `install_branch` invalid-branch abort, `pip_snapshot` `is.na(brn)` skip, `update_pip_packages` null-vector branch, `check_github_token` abort messages. Fix: one small `test_that` each. [advisory]
- **[P3.11]** [cg-testing] dead/commented-out tests in `test-get_branches.R` (empty "returns an error" blocks; pre-existing). Fix: restore or delete. [advisory]
- **[P3.12]** [cg-documentation] `README.Rmd/README.md:59` — credential guidance overbroad post least-privilege; `install_latest_branch()` not flagged as developer-only. Fix: add token note + dev-only flag; re-knit. [advisory]
- **[P3.13]** [cg-reproducibility] `R/init_metapip.R:150-182` — lock written before the interactive prompt regardless of decline. Fix: document; optionally note when skipped. [advisory]

### ✅ Passed

- [cg-code-quality]: no P0; checks verified `.Rbuildignore` excludes `.cg-docs/`.
- [cg-version-control]: commit hygiene, `.Rbuildignore`, tokenless README, resolved `origin/master` merge all pass.

## Notes

- Review produced by /cg-work review:auto (full route) dispatch of: cg-code-quality, cg-testing, cg-documentation, cg-version-control, cg-reproducibility (standard set). cg-performance, cg-architecture, cg-data-quality, cg-learnings-researcher, cg-adversarial: not yet dispatched (see /cg-review for the full set). R skill checks applied (cg-skill-r-technical/r-testing/r-shared).
- Severity for several items retains the originating agent's tag where multiple agents flagged overlapping issues.
