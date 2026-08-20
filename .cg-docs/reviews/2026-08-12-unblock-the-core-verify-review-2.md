---
date: 2026-08-14
depth: light
parent-review: .cg-docs/reviews/2026-08-12-unblock-the-core-review.md
type: verification
plan: .cg-docs/plans/2026-08-13-performance-tests-release.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
  P3.4: fixed
  P3.5: fixed
  P3.6: fixed
  P3.7: fixed
  P3.8: skipped
---

# Verify Review Report

**Review mode**: light (verify pass)
**Files reviewed**: 18 (milestone changes + tests)
**Findings**: 16 (P0: 0, P1: 3, P2: 6, P3: 8)

## Verified Fixed Findings (from `2026-08-12-unblock-the-core-review.md`)

- P0.1 (numeric indexing), P1.1 (ifelse→fcase deviation justified: collapse 2.1.7 exports neither fcase nor fifelse), P1.2, P1.3, P1.5, P2.1, P2.2, P2.3, P2.4, P2.5, P2.6 — **all still resolved**. P1.4 (missing braces) **did not hold** at the named location → P2.2 below.

## P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality] `R/package_branches.R:119,125` — `base::package_version()` errors on malformed remote `Version` strings (untrusted input from raw PRESCRIPTION files), aborting the whole exported `package_branches()`. `suppressWarnings()` does not suppress errors.
  **Why**: Untrusted external data can crash the full status report.
  **Fix**: NA-safe wrapper, e.g. `pv <- function(x) vapply(x, function(z) tryCatch(as.character(base::package_version(z)), error = function(e) NA_character_), character(1))` and handle NA as "unknown".

- **[P1.2]** [cg-code-quality] `R/core_metadata.R:36-42,71-77` — transient `gh::gh` errors (rate limit/network) are memoized (NA fallback cached), silently poisoning the session cache: `compare_sha()` sees `sha=NULL` → `update_pip_packages()` mislabels packages as "branch not available".
  **Why**: Silent correctness failure after one transient API error.
  **Fix**: Cache only successful responses; return the fallback without `cache_set` on error (or use a TTL/sentinel).

- **[P1.3]** [cg-code-quality] `R/attach.R:40` — `to_load <- setdiff(core, not_installed_core_packages)` discards the caller-requested subset (`metapip_attach("wbpip")` attaches all installed core packages when any is missing).
  **Why**: Behavioral bug preserved by the requireNamespace rewrite.
  **Fix**: `to_load <- setdiff(to_load, not_installed_core_packages)`.

## P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/core_metadata.R:56-60` — `latest_release_tag`/`latest_release_time` column type varies (logical NA vs character) by failure count; `latest_release_time` (character) vs `latest_commit_time` (POSIXct) mismatch in the same data.frame.
  **Why**: Type instability breaks downstream time arithmetic/joins.
  **Fix**: `as.character()` the NA fallback; make `latest_release_time` POSIXct to match.

- **[P2.2]** [cg-code-quality] `R/install_pip_packages.R:18-19` — resolved P1.4 (missing braces around `if/else`) was applied to the wrong function; `install_latest_branch` still uses unbraced form `if(!is.null(package)) is_core(package) else package <- core`.
  **Fix**: Add braces.

- **[P2.3]** [cg-testing] `tests/testthat/test-get_branches.R:94-97,112-115` — two empty `test_that` blocks (commented-out expectations). These account for the suite's only 2 skips (`Reason: empty test`) and are misnamed (duplicate name).
  **Fix**: Delete both blocks (or fill real assertions).

- **[P2.4]** [cg-testing] `tests/testthat/test-get_branches.R:9-16,62-73` — `check_github_token()` unstubbed in "get_branches works correctly" and "install_branch works correctly"; tests only pass because this machine has stored credentials.
  **Why**: Environment-dependent test results; fails on clean runners.
  **Fix**: `mockery::stub(..., "check_github_token", function(...) list(password = "x"))`.

- **[P2.5]** [cg-testing] `tests/testthat/test-cache.R:1-16` — order-dependent: tests 2/3 leak keys `x`/`key` and never clear them, so the "cache miss returns NULL" test (test 1) is order-dependent. "No-op on empty cache" test (P3.1) is also mislabeled.
  **Fix**: Self-clear keys with `withr::defer(cache_clear(...))` or `on.exit()`; make no-op test a genuine empty-cache call.

- **[P2.6]** [cg-testing] `tests/testthat/test-package_branches.R` — the `ahead` status branch of the rewritten `join_and_get_status()` is never tested (behind/up-to-date/Not-in-local/not-in-repo covered).
  **Fix**: Add `local_version > version` → expect `"ahead PROD"`.

## P3 — MINOR (nice to have)

- **[P3.1]** [cg-testing] `tests/testthat/test-install_latest_branch.R:80-102` — "behaves consistently with memoization active" doesn't exercise the memoization cache (all deps stubbed); name overstates coverage.
  **Fix**: Rename, or un-stub `compare_sha` so the cache path is exercised between clears.

- **[P3.2]** [cg-testing] `tests/testthat/test-attach.R:41-52` — `expect_error(metapip_attach("wbpip"), NA)` emits a `cli_warn` for the stub-FALSE core packages → 1 suite-wide warning.
  **Fix**: Wrap in `suppressWarnings()` (warning path is covered by its own test).

- **[P3.3]** [cg-testing] `tests/testthat/test-get_core_pagkages.R` — error path (`exclude` non-core → `cli_abort`) untested; `exclude=NULL` literal return only indirectly covered.
  **Fix**: Add `expect_error(get_core_pagkages(exclude = "notacorepkg"), "not part of PIP ecosystem")`.

- **[P3.4]** [cg-code-quality] `R/get_branches.R:33` — `cli::cat_bullet(glue::glue("{branches}"))` collapses vector to single string.
  **Fix**: `cli::cat_bullet("{branches}")`.

- **[P3.5]** [cg-code-quality] `R/install_pip_packages.R:119` — redundant `glue::glue()` inside `cli::cli_alert_info()`.
  **Fix**: `cli::cli_alert_info("Installing branch {branch} from package {package}")`.

- **[P3.6]** [cg-code-quality] `R/zzz.R:32` — stray space before `[`: `options(metapip_default_options [toset])`.
  **Fix**: `options(metapip_default_options[toset])`.

- **[P3.7]** [cg-code-quality] `R/cache.R:13-30` — `cache_get` cannot distinguish stored `NULL` from a miss; `exists()`+`get()` double lookup.
  **Fix**: `get0(key, envir = .metapip_cache, inherits = FALSE)`.

- **[P3.8]** [advisory] `R/init_metapip.R:215-225` + `R/package_branches.R:81-85` + CI — deprecated `get_core_pagkages()` not enforced at runtime (no `.Deprecated()`); `read.dcf(url())` in `get_package_version()` has no error handling; Windows CI job has not run on a live runner.
  **Fix**: Optional; enforce deprecation and guard network fetch in a future pass.

## Passed

- No P0 findings.
- All six prior-fixed findings from the parent review remain resolved (except P1.4, tracked as P2.2).
- Memoization tested with call counters, not timing; cache isolation sound (except test-cache.R ordering, P2.5).
- No prod-path skips; the 2 skips are the (now-tracked) empty test blocks.
- No `installed.packages()` in R/ source; `requireNamespace` swap correct.
