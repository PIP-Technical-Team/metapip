# metapip 0.1.1

## Bug Fixes

* `package_branches()` now reports `"{branch} version unknown"` instead of a
  bare `"unknown"` when a configured branch is absent from the remote repo,
  using vectorized `fcase()` conditions that stay NA-safe for missing versions.

## Testing

* Added test coverage for cache memoization, `core_metadata()`, and utility
  helpers.
* CI coverage now attributes results correctly with
  `with_mocked_bindings()` and indexes the exported `get_core_packages()`
  alias in the pkgdown reference.

## Infrastructure

* `get_core_packages()` added to the pkgdown function reference index.

# metapip 0.1.0

## Breaking Changes

- None.

## New Features

* `get_core_packages()` added as a correctly spelled alias for the
  deprecated `get_core_pagkages()` (spelling kept for backward compatibility).
* `package_branches()` now reports an accurate `local_status` column
  (behind/ahead/up-to-date, plus "Not in local", "not in repo", and "unknown"
  for unparseable versions).
* New `pip_snapshot()` writes a committed team lock manifest (`PIP_LOCK.csv`
  with `package,branch,sha` columns). `init_metapip()` is now lock-driven:
  it installs each package at its recorded SHA and falls back to branch HEAD
  (with a `pip_snapshot()` suggestion) when the lock is absent.
  `update_pip_packages()` refreshes the lock and installs outdated packages
  at their new SHAs.
* `get_branches()` now paginates through all branches (`.limit = Inf`), so
  repos with more than 30 branches are fully returned.
* Least-privilege token handling: read-only GitHub calls
  (`get_branches`, `core_metadata`, `get_branch_info`,
  `get_latest_branch_update`, `package_branches`) no longer require a PAT,
  and `print(check_github_token())` shows a redacted token.

## Bug Fixes

* `package_branches()` no longer fails at runtime: `join_and_get_status()`
  previously referenced a `local_status` column that the join never produced,
  and malformed remote `Version` strings now degrade to `"unknown"` instead of
  crashing the report.
* `install_branch()` now pins to the resolved branch HEAD SHA by default
  (session-level determinism) with an idempotency short-circuit (`force = TRUE`
  bypasses pinning and installs live HEAD; `sha =` overrides the resolved SHA).
* `get_latest_branch_update()` handles empty and gh-pages-only branch sets, and
  parses timestamps as UTC.
* `install_latest_branch()` skips packages already at HEAD of their branch and
  is now developer-only (warns it bypasses the team lockfile).
* `get_package_version()` uses httr2 with timeouts and graceful error handling.
* `update_pip_packages()` isolates per-package failures and reports a summary
  (N succeeded, M failed); no longer hangs in non-interactive sessions.
* `detach_package()` warns and continues on `unloadNamespace()` failure.
* README codecov badge no longer carries a `?token=` parameter.

## Performance

* Per-session memoization of GitHub API calls in `get_branches()`,
  `latest_commit_for_branch()`, and `core_metadata()`. Repeated calls in the
  same R session hit the cache instead of the API (~80 calls reduced to ~3
  for a full `core_metadata()` refresh). Only successful responses are cached
  so transient failures are retried.
* `metapip_attach()` and `rs_theme()` use per-package `requireNamespace()`
  checks instead of a full `installed.packages()` library scan.

## Testing

* Full test coverage for exported functions using mockery stubs; no
  un-skipped production-path tests.
* Network-dependent tests gated with `skip_if_offline()` + `skip_on_cran()`.
* Deleted obsolete test for the nonexistent `install_all_packages()`.

## Internal

* Cleaned `globalVariables()` declarations (duplicates and unused data.table
  symbols removed).
* Removed duplicate `@docType package` roxygen in `R/metapip.R`.
* CI runs `R CMD check` on both Ubuntu and Windows; GitHub Pages deploy uses
  `actions/checkout@v4`.

## Infrastructure

* Added Windows CI job with a documented `GH_TOKEN` (fine-grained PAT with
  `contents:read`) policy.
