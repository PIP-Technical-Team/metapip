# Changelog

<!-- cg:auto:version-history -->
## Unreleased

- `install_branch()` pins to the resolved branch HEAD SHA by default
  (`force = TRUE` installs live HEAD, bypassing the lock; `sha =` overrides
  the resolved commit), with an idempotent skip when the installed
  `RemoteSha` already matches and a clean abort when the SHA cannot be
  resolved.
- New `pip_snapshot()` writes/refreshes the committed team lock manifest
  (`inst/PIP_LOCK.csv`, columns `package,branch,sha`) and aborts when no write
  target can be resolved; `options(metapip.lock_path)` controls the write
  target, with the internal read-only `pip_lock_path()` for reads.
- `init_metapip()` is now lock-driven: installs each package at its recorded
  SHA, falling back to branch HEAD (with a `pip_snapshot()` suggestion) when
  the lock is absent.
- `update_pip_packages()` refreshes `PIP_LOCK` with resolved HEAD SHAs and
  installs outdated packages at their updated SHAs, preserving per-package
  failure isolation and the interactive gate.
- `install_latest_branch()` is developer-only: it warns that it bypasses the
  team lockfile and installs live branch HEAD.
- Least-privilege token handling: `check_github_token()` returns a redacted
  `metapip_token` (its `print()` shows `""`); read-only GitHub functions
  (`get_branches`, `get_branch_info`, `get_latest_branch_update`,
  `core_metadata`, `package_branches`) work without a PAT, while install
  functions still require credentials (rate-limit guard).
- `get_branches()` now paginates through all branches (`.limit = Inf`), so
  repositories with more than 30 branches are fully returned.
- The README Codecov badge no longer carries a `?token=` parameter.

## 0.0.3

- Version fetching (`get_package_version()`, used by `package_branches()`)
  now uses `httr2` with a 10-second timeout and graceful error handling,
  returning `NA` on 404, malformed, or timed-out responses.
- Explicit UTC timezone parsing: `get_latest_branch_update()` and
  `core_metadata()` return timestamps as `POSIXct` in UTC.
- `get_latest_branch_update()` handles packages with only `gh-pages` branches
  gracefully (warns and returns a single-row `NA` data.frame).
- New runtime dependencies: `httr2` (>= 1.0.0) and `data.table`.

## 0.0.1

- Initial release. Meta-package that manages installation, updating, and
  inspection of the core PIP packages via the GitHub API.
<!-- cg:auto:end -->

← [Home](README.md)
