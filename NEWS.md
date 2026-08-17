# metapip (unreleased)

* `install_branch()` now pins to the resolved branch HEAD SHA by default (session-level determinism) with an idempotency short-circuit (`force = TRUE` bypasses pinning and installs live HEAD; `sha =` overrides the resolved SHA).
* New `pip_snapshot()` writes a committed team lock manifest (`PIP_LOCK.csv` with `package,branch,sha` columns). `init_metapip()` is now lock-driven: it installs each package at its recorded SHA and falls back to branch HEAD (with a `pip_snapshot()` suggestion) when the lock is absent. `update_pip_packages()` refreshes the lock and installs outdated packages at their new SHAs.
* `install_latest_branch()` is now developer-only: it warns that it bypasses the team lockfile and installs live branch HEAD.
* Least-privilege token handling: read-only GitHub calls (`get_branches`, `core_metadata`, `get_branch_info`, `get_latest_branch_update`, `package_branches`) no longer require a PAT, and `print(check_github_token())` shows a redacted token.
* `get_branches()` now paginates through all branches (`.limit = Inf`), so repos with more than 30 branches are fully returned.
* README codecov badge no longer carries a `?token=` parameter (removed committed badge token).

# metapip 0.0.3

* `get_package_version()` now uses httr2 with timeouts and graceful error handling.
* Timestamp parsing uses explicit UTC timezone in `get_latest_branch_update()` and `core_metadata()`.
* `get_latest_branch_update()` handles packages with only gh-pages branches gracefully.
* Added httr2 to Imports.

# metapip 0.0.2

* `detach_package()` now warns and continues on `unloadNamespace()` failure instead of aborting.
* `update_pip_packages()` isolates per-package failures and reports a summary (N succeeded, M failed).
* `update_pip_packages(ask=TRUE)` no longer hangs in non-interactive sessions; defaults to install with warning.
* `compare_sha()` returns `"unknown"` (not `FALSE`) when `RemoteSha` is missing (e.g., CRAN install).
* `get_latest_branch_update()` handles empty branch lists gracefully.
* `install_latest_branch()` skips packages already at HEAD of their branch.
* Added CRAN dependency gap note recommending `renv` for coordinated dependency management.

# metapip 0.0.1
 
* Remove dependencies from `tidyverse` packages `dplyr`, `tidyr`, `purrr`, `magrittr` and `tibble`.
* add suit to update packages. 

# metapip 0.0.0.9010

* Add a new `package_branches()` function to show package versions in branches.

# metapip 0.0.0.9008

* Unload packages before installing (#3) 
