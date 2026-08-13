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
