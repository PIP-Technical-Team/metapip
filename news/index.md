# Changelog

## metapip 0.0.2

- `detach_package()` now warns and continues on
  [`unloadNamespace()`](https://rdrr.io/r/base/ns-load.html) failure
  instead of aborting.
- [`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
  isolates per-package failures and reports a summary (N succeeded, M
  failed).
- `update_pip_packages(ask=TRUE)` no longer hangs in non-interactive
  sessions; defaults to install with warning.
- `compare_sha()` returns `"unknown"` (not `FALSE`) when `RemoteSha` is
  missing (e.g., CRAN install).
- [`get_latest_branch_update()`](https://pip-technical-team.github.io/metapip/reference/get_latest_branch_update.md)
  handles empty branch lists gracefully.
- [`install_latest_branch()`](https://pip-technical-team.github.io/metapip/reference/install_latest_branch.md)
  skips packages already at HEAD of their branch.
- Added CRAN dependency gap note recommending `renv` for coordinated
  dependency management.

## metapip 0.0.1

- Remove dependencies from `tidyverse` packages `dplyr`, `tidyr`,
  `purrr`, `magrittr` and `tibble`.
- add suit to update packages.

## metapip 0.0.0.9010

- Add a new
  [`package_branches()`](https://pip-technical-team.github.io/metapip/reference/package_branches.md)
  function to show package versions in branches.

## metapip 0.0.0.9008

- Unload packages before installing
  ([\#3](https://github.com/PIP-Technical-Team/metapip/issues/3))
