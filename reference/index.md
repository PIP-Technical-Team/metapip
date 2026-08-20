# Package index

## Installation

Install and initialise PIP packages

- [`init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
  [`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
  : Initialise and update PIP core packages
- [`install_branch()`](https://pip-technical-team.github.io/metapip/reference/install_branch.md)
  : Install a specific branch of a PIP package
- [`install_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/install_pip_packages.md)
  : Install one or more PIP core packages from a branch
- [`install_latest_branch()`](https://pip-technical-team.github.io/metapip/reference/install_latest_branch.md)
  : Install the live HEAD of a branch (developer-only)

## Inspection

Explore branches, versions, and commit history

- [`get_branches()`](https://pip-technical-team.github.io/metapip/reference/get_branches.md)
  : Get available branches for a package
- [`get_branch_info()`](https://pip-technical-team.github.io/metapip/reference/get_branch_info.md)
  : Get last commit metadata for specific branches
- [`get_latest_branch_update()`](https://pip-technical-team.github.io/metapip/reference/get_latest_branch_update.md)
  : Get details of the most recently updated branch
- [`core_metadata()`](https://pip-technical-team.github.io/metapip/reference/core_metadata.md)
  : Return meta information about core packages
- [`package_branches()`](https://pip-technical-team.github.io/metapip/reference/package_branches.md)
  : Compare package versions across branches and local installations

## Branch Configuration

Set and query default and custom branch assignments

- [`get_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md)
  [`set_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md)
  : Get or set the global default branch
- [`get_current_branches()`](https://pip-technical-team.github.io/metapip/reference/get_current_branches.md)
  [`get_package_current_branch()`](https://pip-technical-team.github.io/metapip/reference/get_current_branches.md)
  : View current branch assignments for all (or selected) packages
- [`set_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md)
  [`get_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md)
  : Set or query custom per-package branch assignments

## Lock Management

Team-wide reproducible installs via PIP_LOCK.csv

- [`pip_snapshot()`](https://pip-technical-team.github.io/metapip/reference/pip_snapshot.md)
  : Snapshot current PIP package SHAs into a lock manifest

## Credentials

GitHub token validation

- [`check_github_token()`](https://pip-technical-team.github.io/metapip/reference/check_github_token.md)
  : Validate GitHub credentials

## Utilities

Package metadata and helper functions

- [`get_core_pagkages()`](https://pip-technical-team.github.io/metapip/reference/get_core_pagkages.md)
  : Get the list of core PIP ecosystem packages
- [`get_core_packages()`](https://pip-technical-team.github.io/metapip/reference/get_core_packages.md)
  : Get core PIP ecosystem package
- [`metapip_packages()`](https://pip-technical-team.github.io/metapip/reference/metapip_packages.md)
  : List all metapip package dependencies
- [`metapip_update()`](https://pip-technical-team.github.io/metapip/reference/metapip_update.md)
  : Check for CRAN updates to metapip and its dependencies
- [`pkg_deps()`](https://pip-technical-team.github.io/metapip/reference/pkg_deps.md)
  : Compare current package versions against CRAN
- [`rowname_to_column()`](https://pip-technical-team.github.io/metapip/reference/rowname_to_column.md)
  : Move row names to a column
- [`print(`*`<metapip_simplelist>`*`)`](https://pip-technical-team.github.io/metapip/reference/print.metapip_simplelist.md)
  : Pretty print for metapip_simplelist objects
- [`metapip`](https://pip-technical-team.github.io/metapip/reference/metapip-package.md)
  [`metapip-package`](https://pip-technical-team.github.io/metapip/reference/metapip-package.md)
  : metapip: Manage, Install, and Inspect PIP R Packages
