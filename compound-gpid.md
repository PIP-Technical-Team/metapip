# metapip

## Objective

This is a meta R package whose only objective is the proper management
of all the other PIP packages. It helps you install the latest branch
from PIP packages, get information about the packages and many more
things, all enabled via the GitHub API. It provides a set of functions
that allow working efficiently with all the PIP R packages (pipapi,
pipaux, pipload, wbpip, pipfun, pipdata, pipr).

## Key Deliverables

- Installable R package from GitHub:
  `devtools::install_github("PIP-Technical-Team/metapip")`
- Branch management: default branch via
  `options(metapip.default_branch)` (default “PROD”), per-package
  override via `options(metapip.custom_branch = list(...))`
- Functions to install, update, and inspect PIP packages:
  `install_pip_packages`, `install_latest_branch`, `install_branch`,
  `update_pip_packages`, `metapip_update`, `get_branches`,
  `get_branch_info`, `get_current_branches`,
  `get_default_branch`/`set_default_branch`,
  `get_custom_branch`/`set_custom_branch`, `core_metadata`,
  `get_core_pagkages`, `metapip_packages`, `pkg_deps`
- GitHub API integration with credential handling via
  [gitcreds](https://gitcreds.r-lib.org/) and
  [`check_github_token()`](https://pip-technical-team.github.io/metapip/reference/check_github_token.md)

## Constraints

- Manages exactly the core PIP packages: pipapi, pipaux, pipload, wbpip,
  pipfun, pipdata, pipr
- Requires valid GitHub credentials/token
  ([gitcreds](https://gitcreds.r-lib.org/), checked via
  [`check_github_token()`](https://pip-technical-team.github.io/metapip/reference/check_github_token.md))
- License: MIT

## Current Focus

I am working on optimizing the package.
