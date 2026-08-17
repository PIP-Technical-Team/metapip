<!-- README.md is generated from README.Rmd. Please edit that file -->

# metapip <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/PIP-Technical-Team/metapip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PIP-Technical-Team/metapip/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/PIP-Technical-Team/metapip/graph/badge.svg)](https://codecov.io/gh/PIP-Technical-Team/metapip)

<!-- badges: end -->

**metapip** installs, inspects, and manages the R packages behind the
World Bank’s [Poverty and Inequality Platform
(PIP)](https://pip.worldbank.org/). Think of it as the
[{tidyverse}](https://github.com/tidyverse/tidyverse) for PIP: one
package that loads and coordinates all the others.

## Why metapip?

The PIP ecosystem comprises multiple interdependent R packages (`pipapi`,
`pipload`, `wbpip`, `pipfun`, `pipdata`, `pipaux`, `pipster`, `pipfaker`)
hosted on GitHub. Keeping them in sync across users and environments is
error-prone. `metapip` solves this by:

- **One-call installs** – install all core packages from their configured
  branches with a single function call.
- **SHA-pinned reproducibility** – a `PIP_LOCK.csv` lockfile pins every
  package to an exact commit, so all team members get identical code.
- **Branch management** – configure per-package branch overrides and inspect
  version status across the ecosystem.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/metapip")
```

## Quick Start

``` r
library(metapip)

# 1. Verify GitHub credentials (recommended for rate limits)
check_github_token() |> print()

# 2. Install and attach all core packages
init_metapip()

# 3. Snapshot the current state for team reproducibility
pip_snapshot()
```

`init_metapip()` checks each package against its configured branch and installs
any outdated packages. When a `PIP_LOCK.csv` lockfile is present, it installs
from the exact recorded SHAs.

## Key Functions

### Installation

| Function                 | Description                                |
|--------------------------|--------------------------------------------|
| `init_metapip()`         | Install + attach all packages (lock-driven) |
| `install_branch()`       | Install one package from a branch/SHA      |
| `install_pip_packages()` | Install multiple packages from branches    |
| `update_pip_packages()`  | Refresh lock + install outdated packages   |

### Inspection

| Function                   | Description                                |
|----------------------------|--------------------------------------------|
| `get_branches()`           | List available branches for a package      |
| `get_branch_info()`        | Last commit info for specific branches     |
| `core_metadata()`          | Overview of all core packages              |
| `package_branches()`       | Version comparison across branches         |
| `get_current_branches()`   | View configured branch assignments         |

### Reproducibility

| Function        | Description                              |
|-----------------|------------------------------------------|
| `pip_snapshot()` | Write a `PIP_LOCK.csv` lock manifest    |

## Setting Up Branches

`metapip` uses a two-tier configuration:

``` r
# Global default (applies to all packages)
get_default_branch()       # "PROD"
set_default_branch("DEV")  # Change for session

# Per-package overrides (take precedence over global)
set_custom_branch(pipapi = "DEV_v3", pipfaker = "main")
get_custom_branch()        # View overrides
```

## GitHub Credentials

Install functions (`install_branch()`, `install_pip_packages()`) require a
GitHub PAT for rate-limit reliability. Read-only functions work without one.
Set up credentials with:

``` r
# Using gitcreds (recommended)
gitcreds::gitcreds_set()

# Or environment variable
Sys.setenv(GITHUB_PAT = "ghp_your_token")
```

## Team Workflow

1.  **Project lead**: run `pip_snapshot()` to write `PIP_LOCK.csv`.
2.  **Commit** the lockfile to version control.
3.  **Team members**: run `init_metapip()` to install at the recorded SHAs.
4.  **Updates**: run `update_pip_packages()` to refresh the lock and install
    new versions.

See `vignette("lockfile-workflow")` for the full workflow.

## Documentation

| Vignette | Description |
|---|---|
| [Getting Started](https://pip-technical-team.github.io/metapip/articles/getting-started.html) | Installation, credentials, first steps |
| [Architecture](https://pip-technical-team.github.io/metapip/articles/architecture.html) | Design, data flow, and core concepts |
| [Lockfile Workflow](https://pip-technical-team.github.io/metapip/articles/lockfile-workflow.html) | Team reproducibility end-to-end |
| [Package Development](https://pip-technical-team.github.io/metapip/articles/package-development.html) | Developing PIP packages with metapip |
| [Branch Configuration](https://pip-technical-team.github.io/metapip/articles/package-specific-options.html) | Default branches and per-package overrides |
| [Function Reference](https://pip-technical-team.github.io/metapip/reference/) | Complete function documentation |

## Dependency Note

`metapip` installs PIP packages from GitHub branches and resolves non-PIP
dependencies from CRAN independently, which can cause version skew. For
coordinated dependency resolution, consider using
[{renv}](https://rstudio.github.io/renv/) as a companion tool.