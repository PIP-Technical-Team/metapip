# Getting Started with metapip

## What is metapip?

`metapip` manages the R packages that power the World Bank’s Poverty and
Inequality Platform (PIP). It lets you install, inspect, and update all
PIP packages with a single function call.

## Prerequisites

- R \>= 4.1 (for the native pipe `|>`)
- Git installed on your system
- A GitHub Personal Access Token (PAT) – recommended for reliability

## Step 1: Install metapip

``` r

# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/metapip")
```

## Step 2: Set Up GitHub Credentials

Install functions require a GitHub PAT for rate limit reliability (5000
vs 60 requests/hour). The easiest approach:

``` r

# install.packages("gitcreds")
gitcreds::gitcreds_set()
```

Follow the prompts to store your token. Alternatively, set the
environment variable:

``` r

Sys.setenv(GITHUB_PAT = "ghp_your_token_here")
```

Verify your credentials:

``` r

library(metapip)
check_github_token() |> print()
#> <metapip_token (redacted)>
#> ● username: ""
#> ● password: ""
#> ● protocol: "https"
```

Note: the token is never displayed – the print method shows blanked
fields.

## Step 3: Initialise the Ecosystem

``` r

init_metapip()
```

This will:

1.  Check for a `PIP_LOCK.csv` lockfile.
2.  If found, install each package at its recorded SHA.
3.  If not found, install from each package’s configured branch HEAD.
4.  Attach all installed packages (like
    [`library()`](https://rdrr.io/r/base/library.html) for each one).

You’ll see output like:

    ── Attaching packages ────────────────────────────── metapip 0.0.3 ──
    ✔ pipapi   1.5.3  (DEV)
    ✔ pipload  1.0.0  (PROD)
    ✔ wbpip    0.1.6  (DEV)
    ✔ pipfun   1.0.0  (PROD)
    ✔ pipdata  0.0.1  (PROD)
    ...

## Step 4: Explore Your Packages

See which branches are available for a package:

``` r

get_branches("wbpip")
```

Check the latest commit on a branch:

``` r

get_branch_info(package = "pipr", branch = "DEV")
```

Get an overview of all core packages:

``` r

core_metadata()
```

Compare your local versions against branches:

``` r

package_branches()
```

## Step 5: Keep Packages Updated

To check for and install updates:

``` r

update_pip_packages()
```

This compares your local installation against each package’s branch
HEAD, installs any outdated packages, and refreshes the lock manifest.

## Understanding Branch Defaults

Every PIP package has a default branch. By default, this is `"PROD"`
globally. Some packages use different defaults (e.g., `pipapi` uses
`"DEV"` by default in `metapip`):

``` r

get_default_branch()
#> [1] "PROD"

get_current_branches()
```

To override a specific package’s branch:

``` r

set_custom_branch(pipdata = "main", pipapi = "DEV_v3")
```

To change the global default:

``` r

set_default_branch("DEV")
```

## Quick Reference

| Task | Function(s) |
|----|----|
| Install all packages | [`init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md) |
| Install one package | `install_branch("wbpip", "DEV")` |
| List branches | `get_branches("pipr")` |
| See latest commits | `get_branch_info("pipr", "DEV")` |
| Overview all packages | [`core_metadata()`](https://pip-technical-team.github.io/metapip/reference/core_metadata.md) |
| Check for updates | [`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md) |
| Snapshot for team reproducibility | [`pip_snapshot()`](https://pip-technical-team.github.io/metapip/reference/pip_snapshot.md) |
| Validate credentials | `check_github_token() |> print()` |

## Next Steps

- Read
  [`vignette("architecture")`](https://pip-technical-team.github.io/metapip/articles/architecture.md)
  to understand the internal design.
- Read
  [`vignette("lockfile-workflow")`](https://pip-technical-team.github.io/metapip/articles/lockfile-workflow.md)
  for team reproducibility workflows.
- Read `vignette("package-development")` for PIP package development
  tips.
