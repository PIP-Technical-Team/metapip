# metapip in Package Development

``` r

library(metapip)
#> ── Attaching packages ───────────────────────────────────────── metapip 0.1.0 ──
```

## Overview

When developing a PIP package (e.g., `pipapi`, `wbpip`), you need the
other PIP ecosystem packages available at their correct versions.
`metapip` automatically detects when you’re working inside a PIP package
directory and excludes it from updates, so you never accidentally
overwrite the code you’re actively developing.

## Default and Custom Branches

The global default branch:

``` r

get_default_branch()
#> [1] "PROD"
```

Per-package overrides:

``` r

get_custom_branch()
#> 
#> ── metapip custom branches: ──
#> 
#> ◌ pipapi  : DEV 
#> ◌ pipfaker: main
#> ◌ wbpip   : DEV 
#> ◌ pipster : DEV
```

Change the global default:

``` r

set_default_branch("DEV")
```

## Inspecting Branch Assignments

See all current assignments:

``` r

get_current_branches()
#> ── metapip current branches (default in red): ──
#> 
#> ◌ pipapi  : DEV 
#> ◌ pipload : PROD
#> ◌ wbpip   : DEV 
#> ◌ pipfun  : PROD
#> ◌ pipdata : PROD
#> ◌ pipster : DEV 
#> ◌ pipaux  : PROD
#> ◌ pipfaker: main
```

Get the branch for a single package:

``` r

get_package_current_branch("pipdata")
#> pipdata 
#>  "PROD"
```

## Setting Custom Branches

Override individual packages:

``` r

set_custom_branch(pipdata = "main", pipapi = "DEV_v3")
#> ── metapip custom branches: ──
#> 
#> ◌ pipapi  : DEV_v3
#> ◌ pipfaker: main  
#> ◌ wbpip   : DEV   
#> ◌ pipster : DEV   
#> ◌ pipdata : main
```

## Listing Core Packages

``` r

get_core_pagkages()
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
```

## Auto-Exclusion Logic

When called with `exclude = NA` (the default),
[`get_core_pagkages()`](https://pip-technical-team.github.io/metapip/reference/get_core_pagkages.md)
automatically detects if your working directory is a PIP package and
excludes it. This is what
[`init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
and
[`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
use internally.

``` r

# Current directory is not a PIP package
get_core_pagkages(exclude = NA)
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
```

``` r

# Simulate working inside pipapi
pipapi_dir <- file.path(tempdir(), "pipapi")
dir.create(pipapi_dir, recursive = TRUE, showWarnings = FALSE)
withr::with_dir(
  pipapi_dir,
  print(get_core_pagkages(NA))
)
#> [1] "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"   "pipfaker"
```

## Package Development Workflow

1.  Navigate to your package directory (e.g., `~/repos/pipapi`).
2.  Run
    [`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
    – all core packages **except** `pipapi` are updated to their
    configured branch HEADs.
3.  Run
    [`init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
    – all core packages **except** `pipapi` are installed (if outdated)
    and attached.
4.  Develop your package with the correct dependency versions.
5.  When done, run
    [`pip_snapshot()`](https://pip-technical-team.github.io/metapip/reference/pip_snapshot.md)
    from a neutral directory to update the team lock.
