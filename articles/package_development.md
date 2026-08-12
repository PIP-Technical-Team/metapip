# metapip in package development

``` r

 # devtools::load_all(".")
library(metapip)
#> ── Attaching packages ───────────────────────────────────────── metapip 0.0.1 ──
#> ✔   ()
```

## metapip: Managing PIP Ecosystem Packages for Development

The `metapip` package is designed to make it easy to manage all the
packages in the PIP ecosystem. This is especially useful for package
development, as the PIP packages depend on each other—most notably, like
`pipfun`, which provides shared utilities. Coordinating branches and
versions across these packages is essential for a smooth development
workflow.

## Default and Custom Branches

To use the most recent version of a package, it’s important to know
which branch is considered the default for each package. By default, the
branch is available with
[`get_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md),

``` r

get_default_branch()
#> [1] "PROD"
```

and you can modify it with
[`set_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md).
For example:

``` r

set_default_branch("DEV")
```

However, not all packages’ most recent versions are available in the
same branch. Packages that do not use the default branch are tracked as
“custom branches.” You can see these with
[`get_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md):

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

## Inspecting and Modifying Branch Assignments

To see all PIP ecosystem packages and their currently assigned branches,
use
[`get_current_branches()`](https://pip-technical-team.github.io/metapip/reference/get_current_branches.md).
To get the branch for a specific package, use
`get_package_current_branch(package)`. For example:

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
get_package_current_branch("pipdata")
#> pipdata 
#>  "PROD"
```

If you need to change the working branch for any package, use
[`set_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md).
For example, to set the `pipdata` package to use the `main` branch and
`pipapi` to use `DEV_v3`:

``` r

set_custom_branch(pipdata = 'main', pipapi = 'DEV_v3')
#> ── metapip custom branches: ──
#> 
#> ◌ pipapi  : DEV_v3
#> ◌ pipfaker: main  
#> ◌ wbpip   : DEV   
#> ◌ pipster : DEV   
#> ◌ pipdata : main
```

## Listing Core Packages

To get a list of all core PIP packages, use:

``` r

get_core_pagkages()
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
```

## Package Development Workflow

When developing a PIP package, it is important to ensure you are using
the most recent versions of all other PIP ecosystem packages, each on
their appropriate default branch. Typically, you can achieve this by
running
[`metapip::update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md),
which updates all core PIP packages except the one you are currently
developing.

If you want to both update and attach (i.e., load with
[`library()`](https://rdrr.io/r/base/library.html)) the PIP packages,
use
[`metapip::init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md).
Both functions automatically detect your current working directory and
exclude the package you are developing from updates and attachment.

This behavior is managed by the
[`get_core_pagkages()`](https://pip-technical-team.github.io/metapip/reference/get_core_pagkages.md)
function. By default, it returns all core PIP packages. However, when
called with `exclude = NA`, it checks if your working directory is a PIP
package and excludes it from the list.

For example, if you are working on the `pipapi` package,
`get_core_pagkages(exclude = NA)` will return all core packages except
`pipapi`. This ensures you do not accidentally update or reload the
package you are actively developing. The following explains the logic
behind the process. **Do NOT use this in your code**.

``` r

# the same as get_core_pagkages() because the working directory 
# is not a PIP package
get_core_pagkages(exclude = NA)
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"

pipapi_dir <- file.path(tempdir(), "pipapi")
dir.create(pipapi_dir, recursive = TRUE, showWarnings = FALSE)
withr::with_dir(
  pipapi_dir,
  
  # Since the working directory is a PIP package, 
  # it will be excluded 
  get_core_pagkages(NA))
#> [1] "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"   "pipfaker"
```
