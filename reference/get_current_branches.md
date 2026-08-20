# View current branch assignments for all (or selected) packages

Returns the branch each core PIP package is \*configured\* to use, not
necessarily the branch currently installed. Use \[init_metapip()\] to
detect and close the gap between configured and installed branches.

Branch resolution order: 1. Custom per-package overrides (from
\[set_custom_branch()\]). 2. Global default (from
\[get_default_branch()\]).

Convenience wrapper around \[get_current_branches()\] that returns the
branch name(s) as a plain character vector (not a
\`metapip_simplelist\`).

## Usage

``` r
get_current_branches(package = NULL, verbose = TRUE)

get_package_current_branch(package)
```

## Arguments

- package:

  Character vector. One or more core PIP package names.

- verbose:

  Logical. If \`TRUE\` (default), prints the result via the
  \[print.metapip_simplelist()\] method and returns it visibly. If
  \`FALSE\`, returns a named list invisibly.

## Value

A named list of class \`"metapip_simplelist"\` where names are package
names and values are the configured branch (character scalar). When
\`verbose = FALSE\`, returned invisibly.

Named character vector. Names are package names, values are branch
names.

## Errors

Aborts if any \`package\` is not in the set of core packages.

## See also

\[get_package_current_branch()\], \[set_custom_branch()\],
\[init_metapip()\]

Other branch configuration:
[`get_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md),
[`set_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md)

## Examples

``` r
# Print all current branch assignments
get_current_branches()
#> 
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

# Silent retrieval for scripting
branches <- get_current_branches(verbose = FALSE)

if (FALSE) { # \dontrun{
# Restrict to specific packages
get_current_branches(package = c("pipdata", "pipfaker"))
} # }

get_package_current_branch("pipdata")
#> pipdata 
#>  "PROD" 
get_package_current_branch(c("pipdata", "pipfaker"))
#>  pipdata pipfaker 
#>   "PROD"   "main" 
```
