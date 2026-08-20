# Get the list of core PIP ecosystem packages

Returns the character vector of core PIP package names, with optional
exclusions. When \`exclude = NA\` (the default), auto-detects if the
current working directory is a core PIP package and excludes it – useful
for package development workflows where you don't want to update the
package you're actively working on.

## Usage

``` r
get_core_pagkages(exclude = NULL)
```

## Arguments

- exclude:

  Exclusion specification: - \`NULL\`: return all core packages (no
  exclusions). - \`NA\` (default): auto-detect and exclude the package
  whose source is the current working directory. - Character vector:
  explicitly named packages to exclude.

## Value

Character vector of core PIP package names, in the standard ordering.

## Errors

Aborts if any explicitly named \`exclude\` package is not in the core
set.

## Examples

``` r
# All core packages
get_core_pagkages()
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"

# Exclude specific packages
get_core_pagkages(exclude = "pipdata")
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipster"  "pipaux"   "pipfaker"

# Auto-detect (for package development)
get_core_pagkages(exclude = NA)
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
```
