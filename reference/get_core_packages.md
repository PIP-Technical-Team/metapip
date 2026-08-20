# Get core PIP ecosystem package

Get core PIP ecosystem package

## Usage

``` r
get_core_packages(exclude = NULL)
```

## Arguments

- exclude:

  Character vector. Packages to exclude from installation and
  attachment. When \`NA\` (default), the function checks if the current
  working directory is a core PIP package and excludes it automatically
  (useful during package development). Pass \`NULL\` to exclude nothing.
  Pass a character vector of package names to exclude explicitly.

## Value

character vector with names of PIP packages

## Note

\`get_core_packages\` is an alias for \[get_core_pagkages()\]. The
\`get_core_pagkages\` spelling (typo) is retained and deprecated for
backward compatibility.

## Examples

``` r
get_core_packages()
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
get_core_packages(exclude = "pipdata")
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipster"  "pipaux"   "pipfaker"
```
