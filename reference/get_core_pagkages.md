# Get core PIP ecosystem package

Get core PIP ecosystem package

## Usage

``` r
get_core_pagkages(exclude = NULL)
```

## Arguments

- exclude:

  character: packages to exclude from attaching. if \`getwd()\` is one
  of the core PIP packages, that package will be excluded by default. To
  avoid that, set exclude to \`NULL\`.

## Value

character vector with names of PIP packages

## Examples

``` r
get_core_pagkages()
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipdata"  "pipster"  "pipaux"  
#> [8] "pipfaker"
get_core_pagkages(exclude = "pipdata")
#> [1] "pipapi"   "pipload"  "wbpip"    "pipfun"   "pipster"  "pipaux"   "pipfaker"
```
