# get the current branches that are meant to be used

It does not necessarily mean that these are the branches currently
installed. \[init_metapip\] will notify you if that is the case.

## Usage

``` r
get_current_branches(package = NULL, verbose = TRUE)

get_package_current_branch(package)
```

## Arguments

- package:

  character: vector with name of branches. E.g., c("pipdata",
  "pipfaker").

- verbose:

  logical: whether to display all current branches. Default is TRUE

## Value

list with names of packages and branches

named character vector with branches of package

## Examples

``` r
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
get_package_current_branch(c("pipdata", "pipfaker"))
#>  pipdata pipfaker 
#>   "PROD"   "main" 
```
