# Set default custom branching options

Set default custom branching options

Get Custom branches

## Usage

``` r
set_custom_branch(...)

get_custom_branch(package = NULL)
```

## Arguments

- ...:

  Named elements to be added or updated in the custom default list.

- package:

  character: vector with name of branches. E.g., c("pipdata",
  "pipfaker"). Default return all packages whose default branches have
  been customed.

## Value

invisible custom branches

Names list of packages and their corresponding branch

## Examples

``` r
if (FALSE) { # \dontrun{
set_custom_branch(pipr = 'main', 'pipapi' = 'DEV_v2')
} # }
get_custom_branch()
#> 
#> ── metapip custom branches: ──
#> 
#> ◌ pipapi  : DEV 
#> ◌ pipfaker: main
#> ◌ wbpip   : DEV 
#> ◌ pipster : DEV 
```
