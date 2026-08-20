# List all metapip package dependencies

Returns the names of all packages listed in the \`Imports\` field of
metapip's DESCRIPTION. Optionally includes \`"metapip"\` itself.

## Usage

``` r
metapip_packages(include_self = TRUE)
```

## Arguments

- include_self:

  Logical. If \`TRUE\` (default), includes \`"metapip"\` in the returned
  vector.

## Value

Character vector of package names.

## Examples

``` r
metapip_packages()
#>  [1] "cli"        "glue"       "gh"         "httr2"      "data.table"
#>  [6] "rstudioapi" "remotes"    "gitcreds"   "collapse"   "rlang"     
#> [11] "colorDF"    "metapip"   
metapip_packages(include_self = FALSE)
#>  [1] "cli"        "glue"       "gh"         "httr2"      "data.table"
#>  [6] "rstudioapi" "remotes"    "gitcreds"   "collapse"   "rlang"     
#> [11] "colorDF"   
```
