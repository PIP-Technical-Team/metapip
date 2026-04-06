# Return meta information about core packages

Returns information like name of the package, number of branches it has,
last release tag, last release time, last commit author etc.

## Usage

``` r
core_metadata(package = NULL)
```

## Arguments

- package:

  One (or more) core packages, if NULL shows information about all of
  them.

## Value

a colorDF::colorDF output with details about the core packages

## Examples

``` r
if (FALSE) { # \dontrun{
core_metadata()
core_metadata(c("pipapi", "wbpip"))
} # }
```
