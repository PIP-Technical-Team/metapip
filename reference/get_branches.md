# Get available branches for a package

Returns a vector of branch names for the package specified. If display
is set to TRUE, the branch names are printed in the console.

## Usage

``` r
get_branches(package = "pipapi", display = TRUE)
```

## Arguments

- package:

  one of the core package name (default "pipapi")

- display:

  (default TRUE) do you want to display the name of the branches in the
  console?

## Value

an invisible character vector of branches

## Examples

``` r
if (FALSE) { # \dontrun{
  get_branches()
  get_branches("wbpip", display = FALSE)
} # }
```
