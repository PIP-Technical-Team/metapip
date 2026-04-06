# Get last update time of branches in a package

Returns a dataframe with information about package name, branch name,
author of the last commit and when was the last commit made to the
branch

## Usage

``` r
get_branch_info(package = "pipapi", branch = NULL, display = TRUE)
```

## Arguments

- package:

  one of the core package (default "pipapi")

- branch:

  valid branch name for the package

- display:

  (default TRUE) do you want to display the branches in the console?

## Value

a colorDF::colorDF output with an invisible dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
get_branch_info()
get_branch_info(branch = c("PROD", "QA"), display = FALSE)
get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
} # }
```
