# Get details of the branch which was last updated

This function is useful to get latest branch name, author of latest
commit and time it was last updated.

## Usage

``` r
get_latest_branch_update(package = "pipapi", display = TRUE)
```

## Arguments

- package:

  one of the core package (default "pipapi")

- display:

  (default TRUE) do you want to display the branches in the console?

## Value

colorDF::colorDF output along with an invisible single row dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
get_latest_branch_update()
get_latest_branch_update("wbpip", display = FALSE)
} # }
```
