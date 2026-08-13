# Get details of the branch which was last updated

This function is useful to get latest branch name, author of latest
commit and time it was last updated. If \`package\` has only
\`gh-pages\` branches, a warning is issued and a single-row data.frame
filled with \`NA\` is returned invisibly.

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

colorDF::colorDF output along with an invisible single row dataframe.
Timestamps (\`last_update_time\`) are returned as POSIXct in UTC. If
\`package\` has only \`gh-pages\` branches, returns a single-row
data.frame with \`NA\` values (and warns).

## Examples

``` r
if (FALSE) { # \dontrun{
get_latest_branch_update()
get_latest_branch_update("wbpip", display = FALSE)
} # }
```
