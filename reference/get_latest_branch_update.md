# Get details of the most recently updated branch

Inspects all branches of a core PIP package (excluding \`gh-pages\`) and
returns the one with the most recent commit. Useful for identifying
which branch has the latest development activity.

If \`package\` has only \`gh-pages\` branches, a warning is issued and a
single-row data.frame of \`NA\` values is returned.

## Usage

``` r
get_latest_branch_update(package = "pipapi", display = TRUE)
```

## Arguments

- package:

  Character. Single core PIP package name. Defaults to \`"pipapi"\`.

- display:

  Logical. If \`TRUE\` (default), prints a colourised table to the
  console.

## Value

A single-row \`data.frame\` (returned invisibly) with columns:

- package:

  Character. Package name.

- branch_name:

  Character. Name of the most recently updated branch, or \`NA\` when
  only \`gh-pages\` exists.

- last_commit_author_name:

  Character. Author of the latest commit.

- last_update_time:

  POSIXct (UTC). Timestamp of the latest commit, or \`NA\` when only
  \`gh-pages\` exists.

## Edge cases

\- Packages with only \`gh-pages\` branches: returns \`NA\` values with
a warning. - Packages whose branches all fail to resolve (network
errors): returns \`NA\` values without aborting.

## See also

\[get_branch_info()\], \[get_branches()\]

## Examples

``` r
if (FALSE) { # \dontrun{
get_latest_branch_update()
get_latest_branch_update("wbpip", display = FALSE)
} # }
```
