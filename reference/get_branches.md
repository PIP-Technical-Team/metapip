# Get available branches for a package

Queries the GitHub API for all branches of a PIP package in the
\`PIP-Technical-Team\` organisation. Paginates automatically so repos
with more than 30 branches are fully returned.

## Usage

``` r
get_branches(package = "pipapi", display = TRUE)
```

## Arguments

- package:

  Character. Single core PIP package name. Defaults to \`"pipapi"\`.

- display:

  Logical. If \`TRUE\` (default), branch names are printed to the
  console with bullet markers. Set to \`FALSE\` for silent use in
  scripts.

## Value

Character vector of branch names (returned invisibly). Always returns
the full set regardless of the \`display\` setting.

## Side effects

When \`display = TRUE\`, prints a header and bullet list to the console
via \[cli::cli_h3()\] and \[cli::cat_bullet()\].

## Authentication

Read-only function: works without a GitHub PAT against the public
\`PIP-Technical-Team\` org. Supplying a PAT via \[gh_token()\] increases
the API rate limit from 60 to 5000 requests/hour.

## See also

\[get_branch_info()\], \[get_latest_branch_update()\],
\[install_branch()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive use: displays branches in console
branches <- get_branches("wbpip")

# Silent use: suppresses display
branches <- get_branches("wbpip", display = FALSE)
} # }
```
