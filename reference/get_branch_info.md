# Get last commit metadata for specific branches

For each requested branch of a PIP package, returns a data.frame with
the package name, branch name, last commit author name, and timestamp of
the last commit. Results are displayed in a colourised table via
\[colorDF::colorDF()\].

## Usage

``` r
get_branch_info(package = "pipapi", branch = NULL, display = TRUE)
```

## Arguments

- package:

  Character. Single core PIP package name. Defaults to \`"pipapi"\`.

- branch:

  Character vector. Branch name(s) to inspect. When \`NULL\` (default),
  uses the package's current branch assignment (via
  \[get_package_current_branch()\]).

- display:

  Logical. If \`TRUE\` (default), prints a colourised table to the
  console.

## Value

A \`data.frame\` (returned invisibly) with columns:

- package:

  Character. Package name.

- branch_name:

  Character. Branch name.

- last_commit_author_name:

  Character. Name of the last committer.

- last_update_time:

  Character. ISO 8601 timestamp of the last commit (e.g.,
  \`"2023-09-04T13:06:47Z"\`).

## Errors

Aborts if any requested branch name is not found in the package's branch
list.

## See also

\[get_branches()\], \[get_latest_branch_update()\],
\[latest_commit_for_branch()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Single branch
get_branch_info(package = "pipr", branch = "DEV")

# Multiple branches
get_branch_info(package = "wbpip", branch = c("PROD", "QA"))

# Use current branch (default)
get_branch_info(package = "wbpip")
} # }
```
