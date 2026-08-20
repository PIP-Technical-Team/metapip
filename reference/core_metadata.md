# Return meta information about core packages

For each requested core PIP package, retrieves and displays: - Number of
branches. - Latest release tag and release date (from GitHub
releases). - Branch name, author, and timestamp of the most recent
commit.

## Usage

``` r
core_metadata(package = NULL)
```

## Arguments

- package:

  Character vector. One or more core PIP package names. If \`NULL\`
  (default), all core packages are included.

## Value

A \`data.frame\` (returned invisibly) with columns:

- package:

  Character. Package name.

- no_of_branches:

  Integer. Number of branches on GitHub.

- latest_release_tag:

  Character. Most recent release tag, or \`NA\` if no releases exist.

- latest_release_time:

  Character. ISO 8601 timestamp of the latest release, or \`NA\`.

- latest_commit_branch:

  Character. Branch with the most recent commit.

- latest_commit_author:

  Character. Author of that commit.

- latest_commit_time:

  POSIXct (UTC). Timestamp of that commit.

## Progress

Uses \[cli::cli_progress_along()\] to show progress bars for branch
retrieval, release lookup, and commit resolution.

## Errors

Aborts if any \`package\` is not a core PIP package.

## See also

\[get_branches()\], \[get_latest_branch_update()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# All packages
core_metadata()

# Specific packages
core_metadata(c("pipapi", "wbpip"))
} # }
```
