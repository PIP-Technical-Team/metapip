# Install latest branch from a package

Based on the last commit of the branch it installs the latest branch of
the package. This is a developer-only tool: it deliberately bypasses the
team \`PIP_LOCK\` manifest and installs the live HEAD of each branch.
Use \[pip_snapshot()\] + \[init_metapip()\] for team-consistent
installs.

## Usage

``` r
install_latest_branch(package = NULL)
```

## Arguments

- package:

  one (or more) of core packages. default NULL would install latest
  branch for all packages

## Examples

``` r
if (FALSE) { # \dontrun{
  install_latest_branch()
  install_latest_branch(c("pipfun", "pipapi"))
} # }
```
