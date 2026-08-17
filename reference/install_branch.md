# Install branch from a package

Install branch from a package

## Usage

``` r
install_branch(package = "pipapi", branch = NULL, force = FALSE, sha = NULL)
```

## Arguments

- package:

  one of the core package name (default "pipapi")

- branch:

  valid branch name (default "PROD")

- force:

  logical: when TRUE, bypasses SHA pinning and the idempotency check,
  installing the live branch HEAD (\`@\<branch\>\`) instead. Intended
  for developers. Default FALSE.

- sha:

  character: optional commit SHA to install at. When supplied it
  overrides the resolved branch HEAD SHA. When NULL (default) and
  \`force = FALSE\`, the branch HEAD SHA is resolved and pinned.

## Value

invisible NULL, or the result of \`remotes::install_github()\` when an
install is performed

## Examples

``` r
if (FALSE) { # \dontrun{
  install_branch()
  install_branch("pipfun", "ongoing")
  install_branch("pipfun", "ongoing", force = TRUE)
  install_branch("pipfun", "ongoing", sha = "a1b2c3d")
} # }
```
