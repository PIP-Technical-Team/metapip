# Install latest branch from a package

Based on the last commit of the branch it installs the latest branch of
the package.

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
