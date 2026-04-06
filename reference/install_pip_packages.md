# Install one (or more) pip core packages from a branch.

This installs packages like pipapi, pipaux, pipload, wbpip, pipfun,
pipdata from a branch

## Usage

``` r
install_pip_packages(package = NULL, branch = NULL)
```

## Arguments

- package:

  one (or more) of the core package name, if NULL all the core packages
  are installed from the branch

- branch:

  valid branch name (default "PROD")

## Value

invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
install_pip_packages(branch = "test")
install_pip_packages(package = "wbpip", branch = "DEV")
} # }
```
