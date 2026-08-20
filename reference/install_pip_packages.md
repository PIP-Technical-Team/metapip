# Install one or more PIP core packages from a branch

Convenience wrapper that installs multiple core PIP packages from a
specified branch (or each package's configured default branch). For a
fresh R installation, running \`install_pip_packages()\` with no
arguments sets up all core packages.

## Usage

``` r
install_pip_packages(package = NULL, branch = NULL)
```

## Arguments

- package:

  Character vector. One or more core PIP package names. If \`NULL\`
  (default), all core packages are installed.

- branch:

  Character scalar or named character vector. The branch to install
  from. When \`NULL\` (default), each package's configured default
  branch (via \[get_package_current_branch()\]) is used. When a scalar,
  all packages are installed from that single branch.

## Value

\`NULL\` invisibly.

## Side effects

Each package is installed via \[install_branch()\] with SHA pinning.
Failures for individual packages are caught and reported without
aborting the remaining installs.

## See also

\[install_branch()\], \[init_metapip()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Install all packages from their default branches
install_pip_packages()

# Install specific packages from a branch
install_pip_packages(c("pipapi", "wbpip"), branch = "test")
} # }
```
