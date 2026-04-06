# Status tables of package versions in different branches along with local installations. For local installation,a status column is returned which indicates if the local version is ahead or behind the PROD branch.

Status tables of package versions in different branches along with local
installations. For local installation,a status column is returned which
indicates if the local version is ahead or behind the PROD branch.

## Usage

``` r
package_branches(
  package = NULL,
  branch_to_compare = getOption("metapip.default_branch")
)
```

## Arguments

- package:

  One (or more) of the PIP core packages. Default NULL will include all
  the packages

- branch_to_compare:

  character: names of branch to compare to. Default is "PROD".

## Value

table of pip packages and the corresponding package versions of branch

## Examples

``` r
if (FALSE) { # \dontrun{
package_branches()
package_branches(c("pipapi", "wbpip"))
package_branches(branch_to_compare = "QA")
} # }
```
