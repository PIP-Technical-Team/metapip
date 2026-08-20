# Check for CRAN updates to metapip and its dependencies

Checks whether \`metapip\` (and optionally its PIP package dependencies)
are up-to-date relative to the versions available on CRAN. Prints the
packages that need updating along with the R code to do so.

## Usage

``` r
metapip_update(pkg = "metapip", recursive = FALSE, ...)
```

## Arguments

- pkg:

  Character scalar. Package to check. Defaults to \`"metapip"\`.

- recursive:

  Logical. If \`TRUE\`, also checks all transitive dependencies of
  \`pkg\`. Default \`FALSE\`.

- ...:

  Additional arguments passed to \[utils::install.packages()\] in the
  generated install expression.

## Value

\`invisible()\`. Prints a human-readable update report; does not modify
any packages directly.

## Details

When \`metapip\` is not on CRAN (pre-release), a hardcoded dependency
list is used as fallback (see \`pkg_deps()\`).

## See also

\[pkg_deps()\]

## Examples

``` r
if (FALSE) { # \dontrun{
metapip_update()
metapip_update(recursive = TRUE)
} # }
```
