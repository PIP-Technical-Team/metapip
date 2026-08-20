# Compare current package versions against CRAN

Queries CRAN for the latest versions of the specified packages and their
(optionally recursive) dependencies, then compares with locally
installed versions.

## Usage

``` r
pkg_deps(x = "metapip", recursive = FALSE)
```

## Arguments

- x:

  Character scalar. Package name to check. Defaults to \`"metapip"\`.

- recursive:

  Logical. If \`TRUE\`, includes all transitive dependencies. Default
  \`FALSE\`.

## Value

A \`data.frame\` with columns:

- package:

  Character. Package name.

- cran:

  Character. CRAN version.

- local:

  Character. Locally installed version.

- behind:

  Logical. \`TRUE\` if the CRAN version is newer.

Base R packages are excluded.

## Details

When \`"metapip"\` is in \`x\` and is not yet on CRAN, a hardcoded set
of dependencies is returned as fallback.

## See also

\[metapip_update()\]

## Examples

``` r
if (FALSE) { # \dontrun{
pkg_deps()
pkg_deps("metapip", recursive = TRUE)
} # }
```
