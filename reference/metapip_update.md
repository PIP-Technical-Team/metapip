# Update metapip packages

This will check to see if all metapip packages (and optionally, their
dependencies) are up-to-date, and will install after an interactive
confirmation.

## Usage

``` r
metapip_update(pkg = "metapip", recursive = FALSE, ...)
```

## Arguments

- pkg:

  A character string for the model being updated.

- recursive:

  If \`TRUE\`, will also check all dependencies of metapip packages.

- ...:

  Extra arguments to pass to \[utils::install.packages()\]

## Value

Nothing is returned but a message is printed to the console about which
packages (if any) should be installed along with code to do so.

## Examples

``` r
if (FALSE) { # \dontrun{
metapip_update()
} # }
```
