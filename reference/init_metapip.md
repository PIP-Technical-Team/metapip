# Initializes and updates the pip core packages

Based on options() settings provides an option to download latest
package versions from the branch

## Usage

``` r
init_metapip(exclude = NA, ask = TRUE, answer = 1)

update_pip_packages(exclude = NA, ask = TRUE, answer = 1)
```

## Arguments

- exclude:

  character: packages to exclude from attaching. if \[getwd\] is one of
  the core PIP packages, that package will be excluded be default. To
  avoid that, set exclude to \`NULL\`.

- ask:

  logical. Ask the user if she wants to install outdated packages.
  Default TRUE

- answer:

  numeric: Developers argument. Only works for demonstration purposes.

## Value

\`init_metapip()\` return invisible() output

\`update_pip_packages()\` return logical vector. TRUE if missing package
were update. FALSE if all packages are up to date of the user selects
not to update.

## Examples

``` r
if (FALSE) { # \dontrun{
  init_metapip()
} # }

if (FALSE) { # \dontrun{
update_pip_packages(ask = FALSE,
answer = 2) # this is to make it work in examples and vignettes.
} # }
```
