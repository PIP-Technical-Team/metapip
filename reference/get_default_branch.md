# Get or set the global default branch

\`get_default_branch()\` returns the value of the
\`metapip.default_branch\` option, which controls the default branch for
all core PIP packages. \`set_default_branch()\` modifies it.

## Usage

``` r
get_default_branch()

set_default_branch(branch)
```

## Arguments

- branch:

  Character. Name of the branch to set as the global default.

## Value

\`get_default_branch()\`: character string of the current default
branch.

\`set_default_branch()\`: the new default branch value (returned
invisibly).

## Details

The global default branch is the fallback used by all functions that
accept a \`branch\` argument when no explicit branch is provided.
Per-package overrides (set via \[set_custom_branch()\]) take precedence
over this global default.

## See also

Other branch configuration:
[`get_current_branches()`](https://pip-technical-team.github.io/metapip/reference/get_current_branches.md),
[`set_custom_branch()`](https://pip-technical-team.github.io/metapip/reference/set_custom_branch.md)

## Examples

``` r
# View current default
get_default_branch()
#> [1] "PROD"

if (FALSE) { # \dontrun{
# Change global default (persists for session)
set_default_branch("DEV")
} # }
```
