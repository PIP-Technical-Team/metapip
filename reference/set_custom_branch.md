# Set or query custom per-package branch assignments

\`set_custom_branch()\` overrides the global default branch for specific
packages, persisting the change as the \`metapip.custom_branch\` R
option. \`get_custom_branch()\` retrieves the current custom branch
assignments.

## Usage

``` r
set_custom_branch(...)

get_custom_branch(package = NULL)
```

## Arguments

- ...:

  For \`set_custom_branch()\`: named character scalars where each name
  is a core PIP package and the value is the branch to use. For
  \`get_custom_branch()\`: not used.

- package:

  Character vector. For \`get_custom_branch()\`: filter to specific
  packages. Defaults to \`NULL\` (all custom branches).

## Value

\`set_custom_branch()\`: the updated custom branch list (returned
invisibly).

\`get_custom_branch()\`: a named list of class \`"metapip_simplelist"\`
with custom branch assignments, printed to the console.

## Details

Custom branches take precedence over the global default (from
\[get_default_branch()\]) in all functions that resolve a package's
branch. The convention is \`package_branch\` in the options list, but
you pass package names directly to \`set_custom_branch()\`.

## Errors

\`get_custom_branch()\` aborts if the requested packages have no custom
branch assignments.

## See also

\[get_default_branch()\], \[get_current_branches()\]

Other branch configuration:
[`get_current_branches()`](https://pip-technical-team.github.io/metapip/reference/get_current_branches.md),
[`get_default_branch()`](https://pip-technical-team.github.io/metapip/reference/get_default_branch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set per-package overrides
set_custom_branch(pipr = "main", pipapi = "DEV_v3")

# View all custom branches
get_custom_branch()
} # }
```
