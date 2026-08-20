# Install the live HEAD of a branch (developer-only)

Installs the current live HEAD of the most recently updated branch for
each requested package. This is a \*\*developer-only\*\* utility that
deliberately bypasses the team \`PIP_LOCK\` manifest.

For reproducible team installs, use \[pip_snapshot()\] to create a lock
manifest, then \[init_metapip()\] to install from it.

## Usage

``` r
install_latest_branch(package = NULL)
```

## Arguments

- package:

  Character vector. One or more core PIP package names. If \`NULL\`
  (default), all core packages are installed.

## Value

\`NULL\` invisibly. Called for its side effect of installing packages.

## Behaviour

Packages already at the HEAD SHA of their latest branch are skipped with
an informational message.

## Warning

This function warns on entry that it bypasses the team lockfile.

## See also

\[install_branch()\], \[install_pip_packages()\], \[pip_snapshot()\],
\[init_metapip()\]

## Examples

``` r
if (FALSE) { # \dontrun{
install_latest_branch()
install_latest_branch(c("pipfun", "pipapi"))
} # }
```
