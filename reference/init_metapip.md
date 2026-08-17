# Initializes and updates the pip core packages

Based on options() settings provides an option to download latest
package versions from the branch

Refreshes the committed team \`PIP_LOCK\` manifest: it resolves each
core package's branch HEAD SHA, writes the updated \`PIP_LOCK.csv\`, and
(with confirmation) installs any outdated packages at their newly
resolved pinned SHAs. Preserves the milestone-2 per-package failure
isolation and interactive gate.

## Usage

``` r
init_metapip(exclude = NA, ask = TRUE, answer = 1)

update_pip_packages(exclude = NA, ask = TRUE, answer = 1)
```

## Arguments

- exclude:

  character: packages to exclude from attaching. if \`getwd()\` is one
  of the core PIP packages, that package will be excluded by default. To
  avoid that, set exclude to \`NULL\`.

- ask:

  logical. Ask the user if she wants to install outdated packages.
  Default TRUE

- answer:

  numeric: Developers argument. Only works for demonstration purposes.

## Value

\`init_metapip()\` returns invisible() output

\`update_pip_packages()\` return logical vector. TRUE if missing package
were update. FALSE if all packages are up to date of the user selects
not to update.

## Details

\`init_metapip()\` is lock-driven: when a committed \`PIP_LOCK.csv\`
manifest is found (via \[pip_lock_path()\]) it installs every package at
the SHA recorded in the lock, giving team-level deterministic installs.
When the lock is absent it falls back to installing each package at its
branch HEAD SHA and suggests running \[pip_snapshot()\] to create a team
lock.

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
