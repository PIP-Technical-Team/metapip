# Initialise and update PIP core packages

\`init_metapip()\` is the primary entry point for setting up the PIP
package ecosystem. It checks that all core packages are installed at the
correct versions and attaches them.

When a committed \`PIP_LOCK.csv\` manifest is found (via
\[pip_lock_path()\]), installs every package at the SHA recorded in the
lock for deterministic, team-consistent results. When the lock is
absent, falls back to installing each package at its branch HEAD SHA and
suggests running \[pip_snapshot()\] to create a team lock.

\`update_pip_packages()\` is the companion to \[pip_snapshot()\] and
\[init_metapip()\]. It:

1\. Compares each core package's locally installed SHA against its
branch HEAD on GitHub. 2. Refreshes the \`PIP_LOCK.csv\` manifest with
the resolved branch HEAD SHAs. 3. Installs any outdated packages at
their newly resolved SHAs (with confirmation).

Per-package installation failures are isolated and reported in a summary
(N succeeded, M failed).

## Usage

``` r
init_metapip(exclude = NA, ask = TRUE, answer = 1)

update_pip_packages(exclude = NA, ask = TRUE, answer = 1)
```

## Arguments

- exclude:

  Character vector. Packages to exclude from installation and
  attachment. When \`NA\` (default), the function checks if the current
  working directory is a core PIP package and excludes it automatically
  (useful during package development). Pass \`NULL\` to exclude nothing.
  Pass a character vector of package names to exclude explicitly.

- ask:

  Logical. If \`TRUE\` (default), prompts interactively before
  installing. In non-interactive sessions, always installs with a
  warning.

- answer:

  Numeric. Developer argument for demonstration purposes. \`1\` = Yes,
  \`2\` = No.

## Value

\`invisible()\`. Called for its side effects: installing packages and
attaching them.

Logical (invisibly). \`TRUE\` when packages were installed, \`FALSE\`
when all packages were up-to-date or the user declined.

## Details

\*\*Lock-driven workflow:\*\* 1. Reads \`PIP_LOCK.csv\` via
\[pip_lock_path()\]. 2. Filters to packages that are not excluded. 3.
Installs each at its recorded SHA.

\*\*Fallback workflow (no lock):\*\* 1. Resolves each package's
configured branch. 2. Installs at branch HEAD SHA. 3. Suggests running
\[pip_snapshot()\] to create a lock.

After installation, attaches all available core packages via
\[metapip_attach()\].

The function checks for three local states: - \`TRUE\`: Local SHA
matches branch HEAD (up-to-date). - \`FALSE\`: Local SHA differs
(outdated). - \`"unknown"\`: No \`RemoteSha\` metadata (e.g., CRAN
install); package is skipped with a warning.

## Errors

Individual package install failures are caught and reported without
aborting the remaining installs.

## See also

\[update_pip_packages()\], \[pip_snapshot()\], \[install_branch()\],
\[get_core_pagkages()\]

\[init_metapip()\], \[pip_snapshot()\], \[compare_sha()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive: prompts before installing
init_metapip()

# Non-interactive: auto-install
init_metapip(ask = FALSE)

# Exclude specific packages
init_metapip(exclude = c("pipdata", "pipfaker"))
} # }

if (FALSE) { # \dontrun{
update_pip_packages()

# Non-interactive (for CI/scripts)
update_pip_packages(ask = FALSE)
} # }
```
