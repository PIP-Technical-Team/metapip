# Snapshot current PIP package SHAs into a lock manifest

Resolves the current branch HEAD SHA of every core PIP package and
writes a \`PIP_LOCK.csv\` manifest with columns \`package\`, \`branch\`,
\`sha\`. Teams can commit this file to version control to make installs
deterministic across all members.

The lock is read by \[init_metapip()\] and refreshed by
\[update_pip_packages()\].

## Usage

``` r
pip_snapshot(path = NULL)
```

## Arguments

- path:

  Character. File path to write the lock to. Defaults to
  \`getOption("metapip.lock_path")\`, falling back to
  \`system.file("PIP_LOCK.csv", package = "metapip")\` (the bundled
  \`inst/\` directory). Set this to a temporary path for testing or to a
  project-level path for team use.

## Value

The path to the written lock file (invisibly).

## Tolerance

Packages whose SHA cannot be resolved (e.g., network error, missing
branch) are skipped with a warning; the lock is still written for the
successfully resolved packages.

## Workflow

1\. Run \`pip_snapshot()\` after confirming that all packages are at the
desired versions. 2. Commit the resulting \`PIP_LOCK.csv\`. 3. Team
members run \`init_metapip()\` to install at the recorded SHAs.

## See also

\[init_metapip()\], \[update_pip_packages()\], \[pip_lock_path()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Write to the default location
pip_snapshot()

# Write to a custom path
pip_snapshot(path = "path/to/my/PIP_LOCK.csv")

# Write to a temp file (for testing)
pip_snapshot(path = tempfile(fileext = ".csv"))
} # }
```
