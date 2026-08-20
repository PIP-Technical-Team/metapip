# Compare local installed SHA against GitHub branch HEAD

Checks whether the locally installed version of a package matches the
current HEAD of a branch on GitHub.

## Usage

``` r
compare_sha(package, branch)
```

## Arguments

- package:

  Character. Core PIP package name.

- branch:

  Character scalar. Branch name.

## Value

\- \`TRUE\` if the local SHA matches the GitHub HEAD. - \`FALSE\` if the
local SHA differs. - \`"unknown"\` if no \`RemoteSha\` metadata is found
locally (e.g., installed from CRAN). - \`NULL\` if the GitHub branch
cannot be resolved (e.g., network error or missing branch).
