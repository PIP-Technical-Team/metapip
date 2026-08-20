# Return the GitHub token when available, or NULL

Resolves a GitHub token by checking (in order): 1. \`GITHUB_PAT\`
environment variable. 2. \`GITHUB_TOKEN\` environment variable. 3.
\`gitcreds::gitcreds_get()\` (stored credentials).

Returns \`NULL\` when no credentials are available, allowing read-only
\`gh::gh()\` calls to proceed unauthenticated against public repos.

## Usage

``` r
gh_token()
```

## Value

Character string (the token) or \`NULL\` when no credentials are
available.

## See also

\[check_github_token()\]
