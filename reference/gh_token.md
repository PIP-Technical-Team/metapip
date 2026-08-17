# Return the GitHub token when available, or NULL

Honours the \`GITHUB_PAT\` / \`GITHUB_TOKEN\` environment variables
first, then falls back to \`gitcreds::gitcreds_get()\`. Never aborts: it
returns \`NULL\` when no credentials are available so read-only
\`gh::gh()\` calls can operate unauthenticated against the public
\`PIP-Technical-Team\` org.

## Usage

``` r
gh_token()
```

## Value

Character string (the token) or \`NULL\` when no credentials are
available.
