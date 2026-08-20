# Validate GitHub credentials

Checks that a GitHub Personal Access Token (PAT) is available and
returns a redacted copy. This is a rate-limit guard: authenticated
requests have 5000 req/hr vs 60 for unauthenticated.

The \`PIP-Technical-Team\` org is public, so this is not a security
requirement; it is a reliability requirement for installation functions.

## Usage

``` r
check_github_token()
```

## Value

An invisible list of class \`"metapip_token"\` with all credential
fields blanked. The \`print.metapip_token()\` method shows \`""\` in
place of the actual token – the real value is never exposed.

## Note

Install functions (\[install_branch()\], \[install_pip_packages()\])
resolve credentials via \`gitcreds\` independently; this function is a
validation gate, not a token carrier.

## Errors

Aborts with an instructional message if no git installation is found or
no credentials are stored.

## See also

\[gh_token()\]

## Examples

``` r
if (FALSE) { # \dontrun{
check_github_token() |> print()
} # }
```
