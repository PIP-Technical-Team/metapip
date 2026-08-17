# make sure your GITHUB credentials are properly setup

make sure your GITHUB credentials are properly setup

## Usage

``` r
check_github_token()
```

## Value

invisible redacted list of credentials (token never exposed). The return
value carries the class \`metapip_token\`, whose \`print()\` method
shows blanked secret fields only. The real token is never carried back —
\`remotes::install_github()\` resolves credentials itself via gitcreds,
so this function is a validation gate, not a token carrier.

## Note

This gate is a rate-limit guard (5000 authenticated vs 60
unauthenticated GitHub API requests/hour). Install reliability requires
the higher limit. The PIP-Technical-Team org is public; this is not a
security gate.

## Examples

``` r
if (FALSE) { # \dontrun{
check_github_token()
} # }
```
