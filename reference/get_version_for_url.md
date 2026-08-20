# Fetch DESCRIPTION version for a GitHub raw URL (internal)

Fetch DESCRIPTION version for a GitHub raw URL (internal)

## Usage

``` r
get_version_for_url(u)
```

## Arguments

- u:

  Character. URL to a DESCRIPTION file on GitHub.

## Value

Character scalar: the version string, or \`NA_character\_\` on error or
non-200 response.
