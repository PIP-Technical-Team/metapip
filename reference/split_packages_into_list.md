# Split non-common branches into per-package list (internal)

Split non-common branches into per-package list (internal)

## Usage

``` r
split_packages_into_list(complete_data)
```

## Arguments

- complete_data:

  \`data.frame\` from \[get_complete_data()\].

## Value

Named list of \`data.frame\`s, one per package, each with columns
\`branch\` and \`version\`.
