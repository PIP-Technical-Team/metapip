# Flatten per-package version lists into a single data.frame (internal)

Flatten per-package version lists into a single data.frame (internal)

## Usage

``` r
get_complete_data(all_package_version)
```

## Arguments

- all_package_version:

  Named list from \[get_package_version()\].

## Value

\`data.frame\` with columns: \`package\`, \`branch\`, \`version\`.
