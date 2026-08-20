# Join local install info with remote and compute status (internal)

Join local install info with remote and compute status (internal)

## Usage

``` r
join_and_get_status(local, dev, branch_to_compare)
```

## Arguments

- local:

  \`data.frame\` with columns \`package\`, \`local_branch\`,
  \`local_version\`.

- dev:

  \`data.frame\` with columns \`package\`, \`branch\`, \`version\` for
  the comparison branch.

- branch_to_compare:

  Character scalar. Name of the comparison branch (for labelling).

## Value

\`data.frame\` with columns: \`package\`, \`local_branch\`,
\`local_version\`, \`local_status\`.
