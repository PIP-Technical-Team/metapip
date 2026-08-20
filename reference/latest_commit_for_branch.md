# Get the latest commit for a specific branch (internal)

Fetches the latest commit object for a given branch of a package from
the GitHub API. Returns a fallback list of \`NA\` values on error so
callers can handle missing branches gracefully.

## Usage

``` r
latest_commit_for_branch(package, branch)
```

## Arguments

- package:

  Character. Core PIP package name.

- branch:

  Character scalar. Branch name. No default; must be provided explicitly
  by internal callers.

## Value

A list (the raw GitHub API response) containing at minimum: \`\$sha\`
(the commit SHA) and \`\$commit\$author\` (with \`\$date\` and
\`\$name\`). On error, returns a list with \`\$commit\$author\$date\` =
\`NA\` and \`\$commit\$author\$name\` = \`NA\`.
