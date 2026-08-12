---
date: 2026-08-12
title: "Rebuilding data frames from named lists in R without losing branch names"
category: "data-quality"
language: "R"
tags: [named-lists, stack, data-frame, collapse, get_complete_data, bugfix]
root-cause: "utils::stack() on a named list of named vectors discards inner names in rownames; subsequent string extraction via regex returns NA, causing silent data corruption."
severity: "P1"
---

# Rebuilding data frames from named lists in R without losing branch names

## Problem

`get_complete_data()` used `utils::stack()` to rebuild a data frame from a named list where inner vectors carried branch names. The old implementation:

```r
get_complete_data <- function(all_package_version) {
  all_package_version |>
    utils::stack() |>
    rowname_to_column("branch") |>
    frename(version = values, package = ind) |>
    fmutate(branch = stringr::str_extract(branch, "([0-9A-Za-z-_]+)/DESCRIPTION\\.Version", group = 1))
}
```

`utils::stack()` on a `list(pkg = c(branch1 = "v1", branch2 = "v2"))` produces rownames `c("branch1", "branch2")` only when the inner vector is *unnamed*. When inner names are present, `stack()` uses them as rownames. However, if the inner names are URLs (e.g., from `sapply(urls, ...)`), the row names become full URLs. The regex then tried to extract branch names from URLs, but failed because the URL suffixes (`DESCRIPTION` vs `DESCRIPTION.Version`) did not match.

## Root Cause

1. `utils::stack()` does not reliably preserve inner names in a way that is easy to recover.
2. The regex pattern assumed a specific URL structure that did not match actual data.
3. When the regex returned NA, the `branch` column became all-NA, causing downstream `fsubset()` and `pivot()` to fail or produce empty results.

## Solution

Rebuild the data frame directly from the named list using base R vector operations:

```r
get_complete_data <- function(all_package_version) {
  branch  <- unlist(lapply(all_package_version, names))
  package <- rep(names(all_package_version), lengths(all_package_version))
  version <- unlist(all_package_version, use.names = FALSE)
  data.frame(package = package, branch = branch, version = version)
}
```

**Critical companion fix**: Ensure the *caller* (`get_package_version()`) sets inner vector names to branch names *before* building the list:

```r
lr[[x]] <- versions
names(lr[[x]]) <- br  # branch names, not URLs
```

Without this companion fix, the inner names remain URLs and the new `get_complete_data()` still returns URLs in the `branch` column.

## Prevention

- When building data frames from nested named lists, prefer explicit reconstruction with `unlist(lapply(...))` over `utils::stack()`.
- Set meaningful names at the point of vector creation (the producer), not after aggregation.
- Add unit tests that verify `branch` column values, not just row counts.
