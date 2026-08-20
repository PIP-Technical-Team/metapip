---
date: 2026-08-14
title: "collapse does not export fcase/fifelse and join() never produces local_status"
category: "bugs"
language: "R"
tags: [collapse, fcase, fifelse, ifelse, join, local_status]
root-cause: "collapse 2.1.7 (used by metapip via `import(collapse)`) does not export `fcase` or `fifelse`, and its `join()` does not create a `local_status` column"
severity: "P1"
---

# collapse does not export fcase/fifelse and join() never produces local_status

## Problem

`package_branches()` crashed at runtime with `could not find function "fcase"`. A rewrite to `fifelse()` failed identically. Worse, the original `join_and_get_status()` referenced a `local_status` column (`fcase(local_status == 1, ...)`) that `collapse::join(local, dev, "package", how = "full")` never actually produces, so the function was **always broken** — it had simply never been executed because its only test was permanently `skip("avoid live network")`.

## Root Cause

- `collapse` 2.1.7 exports neither `fcase` nor `fifelse` (both live in `data.table`, which metapip does not import).
- `collapse::join()` returns the joined columns only (`package, local_branch, local_version, branch, version`); there is no `local_status` indicator column. The original code assumed one existed.
- The bug was invisible for a long time because the network-dependent test was hard-skipped, so the function never ran in CI.

## Solution

Replace `fcase`/`fifelse` with base `ifelse` (nested) or extract a per-row helper using scalar `utils::compareVersion`. Recompute `local_status` from actual data (`local_version` vs `version`) instead of reading a phantom column.

```r
join_and_get_status <- function(local, dev, branch_to_compare) {
  j <- join(local, dev, "package", how = "full")
  status_for <- function(local_version, branch, version) {
    if (is.na(local_version)) return("Not in local")
    if (is.na(branch)) return(paste(branch_to_compare, "not in repo"))
    if (!nzchar(trimws(local_version)) || !nzchar(trimws(version))) return("unknown")
    cmp <- tryCatch(utils::compareVersion(trimws(local_version), trimws(version)),
                    error = function(e) NA_integer_)
    if (is.na(cmp)) return("unknown")
    if (cmp == 0) return("up-to-date")
    if (cmp < 0) return(paste("behind", branch_to_compare))
    paste("ahead", branch_to_compare)
  }
  j$local_status <- mapply(status_for, j$local_version, j$branch, j$version)
  fselect(j, -branch, -version)
}
```

## Prevention

- Before using a collapse function, confirm it is exported: `"fcase" %in% getNamespaceExports("collapse")` (FALSE for fcase/fifelse in 2.1.7).
- Never hard-`skip()` tests that exercise a function's only real logic path — use mockery stubs so the code actually runs offline (full-stub the downstream network calls, e.g. `get_package_version()`).
- Prefer testing status logic directly via a small pure function rather than only through the full network-using caller.

## Related

- `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`
- `.cg-docs/plans/2026-08-13-performance-tests-release.md` (deviations section)
- collapse 2.1.7 `getNamespaceExports("collapse")`
