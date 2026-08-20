---
date: 2026-08-14
title: "Parse untrusted remote version strings safely with compareVersion + tryCatch"
category: "data-quality"
language: "R"
tags: [package_version, compareVersion, untrusted-input, DESCRIPTION, ifelse]
root-cause: "base::package_version() errors (not warns) on malformed version strings from untrusted remote DESCRIPTION files; suppressWarnings() does not catch errors"
severity: "P1"
---

# Parse untrusted remote version strings safely with compareVersion + tryCatch

## Problem

`package_branches()` reads `Version:` fields straight from `raw.githubusercontent.com/.../DESCRIPTION` files via `read.dcf(url(...))` — fully untrusted input controlled by any branch or contributor. The first attempt parsed them with `pv <- function(x) suppressWarnings(base::package_version(x))`. `package_version()` **errors** on `"v1.2.3"`, `"1.2"` (no patch), `""`, `"not-a-version"`, etc., and `suppressWarnings()` does not suppress errors. One malformed value in any branch of any repo aborted the whole exported `package_branches()` report for all packages — and because base `ifelse` evaluates all branches eagerly, even rows that should be "Not in local" were killed.

## Root Cause

`base::package_version()` is strict: invalid specifications raise `Error: invalid version specification`. `package_version("")` and `package_version("1")` both error. In a vectorized `ifelse`, `pv()` is applied to whole columns, so one bad element tears down the entire call. Additionally, metapip defines its own internal `package_version()` (a color formatter that calls `utils::packageVersion()`), so an unqualified `package_version(...)` inside the package silently hits the wrong function.

## Solution

Compare versions with scalar `utils::compareVersion()` wrapped in `tryCatch`, per row, and degrade to a visible `"unknown"` status:

```r
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
```

Key points:

- `utils::compareVersion(a, b)` is **scalar-only** and returns -1/0/1; it errors on malformed input, so wrap it in `tryCatch`. It is the same comparison `package_version` uses but safe under explicit NA/empty guards.
- Guard empty/whitespace and NA **before** comparing (an empty string makes `compareVersion` return -1, i.e. misleading "behind").
- Apply per row (`mapply`) rather than vectorized, so one malformed value degrades only its own row to `"unknown"`.
- Fully qualify `base::`/`utils::` because the package defines its own `package_version()` helper.

## Prevention

- Never pass untrusted strings to strict `package_version()`; use `compareVersion` + `tryCatch`, or a regex validation step first.
- Degrade per-row to a defined status (`"unknown"`) instead of letting one bad value crash a whole report.
- Add a test feeding malformed versions (`"v2.0.1"`, `""`) and assert `"unknown"`.
- Prefer fixing in `get_package_version()` where the `package`+`branch` context is available to name the offending file.

## Related

- `.cg-docs/solutions/bugs/2026-08-14-collapse-fcase-fifelse-not-exported.md`
- `.cg-docs/plans/2026-08-13-performance-tests-release.md`
- base `package_version()` vs `utils::compareVersion()` docs
