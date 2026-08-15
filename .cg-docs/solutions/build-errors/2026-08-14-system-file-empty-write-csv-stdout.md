---
date: 2026-08-14
title: "system.file() empty path and write.csv-to-stdout gotcha for default file targets"
category: "build-errors"
language: "R"
tags: [system.file, write.csv, install.packages, default-path, PIP_LOCK, load_all]
root-cause: "system.file() returns '' when the requested file is absent, and write.csv(x, '') silently writes to stdout on Windows instead of raising an error"
severity: "P2"
plan: ".cg-docs/plans/2026-08-14-lock-down-supply-chain.md"
reviewed-in: ".cg-docs/reviews/2026-08-14-lock-down-supply-chain-review.md"
---

# system.file() empty path and write.csv-to-stdout gotcha

## Problem

`pip_snapshot()` and `update_pip_packages()` were designed to write a team lock
manifest (`PIP_LOCK.csv`) to a default path derived from
`system.file("PIP_LOCK.csv", package = "metapip")`. With no committed
`inst/PIP_LOCK.csv` (and no `inst/` directory), `system.file()` returned `""`.
Calling `utils::write.csv(lock_df, "")` then:

- wrote the CSV **to stdout** on Windows (no file created, no error), and
- the surrounding `cli_alert_success("Wrote PIP_LOCK to ...")` reported success
  with an empty path — a silent false-success.

The same root cause silently disabled the lock-driven install path: `file.exists("")`
and `nzchar("")` are both `FALSE`, so `init_metapip()` could never enter its
lock-driven branch, and `update_pip_packages()` never refreshed a lock.

## Root Cause

`system.file(file, package = pkg)` returns `""` when the file is not installed.
`write.csv()` to `file = ""` is treated by base R as writing to a connection
that is created from the empty string, which on Windows resolves to stdout
(same as `file("")`), so no error is raised. The code trusted a "resolved"
path that was actually empty.

## Solution

Guard the resolved path defensively and never trust `system.file()` to exist:

```r
path <- getOption("metapip.lock_path",
                  system.file("PIP_LOCK.csv", package = "metapip"))
if (!nzchar(path)) {
  cli::cli_abort("Could not resolve a PIP_LOCK write target. Pass {.arg path} explicitly or set metapip.lock_path.")
}
```

Plus: ship a committed `inst/PIP_LOCK.csv` so `system.file()`/`pip_lock_path()`
resolve to a real file, and add an all-SHAs-unresolvable guard before
`rowbind()` (an empty list also crashes `collapse::rowbind`).

## Prevention

- Never assume a `system.file()`-derived default exists; guard with
  `nzchar(p) && file.exists(p)` before read, and `nzchar(p)` + abort before write.
- For any write target that must not silently disappear, validate the path
  before writing and fail loudly.
- Keep read (install/init) and write (`pip_snapshot`/`update_pip_packages`)
  paths separated so writes never target the installed library copy.

## Related

- `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`
- `.cg-docs/solutions/testing-patterns/2026-08-14-installed-rcheck-condition-classes.md`
