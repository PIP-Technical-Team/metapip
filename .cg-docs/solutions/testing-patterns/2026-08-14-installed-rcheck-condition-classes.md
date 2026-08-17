---
date: 2026-08-14
title: "stop(class=) conditions are not inheritable for tryCatch handlers - use rlang::abort"
category: "testing-patterns"
language: "R"
tags: [tryCatch, rlang, abort, condition-classes, R CMD check, load_all, mockery]
root-cause: "stop('msg', class = 'foo') does not create a condition inheriting class 'foo' the way tryCatch handlers expect; rlang::abort(..., class = 'foo') does"
severity: "P2"
plan: ".cg-docs/plans/2026-08-14-lock-down-supply-chain.md"
reviewed-in: ".cg-docs/reviews/2026-08-12-unblock-the-core-verify-review-2.md"
---

# stop(class=) conditions are not inheritable for tryCatch handlers - use rlang::abort

## Problem

Unit tests for `check_github_token()` simulated `gitcreds` failures with:

```r
mockery::stub(check_github_token, "gitcreds::gitcreds_get", function() {
  stop("no git", class = "gitcreds_nogit_error")
})
expect_error(check_github_token(), "No git installation found")
```

These passed under `devtools::load_all()`, but **failed in `R CMD check`**
(installed-package context) with the raw error leaking through:

```
Error in `gitcreds::gitcreds_get()`: no gitgitcreds_nogit_error
```

## Root Cause

`stop("msg", class = "foo")` does NOT signal a condition that inherits class
`"foo"` in a way that a `tryCatch` handler named `foo` matches — in R 4.5 the
raised `simpleError` is not attributed with the `foo` class, so the
`tryCatch(... foo = function(e) ...)` handler never fires. The behavior is
context-dependent enough that it passed under `load_all` (different evaluation
path) but broke in the installed check. `rlang::abort(..., class = "foo")`
creates a condition that properly inherits `"foo"` and is catchable by
`tryCatch` via class name.

## Solution

Construct failure conditions with `rlang::abort` when a specific condition
class must be catchable:

```r
mockery::stub(check_github_token, "gitcreds::gitcreds_get", function() {
  rlang::abort("no git", class = "gitcreds_nogit_error")
})
```

## Prevention

- When a code path catches errors by class name (`tryCatch(..., customclass = ...)`),
  simulate it with `rlang::abort(..., class = ...)`, not `stop(msg, class = ...)`.
- Always validate R package changes with an **installed-context** check
  (`Rscript -e "rcmdcheck::rcmdcheck()"` or `R CMD check`), not only
  `devtools::load_all()`/`devtools::test()` — internal-symbol visibility and
  condition semantics differ (e.g. internal functions are referenced via the
  namespace under load_all but via `pkg::fun` in the installed check).
- Use `rcmdcheck(..., check_dir = <persistent>)` when you need to read
  `tests/testthat.Rout.fail` and `_problems/` after a failed run, because
  rcmdcheck cleans its temp check dir on exit.

## Related

- `.cg-docs/solutions/build-errors/2026-08-14-system-file-empty-write-csv-stdout.md`
- `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`
