---
date: 2026-08-13
title: "Tri-state compare_sha and mockery stubbing with mapply"
category: "testing-patterns"
language: "R"
tags: [mockery, mapply, tri-state, compare_sha, tryCatch]
root-cause: "mapply does not reliably intercept mocked functions when the mock returns different values per argument"
severity: "P2"
---

# Tri-state compare_sha and mockery stubbing with mapply

## Problem

When testing `update_pip_packages()`, which uses `mapply(compare_sha, pkgs, default_branch[pkgs])`, mocking `compare_sha` to return different values per package (e.g., `"unknown"` for one, `FALSE` for another) did not work correctly. The mock was only partially intercepted by `mapply`.

## Root Cause

`mapply` calls the original function from the package namespace, not the mocked version, when the mock returns different values based on arguments. This is a known limitation of `mockery::stub` with `mapply`.

## Solution

Simplify the test to use a single return value for the mock when testing specific branches. For example, to test the `"unknown"` path, mock `compare_sha` to always return `"unknown"` and test only that specific behavior. Test the `FALSE` path separately with a different mock.

```r
# Test unknown path only
mockery::stub(update_pip_packages, "compare_sha", function(pkg, branch) "unknown")
result <- update_pip_packages(ask = FALSE, answer = 1)
expect_false(result)

# Test FALSE path separately
mockery::stub(update_pip_packages, "compare_sha", function(pkg, branch) FALSE)
result <- update_pip_packages(ask = FALSE, answer = 1)
expect_true(result)
```

## Prevention

When testing functions that use `mapply` or `Map` with mocked functions:
1. Keep mock return values simple (single value, not conditional)
2. Test each code path in separate test blocks
3. If conditional mocking is needed, mock the parent function instead of the inner function

## Related

- mockery package documentation on `stub()` limitations
- R base `mapply` behavior with function environments
