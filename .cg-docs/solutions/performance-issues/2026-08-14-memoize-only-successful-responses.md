---
date: 2026-08-14
title: "Memoize only successful API responses to avoid session cache poisoning"
category: "performance-issues"
language: "R"
tags: [memoization, cache, tryCatch, gh, rate-limit, retry]
root-cause: "A session-scoped memoization cache stored error fallbacks (NA payloads), so one transient API failure silently poisoned all later calls"
severity: "P1"
---

# Memoize only successful API responses to avoid session cache poisoning

## Problem

Per-session memoization of GitHub API calls in metapip initially cached the **error fallback** returned by `tryCatch`. If the first call for a key hit a transient failure (rate limit 403, network blip, 5xx), the NA payload was written to the cache and **every subsequent call in the session silently reused it** instead of retrying. Via `latest_commit_for_branch()` this made `compare_sha()` return `NULL`, so `update_pip_packages()` wrongly reported "branch not available" and never raised the install prompt — for the rest of the session, with no error surfaced.

## Root Cause

The `cache_set()` call wrapped the `tryCatch()` expression, so both the success value *and* the fallback were cached:

```r
out <- tryCatch(gh::gh(...), error = function(e) <NA-fallback>)
cache_set(key, out)   # caches the fallback on failure too
```

## Solution

Return the fallback but only cache genuine successes (and, optionally, genuine negative results such as 404 gh_errors where "no release"/"no branch" is the true, stable answer):

```r
key <- "release:pipapi"
dat <- cache_get(key)
if (is.null(dat)) {
  ok <- FALSE
  dat <- tryCatch({
    out <- gh::gh("GET /repos/{owner}/{repo}/releases/latest", owner = "PIP-Technical-Team", repo = pkg)
    ok <- TRUE            # plain `<-`; `<<-` misses the caller frame in tryCatch
    out
  }, error = function(err) {
    if (inherits(err, "gh_error") && isTRUE(err$status_code == 404L)) ok <<- TRUE
    data.frame(tag_name = NA_character_, published_at = NA_character_)
  })
  if (ok) cache_set(key, dat)
}
```

Two important R details:

- **`ok <- TRUE` inside the `tryCatch` expression must use plain `<-`, not `<<-`.** `<<-` walks the lexical enclosing environments, so inside a `lapply\(i)` block it does not reach the outer frame's `ok`; the value silently stays `FALSE` and nothing is cached. Plain `<-` assigns to the caller frame where `ok` was initialized.
- **Detecting 404 via `gh_error`**: gh aborts with class `gh_error` carrying `$status_code`. A 404 ("doesn't exist") is a stable negative result worth caching; other errors should NOT be cached so the next call retries.

## Prevention

- Memoize at the API-call site with an explicit success flag; never `cache_set` an error fallback.
- Cache permanent negatives explicitly (404), transient failures never.
- Test both paths: a success memoization test (call counter stays 1) and an error path test (call counter increments on retry).
- Document that restarting R is the hard cache-invalidation mechanism; `install_branch()` clears per-package keys after install.

## Related

- `.cg-docs/plans/2026-08-13-performance-tests-release.md`
- `.cg-docs/solutions/bugs/2026-08-14-collapse-fcase-fifelse-not-exported.md`
- gh package `gh_error` class / `$status_code`
