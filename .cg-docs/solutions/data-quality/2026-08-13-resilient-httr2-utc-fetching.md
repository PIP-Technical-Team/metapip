---
date: 2026-08-13
title: "Resilient httr2 fetching, explicit-UTC timestamps, and safe subsetting in R"
category: "data-quality"
language: "R"
tags: [httr2, timezone, POSIXct, collapse, data.table, fcase, timestamp, http, bugfix]
root-cause: "read.dcf(url()) opened unclosed connections with no timeout; GitHub UTC timestamps were parsed as local time; ss(1L) errored on 0-row subsets; collapse does not export fcase and collapse::join never mints the status column the code referenced."
severity: "P1"
---

# Resilient httr2 fetching, explicit-UTC timestamps, and safe subsetting in R

## Problem

Three fragility classes in an R package's remote-data layer:

1. `read.dcf(url(y))` had no timeout, no HTTP error handling, and leaked connections.
2. GitHub timestamps like `2026-01-15T12:30:00Z` were parsed with `as.POSIXct(x, format = "%Y-%m-%dT%T")` — no `tz`, so "12:30 UTC" silently became "12:30 local" (5h off on `America/New_York`, and different on every user machine).
3. `fsubset(...) |> ss(1L)` errored when filtering removed every row (e.g., a package with only a `gh-pages` branch), and `core_metadata()` consumed the result unsafely.

## Root Cause

- `read.dcf(url())` relies on base-R connection handling with no failure surface for HTTP status codes or timeouts.
- `as.POSIXct()` on an ISO string without `tz = "UTC"` interprets the string in the host timezone, so the same data renders differently per machine.
- `ss(1L)` (collapse) on an empty result aborts; there was no empty-branch guard.
- Bonus landmine: **`fcase`/`fifelse` are NOT exported by `collapse`** — they come from `data.table`. Code that wrote `fcase(...)` with only `collapse` imported failed at runtime with `could not find function "fcase"`. Additionally, `collapse::join()` does **not** create a numeric `local_status` column as assumed; a derived status column must be computed explicitly.

## Solution

**If you do this again, the fast path is:**

```r
# httr2 fetch with timeout + graceful failure (replace read.dcf(url()))
get_version_for_url <- function(u) {
  tryCatch(
    {
      resp <- httr2::request(u) |>
        httr2::req_timeout(seconds = 10) |>
        httr2::req_perform()
      if (httr2::resp_status(resp) != 200L) return(NA_character_)
      tc <- suppressWarnings(textConnection(httr2::resp_body_string(resp)))
      on.exit(close(tc), add = TRUE)
      mat <- suppressWarnings(read.dcf(tc))
      if (!"Version" %in% colnames(mat)) return(NA_character_)
      unname(mat[, "Version"])
    },
    error = function(e) NA_character_          # 404/timeout/5xx/network all -> NA
  )
}
```

*Gotcha:* `httr2::req_perform()` already errors on 4xx/5xx, so a wrapper `tryCatch` returning `NA` is what actually absorbs 404s; an explicit `resp_status != 200L` check is mostly dead code for 4xx/5xx (still harmless). `read.dcf()` has **no** `text=` argument — you must use a `textConnection` (some reviewers will suggest `read.dcf(text=...)`, which errors). Wrap the connection and the `read.dcf` in `suppressWarnings()` because malformed DCF emits warnings, not errors.

```r
# Explicit-UTC parsing with optional fractional seconds
as.POSIXct(last_update_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
# %OS accepts optional fractional seconds; the literal Z requires %OSZ;
# always pass tz = "UTC" explicitly (never rely on host TZ)

# Empty-branch guard
res <- out |> fsubset(branch_name != "gh-pages")
if (nrow(res) == 0L) {
  cli::cli_warn("No non-gh-pages branches found for {.pkg {package}}")  # cli_warn, not cli_alert_warning
  return(invisible(data.frame(
    package = package, branch_name = NA_character_,
    last_commit_author_name = NA_character_,
    last_update_time = as.POSIXct(NA, tz = "UTC")
  )))
}
```

*Gotcha:* `cli::cli_alert_warning()` prints an alert but does **not** signal a condition, so `expect_warning()` in tests never fires. Use `cli::cli_warn()` (or `warning()`) when the plan/tests require a catchable warning.

**Status column with `fcase`:**

```r
join(local, dev, "package", how = "full") |>
  fmutate(cmp = mapply(function(a, b) {
    if (is.na(a) || is.na(b)) return(0L)
    utils::compareVersion(a, b)
  }, local_version, version, SIMPLIFY = TRUE)) |>
  fmutate(local_status = fcase(
    is.na(local_version), "Not in local",
    is.na(branch),       paste(branch_to_compare, "not in repo"),
    is.na(version),      paste(branch_to_compare, "version unknown"),
    cmp < 0,             paste("behind", branch_to_compare),
    cmp > 0,             paste("ahead", branch_to_compare),
    default = "up-to-date"
  )) |>
  fselect(-branch, -version, -cmp)
```

Key facts that made this necessary:
- Collapse does NOT export `fcase`/`fifelse`; add `data.table` to Imports (or `import(data.table, except = fdroplevels)` in NAMESPACE) to get `fcase`.
- `collapse::join(..., how = "full")` returns only the joined columns — it never creates a `local_status` numeric column out of thin air; compute the comparison yourself.
- `utils::compareVersion()` is scalar, not vectorized — wrap it in `mapply` before feeding `fcase` (which evaluates all branches eagerly, so NAs would otherwise error).
- Always add an `is.na(version)` status for the *compared* branch so a failed fetch reads "version unknown", never a false "up-to-date".

## Testing

Herme-ify tests that call functions guarded by `check_github_token()` (-> `gitcreds`): stub the token check or they silently depend on each developer's machine having stored credentials.

```r
mockery::stub(get_latest_branch_update, "check_github_token",
              function() invisible(list(username = "x", password = "x")))
```

For httr2, use `httr2::with_mocked_responses()` (httr2 >= 1.0.0); a mock may dispatch on `req$url` (e.g., `grepl("deleted-branch", req$url)`) and return different `httr2::response(status_code = ..., body = charToRaw(...))` objects per URL. Assert on `attr(x, "tzone") == "UTC"` and numeric parity with a known UTC instant under `withr::local_envvar(TZ = "America/New_York")`.

## Prevention

- Prefer `httr2` over `read.dcf(url())`; always set `req_timeout()`.
- Always pass `tz = "UTC"` when parsing ISO strings that carry a `Z`; use `%OS` for optional fractional seconds.
- Never `ss(1L)` / take the head of a subset you haven't guarded for 0 rows.
- Remember the dialect: `fcase`/`fifelse` are `data.table`, `collapse::join` does not mint status columns, and `compareVersion` is scalar.
- Prefer `cli::warn()` over `cli::alert_warning()` when a test must `expect_warning()`.

## Related

- `.cg-docs/plans/2026-08-12-resilient-networking-time.md`
- `.cg-docs/reviews/2026-08-12-resilient-networking-time-review.md`
- `.cg-docs/solutions/data-quality/2026-08-12-rebuild-named-list-branches.md`
