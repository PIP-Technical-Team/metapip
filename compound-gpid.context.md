# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely
— this file is committed to git and shared with the team.

## Data Sources

## Domain Rules

### R: data.table-collapse dialect gotchas (discovered 2026-08-13)

- `fcase()` / `fifelse()` come from **data.table**, not collapse.
  collapse does not export them; use data.table’s
  (`import(data.table, except = fdroplevels)` is in NAMESPACE). The
  installed `collapse` 2.1.7 does NOT export `fcase`/`fifelse` — verify
  with `"fname" %in% getNamespaceExports("collapse")` before relying on
  any collapse function.
- Prefer base `ifelse` (or
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
  for version comparison); metapip defines its own internal
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html)
  color formatter, so always fully qualify
  [`base::package_version`](https://rdrr.io/r/base/numeric_version.html)
  /
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html).
- `collapse::join(..., how = "full")` returns only the joined columns —
  it never creates a status column. Compute comparisons explicitly
  (e.g.,
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
  via `mapply`; it is scalar, not vectorized).
- GitHub timestamps (`...Z`) must be parsed with
  `as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")` — `%OS`
  handles optional fractional seconds; never rely on host TZ.
- Prefer
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
  over
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)
  when a test must `expect_warning()`.
- Only successful API responses (and genuine 404 `gh_error`s) are cached
  — never error fallbacks. GitHub API calls are memoized per R session
  (session-scoped `.metapip_cache`); restart R to invalidate, and
  [`install_branch()`](https://pip-technical-team.github.io/metapip/reference/install_branch.md)
  invalidates per-package keys after install.

### Supply-chain / install conventions (discovered 2026-08-14)

- A committed team lock manifest lives at `inst/PIP_LOCK.csv`
  (`package,branch,sha`).
  [`pip_snapshot()`](https://pip-technical-team.github.io/metapip/reference/pip_snapshot.md)
  writes/refreshes it;
  [`init_metapip()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
  installs from it (falls back to branch HEAD when absent);
  [`update_pip_packages()`](https://pip-technical-team.github.io/metapip/reference/init_metapip.md)
  refreshes it. `options(metapip.lock_path)` overrides the write target;
  [`pip_lock_path()`](https://pip-technical-team.github.io/metapip/reference/pip_lock_path.md)
  (read-only, via `system.file`) is the read path.
- [`system.file()`](https://rdrr.io/r/base/system.file.html) returns
  `""` when a file is absent — never rely on it for a default write
  target without [`nzchar()`](https://rdrr.io/r/base/nchar.html)
  guarding (and `write.csv(x, "")` silently writes to stdout on
  Windows).
- Least-privilege tokens: read-only GitHub functions work without a PAT;
  install functions gate on
  [`check_github_token()`](https://pip-technical-team.github.io/metapip/reference/check_github_token.md),
  which returns a redacted `metapip_token` (env-var first via
  [`gh_token()`](https://pip-technical-team.github.io/metapip/reference/gh_token.md),
  gitcreds fallback).
- R testing: `stop(msg, class = "foo")` does NOT produce a condition a
  `tryCatch` handler named `foo` inherits — use
  `rlang::abort(..., class=)`; validate with an installed `rcmdcheck`,
  not just `load_all()`, since internal-symbol/condition semantics
  differ.
- `mapply(compare_sha, ...)` resists per-argument mockery mocks — keep
  mock return values single-valued per test block (see
  `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`).

## Work in Progress

## Workspace Notes

## Wiki Configuration
