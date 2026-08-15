# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Data Sources
<!-- Where does data come from? File paths, databases, APIs, vintage conventions -->

## Domain Rules
<!-- Project-specific rules that Copilot should always follow -->

### R: data.table-collapse dialect gotchas (discovered 2026-08-13)

- `fcase()` / `fifelse()` come from **data.table**, not collapse. collapse does not export them; use data.table's (`import(data.table, except = fdroplevels)` is in NAMESPACE).
- `collapse::join(..., how = "full")` returns only the joined columns — it never creates a status column. Compute comparisons explicitly (e.g., `utils::compareVersion` via `mapply`; it is scalar, not vectorized).
- GitHub timestamps (`...Z`) must be parsed with `as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")` — `%OS` handles optional fractional seconds; never rely on host TZ.
- Prefer `cli::cli_warn()` over `cli::cli_alert_warning()` when a test must `expect_warning()`.

### Supply-chain / install conventions (discovered 2026-08-14)

- A committed team lock manifest lives at `inst/PIP_LOCK.csv` (`package,branch,sha`).
  `pip_snapshot()` writes/refreshes it; `init_metapip()` installs from it (falls
  back to branch HEAD when absent); `update_pip_packages()` refreshes it.
  `options(metapip.lock_path)` overrides the write target; `pip_lock_path()`
  (read-only, via `system.file`) is the read path.
- `system.file()` returns `""` when a file is absent — never rely on it for a
  default write target without `nzchar()` guarding (and `write.csv(x, "")`
  silently writes to stdout on Windows).
- Least-privilege tokens: read-only GitHub functions work without a PAT;
  install functions gate on `check_github_token()`, which returns a redacted
  `metapip_token` (env-var first via `gh_token()`, gitcreds fallback).
- R testing: `stop(msg, class = "foo")` does NOT produce a condition a
  `tryCatch` handler named `foo` inherits — use `rlang::abort(..., class=)`;
  validate with an installed `rcmdcheck`, not just `load_all()`, since
  internal-symbol/condition semantics differ.
- `mapply(compare_sha, ...)` resists per-argument mockery mocks — keep mock
  return values single-valued per test block (see
  `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`).

## Work in Progress
<!-- Modules, features, or migrations currently underway -->

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->

## Wiki Configuration
<!-- folder: wiki -->
<!-- audience: developers | researchers | end-users -->
<!-- tone: technical | conversational | formal -->