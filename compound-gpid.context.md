# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely
— this file is committed to git and shared with the team.

## Data Sources

## Domain Rules

### R: data.table-collapse dialect gotchas (discovered 2026-08-13)

- `fcase()` / `fifelse()` come from **data.table**, not collapse.
  collapse does not export them; use data.table’s
  (`import(data.table, except = fdroplevels)` is in NAMESPACE).
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

## Work in Progress

## Workspace Notes

## Wiki Configuration
