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

## Work in Progress
<!-- Modules, features, or migrations currently underway -->

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->

## Wiki Configuration
<!-- folder: wiki -->
<!-- audience: developers | researchers | end-users -->
<!-- tone: technical | conversational | formal -->