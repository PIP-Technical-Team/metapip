# API Reference

<!-- cg:auto:functions -->
Key exported functions:

| Function | Purpose |
|----------|---------|
| `install_pip_packages()` | Install all core PIP packages |
| `install_latest_branch()` | Install the latest branch of each package (developer-only; bypasses the team lock) |
| `install_branch(package, branch, force = FALSE, sha = NULL)` | Install a specific branch of a package; pins to the resolved branch HEAD SHA by default, `force = TRUE` installs live HEAD, `sha =` overrides the commit |
| `update_pip_packages()` / `metapip_update()` | Update installed PIP packages and refresh the `PIP_LOCK` lock manifest |
| `pip_snapshot()` | Write or refresh the team lock manifest (`PIP_LOCK.csv`, columns `package,branch,sha`); aborts when no write target can be resolved |
| `init_metapip()` | Lock-driven install of each package at its recorded SHA; falls back to branch HEAD when the lock is absent |
| `get_branches()` / `get_branch_info()` / `get_current_branches()` | Inspect available branches |
| `get_latest_branch_update()` | Latest commit's branch, author, and timestamp per package |
| `get_default_branch()` / `set_default_branch()` | Get/set the default branch |
| `get_custom_branch()` / `set_custom_branch()` | Get/set per-package branch overrides |
| `get_core_packages()` | List the core PIP packages (correctly-spelled name) |
| `get_core_pagkages()` | Deprecated alias, retained for backward compatibility (typo) |
| `core_metadata()` / `metapip_packages()` | Package metadata and listing |
| `package_branches()` | Status table of local and remote versions, incl. a `local_status` column |
| `pkg_deps()` | Package dependency details |
| `check_github_token()` | Verify GitHub credentials; returns a redacted `metapip_token` (never the live token) |

GitHub API calls are memoized per R session, so repeated inspections reuse
cached results instead of re-hitting the network. Restart R to clear the
cache.
<!-- cg:auto:end -->

<!-- cg:auto:parameters -->
Options controlling behavior:

- `options(metapip.default_branch)` — default branch to install from (default `"PROD"`)
- `options(metapip.custom_branch = list(...))` — per-package branch overrides
- `options(metapip.lock_path)` — write target for the `PIP_LOCK` lock manifest (default: `system.file("PIP_LOCK.csv", package = "metapip")`)
<!-- cg:auto:end -->

<!-- cg:auto:return-values -->
Most inspection functions return named character vectors or data frames
describing branches, metadata, or package names. Installation and update
functions return confirmation of what was installed or updated.

Timestamps are returned as `POSIXct` in UTC: `get_latest_branch_update()`
(`last_update_time`) and `core_metadata()` (`latest_commit_time`). When a
package has only `gh-pages` branches, `get_latest_branch_update()` issues a
warning and returns a single-row data.frame filled with `NA`. Package versions
are fetched via `httr2` with a 10-second timeout, gracefully returning `NA`
on 404, malformed, or timed-out responses.

`pip_snapshot()` returns its write path invisibly. `check_github_token()`
returns a redacted `metapip_token` object whose secret fields are blanked (its
`print()` shows `""`) — the live token is never placed in the return value
(`remotes` resolves gitcreds itself), so the function acts as an install gate,
not a token carrier. The internal read-only `pip_lock_path()` returns the
committed lock path via `system.file()` (or `""` when absent), keeping read
and write paths separate.
<!-- cg:auto:end -->

← [Home](README.md)
