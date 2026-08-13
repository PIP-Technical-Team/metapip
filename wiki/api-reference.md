# API Reference

<!-- cg:auto:functions -->
Key exported functions:

| Function | Purpose |
|----------|---------|
| `install_pip_packages()` | Install all core PIP packages |
| `install_latest_branch()` | Install the latest branch of each package |
| `install_branch()` | Install a specific branch of a package |
| `update_pip_packages()` / `metapip_update()` | Update installed PIP packages |
| `get_branches()` / `get_branch_info()` / `get_current_branches()` | Inspect available branches |
| `get_latest_branch_update()` | Latest commit's branch, author, and timestamp per package |
| `get_default_branch()` / `set_default_branch()` | Get/set the default branch |
| `get_custom_branch()` / `set_custom_branch()` | Get/set per-package branch overrides |
| `core_metadata()` / `get_core_pagkages()` / `metapip_packages()` | Package metadata and listing |
| `pkg_deps()` | Package dependency details |
| `check_github_token()` | Verify GitHub credentials |
<!-- cg:auto:end -->

<!-- cg:auto:parameters -->
Options controlling behavior:

- `options(metapip.default_branch)` — default branch to install from (default `"PROD"`)
- `options(metapip.custom_branch = list(...))` — per-package branch overrides
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
<!-- cg:auto:end -->

← [Home](README.md)
