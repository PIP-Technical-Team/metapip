# Changelog

<!-- cg:auto:version-history -->
## 0.0.3

- Version fetching (`get_package_version()`, used by `package_branches()`)
  now uses `httr2` with a 10-second timeout and graceful error handling,
  returning `NA` on 404, malformed, or timed-out responses.
- Explicit UTC timezone parsing: `get_latest_branch_update()` and
  `core_metadata()` return timestamps as `POSIXct` in UTC.
- `get_latest_branch_update()` handles packages with only `gh-pages` branches
  gracefully (warns and returns a single-row `NA` data.frame).
- New runtime dependencies: `httr2` (>= 1.0.0) and `data.table`.

## 0.0.1

- Initial release. Meta-package that manages installation, updating, and
  inspection of the core PIP packages via the GitHub API.
<!-- cg:auto:end -->

← [Home](README.md)
