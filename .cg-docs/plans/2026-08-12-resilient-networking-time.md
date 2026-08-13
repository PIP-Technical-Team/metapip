---
date: 2026-08-12
title: "Resilient Networking & Time — httr2 fetch, UTC timestamps, empty-branch guard"
status: completed
completed-date: 2026-08-13
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
execution-report: .cg-docs/work-reports/2026-08-13-resilient-networking-time.md
tags: [httr2, timestamps, robustness, milestone-4]
---

# Plan: Resilient Networking & Time

## Objective

Replace fragile `read.dcf(url())` HTTP fetching with httr2 (timeouts, error handling, connection safety), fix UTC timestamp parsing in `get_latest_branch_update()` and `core_metadata()`, and guard against empty-branch subsetting in `get_latest_branch_update()`.

## Context

An engineering review identified three fragility classes in metapip's remote data handling:
- `get_package_version()` uses `read.dcf(url(y))` with no timeout, no HTTP error handling, and unclosed connections.
- `get_latest_branch_update()` parses GitHub UTC timestamps without `tz = "UTC"`, silently interpreting them as local time.
- `get_latest_branch_update()` calls `ss(1L)` after filtering `gh-pages`, which errors if the result has 0 rows.

These belong to milestone 4 ("Resilient Networking & Time") in `roadmap.json`. The fixes are independent of milestones 1-3 and can be developed in parallel once V1-V4 fixes are merged.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R4 | Replace `read.dcf(url())` in `get_package_version()` with httr2; add timeout, HTTP status check, connection safety | Engineering review R4 |
| R5 | Parse timestamps with explicit `tz = "UTC"` in `get_latest_branch_update()` and `core_metadata()` | Engineering review R5 |
| R6 | Guard `ss(1L)` on 0-row result in `get_latest_branch_update()`; return warning + empty NA row | Engineering review R6 edge |
| R7 | Add `httr2` to DESCRIPTION Imports | R4 dependency |
| R8 | Write tests for each fix using mockery + withr | Testing convention |

## Implementation Steps

### 1. Add httr2 helper and refactor get_package_version()
- **Requirements**: R4, R7
- **Files**: `R/package_branches.R`, `DESCRIPTION`
- **Details**:
  - Add `httr2` to `Imports` in DESCRIPTION.
  - Create internal helper `get_version_for_url(u)` in `R/package_branches.R` (annotate `@noRd`):
    - `httr2::request(u) |> httr2::req_timeout(seconds = 10) |> httr2::req_perform()`
    - Check `httr2::resp_status(resp) == 200L`; if not, return `NA_character_`
    - Parse with `tc <- textConnection(httr2::resp_body_string(resp)); on.exit(close(tc), add = TRUE)` then `read.dcf(tc)`
    - Guard missing Version field: `if (!"Version" %in% colnames(mat)) return(NA_character_)`
    - Return `mat[, "Version"]`
    - Wrap entire body in `tryCatch(..., error = function(e) NA_character_)`; return `NA_character_` on any failure
  - Replace `sapply(urls, \(y) { mat <- read.dcf(url(y)); mat[, "Version"] })` with `vapply(urls, get_version_for_url, character(1L))`.
- **Test Scenarios**: happy path (200 response returns version string), HTTP 404 returns NA_character_, timeout returns NA_character_, malformed DESCRIPTION (no Version field) returns NA_character_, network error returns NA_character_
- **Tests**: Add tests in `tests/testthat/test-package_branches.R`. Use `httr2::with_mocked_responses()` to mock HTTP responses — one mock returning 200 with valid DESCRIPTION body, one returning 404, one returning malformed body. No live network.
- **Acceptance criteria**: `get_package_version("pipapi")` completes without error even when some branches return 404; versions for valid branches are correct character strings; `textConnection` is closed after each call (no resource leak).

### 2. Fix UTC timestamp parsing in get_latest_branch_update()
- **Requirements**: R5
- **Files**: `R/get_branches.R`
- **Details**:
  - Line 103: change `as.POSIXct(last_update_time, format = "%Y-%m-%dT%T")` to `as.POSIXct(last_update_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")`.
  - The `%OS` format specifier handles optional fractional seconds (e.g., `2026-01-15T12:30:00.123Z`).
  - Verify the `tzone` attribute of the resulting column is `"UTC"`.
- **Test Scenarios**: timestamp "2026-01-15T12:30:00Z" parses to UTC 12:30 (not local time); tzone attribute is "UTC"; fractional seconds "2026-01-15T12:30:00.500Z" also parse correctly.
- **Tests**: Add tests in `tests/testthat/test-get_branches.R`. Mock `get_branch_info` via `mockery::stub()` to return a data.frame with a known UTC timestamp string. Run inside `withr::local_envvar(TZ = "America/New_York")` to verify the result is UTC 12:30 (not EST 17:30). Assert `tzone(result$last_update_time) == "UTC"`.
- **Acceptance criteria**: `get_latest_branch_update("pipapi")$last_update_time` has `tzone == "UTC"`.

### 3. Fix UTC timestamp parsing in core_metadata()
- **Requirements**: R5
- **Files**: `R/core_metadata.R`
- **Details**:
  - Line 55: change `as.POSIXct(sapply(latest_commit, `[[`, "last_update_time"))` to `as.POSIXct(sapply(latest_commit, `[[`, "last_update_time"), tz = "UTC")`.
  - Ensure consistency with `get_latest_branch_update()` output — both must produce POSIXct with `tzone == "UTC"`.
- **Test Scenarios**: `core_metadata("pipapi")$latest_commit_time` has `tzone == "UTC"`; timestamps match those from `get_latest_branch_update()`.
- **Tests**: Add tests in `tests/testthat/test-core_metadata.R`. Mock `get_branches`, `gh::gh` (for releases), and `get_latest_branch_update` via `mockery::stub()` to return controlled data. Use `withr::local_envvar(TZ = "America/New_York")`. Assert `tzone(result$latest_commit_time) == "UTC"`.
- **Acceptance criteria**: `core_metadata()` timestamps are consistent with `get_latest_branch_update()` timestamps (both UTC).

### 4. Guard ss(1L) on empty branch set
- **Requirements**: R6
- **Files**: `R/get_branches.R`
- **Details**:
  - After `fsubset(branch_name != "gh-pages")` (line 102), check `nrow(res) == 0L`.
  - If empty: `cli::cli_alert_warning("No non-gh-pages branches found for {.pkg {package}}")`, return `invisible(data.frame(package = package, branch_name = NA_character_, last_commit_author_name = NA_character_, last_update_time = as.POSIXct(NA, tz = "UTC")))`.
  - Otherwise proceed with existing `fmutate` + `roworder` + `ss(1L)`.
- **Test Scenarios**: package with only gh-pages branch returns warning + 1-row NA data.frame with correct columns; normal case (multiple non-gh-pages branches) still returns single row with latest update; single non-gh-pages branch returns that row without error.
- **Tests**: Add tests in `tests/testthat/test-get_branches.R`. Mock `get_branch_info` via `mockery::stub()` to return a data.frame with only `gh-pages` rows. Assert `expect_warning()`, `nrow(result) == 1L`, all columns present, `is.na(result$branch_name)`. Also test the happy path with 3 branches (mock returns 3 non-gh-pages rows) and verify `ss(1L)` still selects the latest.
- **Acceptance criteria**: `get_latest_branch_update()` on a package with only `gh-pages` returns a warning and a single-row data.frame with NAs, no error.

### 5. Update NEWS.md and verify full check
- **Requirements**: R4, R5, R6, R7, R8
- **Files**: `NEWS.md`
- **Details**:
  - Add changelog entries under a new `# metapip 0.0.3` header:
    - `get_package_version()` now uses httr2 with timeouts and graceful error handling
    - Timestamp parsing uses explicit UTC timezone in `get_latest_branch_update()` and `core_metadata()`
    - `get_latest_branch_update()` handles packages with only gh-pages branches gracefully
    - Added httr2 to Imports
  - Run `rcmdcheck::rcmdcheck()` and verify no new NOTEs/WARNINGs/ERRORs.
- **Test Scenarios**: N/A (documentation + verification step)
- **Tests**: `Rscript -e "rcmdcheck::rcmdcheck()"`
- **Acceptance criteria**: R CMD check passes clean; NEWS.md has entries for all three fixes.

## Testing Strategy

- **Mocking httr2**: Use `httr2::with_mocked_responses()` (httr2 >= 1.0.0) for `get_version_for_url()` tests — the purpose-built mock mechanism for httr2, more stable than `mockery::stub()` on pipe chains. Provide mock functions returning `httr2::response(status_code = 200L, body = charToRaw(description_text))` for happy path, `httr2::response(status_code = 404L)` for not-found.
- **Mocking non-httr2**: Use `mockery::stub()` for `gh::gh`, `get_branch_info`, `get_branches`, and `get_latest_branch_update` calls in Steps 2-4. These are standard function stubs on internal/package functions.
- **Timezone isolation**: Use `withr::local_envvar(TZ = "America/New_York")` inside tests to verify UTC parsing. This works because the fixed code passes `tz = "UTC"` explicitly; the test verifies the result is NOT shifted to EST.
- **Edge cases**: HTTP 404, timeout, malformed DESCRIPTION (missing Version field), empty branch set after filtering, single-branch package, fractional-second timestamps.
- **Existing tests**: Existing tests in `test-get_branches.R`, `test-package_branches.R`, and `test-core_metadata.R` must continue to pass. The `skip("avoid live network")` tests are already skipped; no changes needed to them.

## Documentation Checklist

- [ ] `NEWS.md` updated with all three fixes
- [ ] Roxygen2 comments on `get_version_for_url()` (internal, `@noRd`)
- [ ] `DESCRIPTION` Imports updated with `httr2`
- [ ] `devtools::document()` run if roxygen2 changes

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| httr2 version conflict with existing gh dependency | Low | High | httr2 is already a transitive dep of gh; check version constraints in R CMD check |
| `httr2::with_mocked_responses()` unavailable (httr2 < 1.0.0) | Low | Medium | Check installed httr2 version; fall back to `mockery::stub()` on `httr2::req_perform` if needed |
| withr timezone override doesn't affect POSIXct parsing on all platforms | Low | Medium | Test on Windows CI (already planned in milestone 5); use `tz` parameter directly which is platform-independent |
| `textConnection` in `get_version_for_url` leaks on error | Low | Low | Use `tc <- textConnection(...); on.exit(close(tc), add = TRUE)` pattern — implemented in Step 1 |
| `read.dcf` returns matrix without "Version" column for malformed DESCRIPTION | Low | Medium | Guard with `if (!"Version" %in% colnames(mat)) return(NA_character_)` — implemented in Step 1 |

## Out of Scope

- Other milestones (install pipeline hardening, supply chain, performance)
- Pagination improvements for `gh::gh()` calls
- Memoization of API calls
- CI configuration changes

## Completion Contract

### Outcome

`get_package_version()` fetches DESCRIPTION files via httr2 with timeouts and graceful 404 handling; all timestamp parsing uses explicit UTC timezone; `get_latest_branch_update()` returns a safe empty row instead of erroring on filtered-empty branch sets.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | httr2 helper returns NA on HTTP 404 | `Rscript -e "devtools::test()"` | yes |
| V2 | httr2 helper returns version on 200 | `Rscript -e "devtools::test()"` | yes |
| V3 | `get_latest_branch_update()` timestamp has `tzone == "UTC"` | `Rscript -e "devtools::test()"` | yes |
| V4 | `get_latest_branch_update()` on empty-branch pkg returns warning + NA row | `Rscript -e "devtools::test()"` | yes |
| V5 | `core_metadata()` timestamps are UTC | `Rscript -e "devtools::test()"` | yes |
| V6 | `package_branches("pipapi")` handles deleted branches without abort | `Rscript -e "devtools::test()"` | yes |
| V7 | DESCRIPTION has httr2 in Imports | `Rscript -e "devtools::check()"` | yes |
| V8 | R CMD check passes with no new NOTEs/WARNINGs/ERRORs | `Rscript -e "rcmdcheck::rcmdcheck()"` | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | httr2 must be in Imports, not Suggests | DESCRIPTION file |
| C2 | No new dependency conflicts with existing gh/gitcreds versions | R CMD check |
| C3 | collapse, cli, glue conventions maintained | Code review |
| C4 | httr2 tests use `httr2::with_mocked_responses()`; non-httr2 tests use `mockery::stub()`; timezone tests use `withr::local_envvar(TZ = ...)` | Test files |
| C5 | Changes produce a single mergeable PR targeting master | Git log |
| C6 | `get_version_for_url()` closes `textConnection` via `on.exit()` | Code review |

### Boundaries

- Allowed: `R/package_branches.R`, `R/get_branches.R`, `R/core_metadata.R`, `DESCRIPTION`, `tests/testthat/test-*.R`, `NEWS.md`
- Out of scope: Other milestones, pagination, memoization, CI changes

### Blocked-Stop Conditions

- httr2 cannot be installed or conflicts with existing deps
- `httr2::with_mocked_responses()` is unavailable (httr2 < 1.0.0) and fallback to `mockery::stub()` also fails
- R CMD check produces ERRORs unrelated to this change

### Iteration Policy

1. Each step is atomic: implement, test, verify before moving on.
2. Implement R4 (httr2 fetch) first; run tests after.
3. Implement R5 (UTC timestamps) second; run tests after.
4. Implement R6 (empty-branch guard) third; run tests after.
5. Run full `rcmdcheck::rcmdcheck()` as final verification.
6. If a step fails verification, fix it in-place before proceeding.
7. If a blocked-stop condition is hit, halt and report to user.
