---
date: 2026-08-13
title: "Performance, Tests & Release — memoization, test coverage, CI hardening, ship 0.1.0"
status: completed
completed-date: 2026-08-14
scope: "Deep"
brainstorm: null
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [performance, testing, ci, release, memoization, mockery, github-actions]
phases: 3
completed-phases: [1, 2]
current-phase: 3
execution-report: .cg-docs/work-reports/2026-08-14-performance-tests-release.md
---

# Plan: Performance, Tests & Release

## Objective

Harden metapip for production use: eliminate GitHub API performance bottlenecks via per-session memoization, replace the O(N) `installed.packages()` scan with O(1) per-package checks, achieve full test coverage with no un-skipped production-path tests, add Windows CI, and ship version 0.1.0 with a clean changelog.

## Context

An engineering review (`inst/TMP/metapip-review.html`) identified performance bottlenecks, test coverage gaps, and CI/release issues. This is milestone 5 of 5 ("harden and ship"). Milestones 1-4 fix critical bugs and harden the install pipeline; this milestone assumes those fixes are complete.

Key findings from source analysis:
- `core_metadata()` makes ~80 sequential GitHub API calls (8 packages x ~10 calls each) with zero caching
- `metapip_attach()` calls `utils::installed.packages()` which scans the entire library (~1-3s); `rs_theme()` in `utils.R` also calls it
- `test-install_all_packages.R` is entirely `skip()`'d; `test-core_metadata.R` and parts of `test-get_branches.R` hit the live GitHub API un-mocked
- `pkgdown.yaml` uses `actions/checkout@v3`; no Windows CI matrix
- DESCRIPTION Version is 0.0.2; NEWS.md has incoherent version history
- `get_core_pagkages` exported with typo; `globalVariables` has duplicates and irrelevant data.table symbols; `metapip.R` has duplicate `@docType package` conflicting with `metapip-package.R`

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Per-session memoization for `get_branches()`, `latest_commit_for_branch()`, and `gh::gh()` calls in `core_metadata()` | P1 — api-memoization |
| R2 | Replace `utils::installed.packages()` with `requireNamespace()` per-package checks in `metapip_attach()` and `rs_theme()` | P2 — require-namespace-checks |
| R3 | Delete obsolete `test-install_all_packages.R` (tests nonexistent function); verify `test-install_latest_branch.R` and `test-install_pip_packages.R` coverage | unskip-install-tests |
| R4 | Mark all network-dependent tests with `skip_if_offline()` + `skip_on_cran()` | network-test-gating |
| R5 | Add Windows job to CI matrix in `codecov.yml`; pin `actions/checkout@v4` in `pkgdown.yaml`; document `GH_TOKEN` policy | windows-ci |
| R6 | Bump version to 0.1.0; rewrite NEWS.md; clean `globalVariables`; fix `get_core_pagkages` typo alias; add missing `@return` tags | version-bump-010 |

## Implementation Steps

## Phase 1: Performance

### 1. Create package-level memoization cache (P1)
- **Requirements**: R1
- **Files**: `R/cache.R` (new)
- **Details**: Create a package-level environment `.metapip_cache` (created in `.onLoad`, not locked at the binding level so entries can be added/removed). Implement `cache_get(key)`, `cache_set(key, value)`, and `cache_clear(pattern)` helpers. `cache_clear(pattern)` removes all keys matching a regex pattern (needed for post-install invalidation). The cache is session-scoped and clears on package reload. No new dependencies needed. **Test setup**: Add to `tests/testthat/setup.R`: `if (exists(".metapip_cache", envir = asNamespace("metapip"))) { rm(list = ls(metapip:::.metapip_cache), envir = metapip:::.metapip_cache) }` to ensure each test file starts with a clean cache.
- **Test Scenarios**: cache miss returns NULL; cache hit returns stored value; cache survives multiple calls in same session
- **Tests**: `tests/testthat/test-cache.R`
- **Acceptance criteria**: `cache_get("x")` returns NULL before any `cache_set`; `cache_set("x", 1); cache_get("x")` returns 1

### 2. Wrap API functions with memoization (P1)
- **Requirements**: R1
- **Files**: `R/get_branches.R`, `R/core_metadata.R`
- **Details**: Memoize at the **raw API response level**, not the function return level, to preserve `display=TRUE` output behavior. Specifically: (a) In `get_branches()`, cache the `gh::gh()` response keyed by `paste0("branches:", package)`. On cache hit, skip the API call but still run the `cli::cat_bullet()` display logic when `display=TRUE`. (b) In `latest_commit_for_branch()`, cache the `gh::gh()` response keyed by `paste0("commit:", package, ":", branch)`. (c) In `core_metadata()`, cache the `gh::gh("GET /repos/{owner}/{repo}/releases/latest", ...)` response keyed by `paste0("release:", package)`. Do NOT memoize write operations or functions that modify state. **Trade-off**: memoized API results bypass per-call `check_github_token()` validation; restarting R is the intended cache invalidation mechanism. After `install_branch()` completes, clear relevant cache entries for the installed package (e.g., `cache_clear("branches:pipapi")`, `cache_clear("commit:pipapi:...")`) to avoid stale SHA comparisons.
- **Test Scenarios**: second call to `get_branches("pipapi")` in same session returns same result without hitting GitHub; cache key isolation between packages
- **Tests**: extend `tests/testthat/test-get_branches.R`, `tests/testthat/test-core_metadata.R`
- **Acceptance criteria**: `system.time()` of second `core_metadata("pipapi")` call is significantly faster than first

### 3. Replace installed.packages() with requireNamespace() (P2)
- **Requirements**: R2
- **Files**: `R/attach.R`, `R/utils.R`
- **Details**: (a) In `R/attach.R`, replace `utils::installed.packages()` on line 36 with per-package checks using `requireNamespace(pkg, quietly = TRUE)` or `find.package(pkg, quiet = TRUE)` for each package in `core`. This is O(1) per package instead of O(N) for the full library. Preserve the existing display logic: `metapip_attach()` must still show version and branch info for each loaded package, and warn about packages not installed. (b) In `R/utils.R` line 172, replace `if ("rstudioapi" %in% rownames(utils::installed.packages()))` with `if (requireNamespace("rstudioapi", quietly = TRUE))` in the `rs_theme()` function. This eliminates a second full library scan during `.onAttach`.
- **Test Scenarios**: attach works when all packages installed; warns for missing packages; completes in <1s
- **Tests**: extend `tests/testthat/test-attach.R`
- **Acceptance criteria**: `metapip_attach()` completes in under 1 second; display output unchanged

## Phase 2: Test Coverage

### 4. Delete obsolete test-install_all_packages.R
- **Requirements**: R3
- **Files**: `tests/testthat/test-install_all_packages.R`
- **Details**: The file tests `install_all_packages(branch = "test")` which is a **nonexistent function** -- no such function exists in `R/`. The real function is `install_pip_packages()`, which already has a well-written mockery-based test file at `tests/testthat/test-install_pip_packages.R` (3 test cases: happy path, named branches, error isolation). Delete `test-install_all_packages.R` entirely. The `skip()` is removed by deletion. R3 coverage is satisfied by the existing `test-install_pip_packages.R` and the verified `test-install_latest_branch.R`.
- **Test Scenarios**: N/A (file deletion)
- **Tests**: verify `test-install_pip_packages.R` and `test-install_latest_branch.R` pass
- **Acceptance criteria**: `test-install_all_packages.R` no longer exists; zero `skip()` calls in install-related test files

### 5. Verify and extend test-install_latest_branch.R
- **Requirements**: R3
- **Files**: `tests/testthat/test-install_latest_branch.R`
- **Details**: The file already has good mockery-based tests (3 test cases: skip-at-HEAD, install-not-at-HEAD, install-when-unknown). Verify these still pass after the memoization changes in Step 2. The existing tests stub `compare_sha` at the `install_latest_branch` level, so memoization of `latest_commit_for_branch` underneath does not affect them (the stub intercepts before the memoized call). Add a test for error handling when `install_branch` throws. Also add a test that explicitly verifies memoization + stub interaction: call `install_latest_branch` with cache active, clear cache, call again, verify consistent behavior.
- **Test Scenarios**: error path when install_branch fails
- **Tests**: `tests/testthat/test-install_latest_branch.R`
- **Acceptance criteria**: 4+ test cases, all passing, zero `skip()` calls

### 6. Gate network tests with skip_if_offline() + skip_on_cran()
- **Requirements**: R4
- **Files**: `tests/testthat/test-core_metadata.R`, `tests/testthat/test-get_branches.R`, `tests/testthat/test-package_branches.R`
- **Details**: Add `skip_if_offline()` and `skip_on_cran()` guards to all tests that call the live GitHub API. Specifically: (a) `test-core_metadata.R` lines 1-6 call `core_metadata()` un-mocked -- rewrite with stubs (preferred) or add guards. (b) `test-get_branches.R` lines 38-46 and 55-63 already have `skip("avoid live network")` -- replace with proper `skip_if_offline()` + `skip_on_cran()` guards. (c) `test-package_branches.R` line 2 has `skip("avoid live network")` -- the existing stub only covers `get_branches` but `get_package_version()` internally calls `read.dcf(url(...))` which hits the network for raw DESCRIPTION files. Either stub `get_package_version` fully, or keep as a network-gated integration test with `skip_if_offline()` + `skip_on_cran()`. Prefer stubbing over skipping where feasible; use network guards only for tests that intentionally test network behavior.
- **Test Scenarios**: tests pass offline (skipped), tests pass online (executed)
- **Tests**: affected test files
- **Acceptance criteria**: no `skip("avoid live network")` remains; network tests use proper guards or are fully stubbed

### 7. Replace placeholder test-attach.R with real tests
- **Requirements**: R3
- **Files**: `tests/testthat/test-attach.R`
- **Details**: The file already has 2 real tests (`.onAttach()` calls `metapip_attach`, `.onAttach()` does not error). Extend with tests for: (a) `pkg_loaded()` returns currently loaded core packages, (b) `pkg_unloaded()` returns unloaded core packages, (c) `package_version()` returns version string, (d) `metapip_attach()` display output includes version and branch info.
- **Test Scenarios**: pkg_loaded/pkg_unloaded filtering; version formatting; attach display
- **Tests**: `tests/testthat/test-attach.R`
- **Acceptance criteria**: 5+ test cases covering attach.R exported functions

## Phase 3: CI & Release Hygiene

### 8. Pin actions/checkout@v4 in pkgdown.yaml
- **Requirements**: R5
- **Files**: `.github/workflows/pkgdown.yaml`
- **Details**: Change `uses: actions/checkout@v3` (line 25) to `uses: actions/checkout@v4`. Verify branch triggers match actual strategy: `master` is default per AGENTS.md; `main` and `DEV` in the trigger list should be verified against actual branches. Remove `main` if it does not exist.
- **Test Scenarios**: workflow YAML is valid; checkout action version is v4
- **Tests**: manual review / `yamllint` if available
- **Acceptance criteria**: `grep "actions/checkout@v3" .github/workflows/*` returns nothing

### 9. Add Windows job to CI matrix in codecov.yml
- **Requirements**: R5
- **Files**: `.github/workflows/codecov.yml`
- **Details**: Add a Windows job (`runs-on: windows-latest`) to the CI matrix. The Windows job should run `R CMD check` only (not Codecov, which requires Linux). Adjust `libcurl` system dependency step to be Linux-only (use `if: runner.os == 'Linux'`). Add a comment documenting that `secrets.GH_TOKEN` requires a fine-grained PAT with `contents:read` only.
- **Test Scenarios**: CI runs on both Ubuntu and Windows; Windows job passes
- **Tests**: push to branch and verify GitHub Actions
- **Acceptance criteria**: codecov.yml has two jobs (ubuntu + windows); GH_TOKEN comment present

### 10. Clean globalVariables and fix duplicate entries
- **Requirements**: R6
- **Files**: `R/metapip-package.R`, `R/metapip.R`
- **Details**: (a) In `R/metapip-package.R`, remove data.table symbols that are not used (`.I`, `.N`, `.SD`, `:=`) since metapip uses collapse, not data.table. Remove duplicate `"."` entry. Keep only symbols actually used in the package: `"branch"`, `"ind"`, `"local_status"`, `"values"`, `"email"`, `"version"`, `"package"`, `"PROD"`, `"local_version"`, `"branch_name"`, `"last_update_time"`, `"behind"`, `"cran"`, `"local"`. (b) In `R/metapip.R`, remove the entire `globalVariables` block (lines 20-33) since `metapip-package.R` is the canonical location. Also remove the duplicate `@docType package` and `@name metapip` roxygen lines (13-14) which conflict with the `"_PACKAGE"` sentinel in `metapip-package.R` and would cause roxygen2 collision warnings. Keep the `@importFrom glue glue` if still needed elsewhere.
- **Test Scenarios**: `R CMD check` produces no globalVariables NOTEs
- **Tests**: `rcmdcheck::rcmdcheck()`
- **Acceptance criteria**: single `globalVariables` call in `metapip-package.R` with no duplicates

### 11. Add get_core_packages alias and @return tags
- **Requirements**: R6
- **Files**: `R/init_metapip.R`, other R files as needed
- **Details**: (a) Add `get_core_packages` as an alias that calls `get_core_pagkages()`. Export it. Keep the typo export for backward compatibility. Add a `@note` marking the typo as deprecated. (b) Audit all exported functions for missing `@return` roxygen2 tags. Add them where missing. Functions to check: `get_branches`, `get_branch_info`, `get_latest_branch_update`, `install_branch`, `install_pip_packages`, `install_latest_branch`, `core_metadata`, `check_github_token`, `metapip_packages`, `rowname_to_column`.
- **Test Scenarios**: `get_core_packages()` works identically to `get_core_pagkages()`; `devtools::document()` generates clean NAMESPACE
- **Tests**: extend `tests/testthat/test-get_core_pagkages.R`
- **Acceptance criteria**: NAMESPACE exports both `get_core_pagkages` and `get_core_packages`; `roxygen2::roxygenize()` produces no warnings

### 12. Version bump to 0.1.0 and NEWS.md rewrite
- **Requirements**: R6
- **Files**: `DESCRIPTION`, `NEWS.md`
- **Details**: (a) Change `Version: 0.0.2` to `Version: 0.1.0` in DESCRIPTION. (b) Rewrite NEWS.md with a single clean 0.1.0 entry that summarizes all changes from the 5 milestones. Remove the incoherent 0.0.0.9010/0.0.0.9008 entries. Structure: `# metapip 0.1.0` with sections for Breaking Changes (none), New Features, Bug Fixes, Performance, Testing, and Internal.
- **Test Scenarios**: DESCRIPTION parses correctly; NEWS.md is valid markdown
- **Tests**: `rcmdcheck::rcmdcheck()`
- **Acceptance criteria**: `grep "Version:" DESCRIPTION` shows 0.1.0; NEWS.md has single 0.1.0 entry

## Testing Strategy

- **Unit tests**: All new/modified functions have testthat 3e tests with mockery stubs. Memoization tested with call-count mocks (not timing). Cache invalidation tested after `install_branch()`.
- **Integration tests**: `core_metadata()` memoization verified with timing test
- **Regression tests**: Existing tests for V1-V4 fixes (in milestone 1) must still pass
- **CI verification**: Push to branch, verify Ubuntu + Windows jobs both pass
- **Full check**: `rcmdcheck::rcmdcheck()` passes with no errors

## Documentation Checklist

- [ ] `@return` tags on all exported functions
- [ ] `get_core_packages` alias documented
- [ ] `NEWS.md` 0.1.0 entry complete
- [ ] GH_TOKEN comment in codecov.yml
- [ ] `devtools::document()` run and NAMESPACE/man/ committed

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Memoization caches stale data in long-running R sessions | Low | Medium | Cache is per-session only; document that restarting R clears cache. `install_branch()` clears relevant cache entries for the installed package. For `core_metadata()` the data changes infrequently. |
| Memoization bypasses per-call credential validation | Low | Low | Documented trade-off: cached API results skip `check_github_token()`. Restarting R re-validates. Tokens rarely expire within a single session. |
| Mockery stubs don't intercept memoized functions correctly | Medium | High | Test memoization separately from mocked tests; ensure cache is cleared in test setup. Use `setup.R` to reset `.metapip_cache` before each test file. |
| Windows CI reveals platform-specific bugs | Medium | Medium | Run CI first, fix any platform issues in a follow-up. Do not block 0.1.0 on Windows-only cosmetic issues. |
| `requireNamespace()` behavior differs from `installed.packages()` for display logic | Low | Low | `requireNamespace()` only checks availability; `packageVersion()` and `packageDescription()` still work for metadata. Test display output explicitly. |
| Removing data.table globalVariables causes R CMD check NOTEs | Low | Low | Verify with `rcmdcheck()` after cleanup; add back any symbols that are actually needed. |

## Out of Scope

- Milestones 1-4 features (separate plans)
- CRAN submission process
- renv integration
- httr2 migration (milestone 4)
- Pagination improvements (milestone 3)
- Token privilege changes (milestone 3)

## Completion Contract

### Outcome
After implementation, `metapip` 0.1.0 will have per-session API memoization reducing repeated GitHub calls from ~80 to ~3, O(1) per-package namespace checks replacing the O(N) `installed.packages()` scan, comprehensive test coverage with zero un-skipped tests on production code paths, CI running on both Ubuntu and Windows, and a clean 0.1.0 changelog.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required | Phase |
|----|-------------------|------------------|----------|-------|
| V1 | Memoized call skips API on second invocation | Unit test: call `get_branches("pipapi")`, clear cache, call again with `gh::gh` stubbed to error -- verify first call hits API, second returns cached result. Or: mock `gh::gh` with a call counter and verify it's called exactly once per unique key. | yes | 1 |
| V2 | `metapip_attach()` uses `requireNamespace` not `installed.packages()` | `grep "installed.packages" R/attach.R` returns nothing; `grep "requireNamespace" R/attach.R` returns a match | yes | 1 |
| V3 | `devtools::test()` passes with zero skips on prod paths | `Rscript -e "devtools::test()"` | yes | 2 |
| V4 | `rcmdcheck::rcmdcheck()` passes | `Rscript -e "rcmdcheck::rcmdcheck()"` | yes | 3 |
| V5 | CI runs on Ubuntu and Windows | `.github/workflows/codecov.yml` matrix | yes | 3 |
| V6 | DESCRIPTION shows Version: 0.1.0 | `grep "Version:" DESCRIPTION` | yes | 3 |
| V7 | NEWS.md has clean 0.1.0 entry | `head -30 NEWS.md` | yes | 3 |
| V8 | No `skip()` in install-related test files; obsolete file deleted | `test-install_all_packages.R` does not exist; `grep -r "skip(" tests/testthat/test-install_latest_branch.R tests/testthat/test-install_pip_packages.R` returns nothing | yes | 2 |

### Constraints

| ID | Constraint | Check | Phase |
|----|------------|-------|-------|
| C1 | Zero new Imports; memoization uses package-level env cache | `grep "memoise" DESCRIPTION` returns nothing | 1 |
| C2 | All install tests use mockery stubs; no real installs in CI | Review test files | 2 |
| C3 | Network tests gated with skip_if_offline() + skip_on_cran() | `grep -r "skip_if_offline" tests/` | 2 |
| C4 | actions/checkout@v4 in all workflows | `grep "actions/checkout" .github/workflows/*` | 3 |
| C5 | `get_core_pagkages` export preserved for backward compat | `NAMESPACE` still exports it | 3 |
| C6 | globalVariables cleaned (no duplicates, no data.table symbols) | Review R/metapip-package.R | 3 |

### Boundaries
- **Allowed**: R source changes, test rewrites, CI workflow edits, DESCRIPTION/NEWS.md updates, NAMESPACE regeneration
- **Out of scope**: Milestones 1-4 features, CRAN submission, renv integration, httr2 migration

### Iteration Policy
1. Implement P2 (requireNamespace) first -- simplest, no dependencies
2. Implement P1 (memoization) second -- depends on understanding the API call graph
3. Rewrite tests third -- depends on stable function signatures from P1/P2
4. CI + hygiene last -- depends on all code changes being final
5. After Step 11, run `devtools::document()` and commit regenerated `man/` and `NAMESPACE`

### Blocked-Stop Conditions
- `devtools::test()` fails after any phase
- `rcmdcheck::rcmdcheck()` produces errors (warnings acceptable if pre-existing)
- Memoization introduces stale-data bugs in interactive sessions
