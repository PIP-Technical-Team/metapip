---
date: 2026-08-13
title: "Harden the Install Pipeline — safe unload, failure isolation, interactive gate, tri-state SHA"
status: completed
completed-date: 2026-08-13
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [install, robustness, namespace, sha, non-interactive, metapip]
phases: 2
completed-phases: [1, 2, 3]
---

# Plan: Harden the Install Pipeline

## Objective

Make the metapip install/update flow robust across six verified issues from the
engineering review (`inst/TMP/metapip-review.html`): safe namespace unloading,
per-package failure isolation, interactive gating, tri-state SHA comparison,
SHA short-circuit on already-current installs, and documenting the CRAN
dependency gap. All fixes ship as a single mergeable PR targeting `master`.

## Context

- Milestone: `harden-install-pipeline` (roadmap.json, milestone 2 of 5).
- Depends on milestone 1 ("Unblock the Core", V1–V4) being merged. Verification
  assumes those fixes are present on `master`.
- PIP packages import each other heavily (pipapi → pipaux, etc.), so
  `unloadNamespace()` commonly raises "namespace 'x' is imported by 'y'" during
  sessions where packages are attached.
- Current branch: `install-robustness` (feature branch off `master`).
- Conventions: data.table-collapse dialect, `cli`/`glue` messaging, `mockery`
  for unit-test stubs. Tests are testthat 3rd edition in `tests/testthat/`.
- Key files:
  - `R/utils.R` — `detach_package()` (R1)
  - `R/install_pip_packages.R` — `install_branch()`, `install_latest_branch()`, `install_pip_packages()` (R1, P3)
  - `R/init_metapip.R` — `update_pip_packages()`, `compare_sha()`, `init_metapip()` (R2, R3, R6, S5)
  - `R/get_branches.R` — `get_latest_branch_update()` `ss(1L)` guard (R6 edge)
  - `README.md` — companion-tool note (S5)

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | `detach_package()` must not abort on `unloadNamespace()` failure; log a restart advisory and continue | Issue R1 |
| R2 | `update_pip_packages()` must isolate per-package failures in the install loop and report a summary (N succeeded, M failed) | Issue R2 |
| R3 | `update_pip_packages(ask=TRUE)` must gate `utils::menu()` on `interactive()`; non-interactive sessions default to install with a warning, no hang | Issue R3 |
| R4 | `compare_sha()` must return `"unknown"` (not FALSE) when `RemoteSha` is NA | Issue R6 |
| R5 | `update_pip_packages()` must treat `"unknown"` as skip-with-warning, not reinstall | Issue R6 |
| R6 | `get_latest_branch_update()` must guard `ss(1L)` when all branches are `gh-pages` (0 rows) and return an empty correct-column data.frame | Issue R6 edge |
| R7 | `install_latest_branch()` must call `compare_sha()` first and skip packages already at HEAD with an info message | Issue P3 |
| R8 | `update_pip_packages()` emits a `cli_alert_info` noting the CRAN dependency gap / `renv` recommendation; README gets a brief companion-tool note | Issue S5 (deferred) |
| R9 | Each fix has a corresponding mockery-based unit test; no live GitHub calls in unit tests | Requirements 5, 6 |
| R10 | Full `devtools::test()` and `rcmdcheck::rcmdcheck()` pass | Requirement 4 |

## Implementation Steps

## Phase 1: Core pipeline robustness (namespace + failure isolation + interactive gate)

### 1. Safe unloadNamespace() in detach_package() (R1)

- **Requirements**: R1
- **Files**: `R/utils.R`, `tests/testthat/test-utils.R`
- **Details**:
  - Wrap `unloadNamespace(package)` in `tryCatch`. On error, emit
    `cli::cli_alert_warning()` naming the package and advising:
    "Restart R after installation to guarantee the new code is active." Continue
    (do NOT re-throw / abort). Use `cli::format_inline` / `glue` for the message
    consistent with existing `gitcreds_msg` style.
  - Keep `detach_package()` as a single-package helper (signature unchanged).
- **Test Scenarios**:
  - happy path: `unloadNamespace` succeeds → no warning emitted, returns invisibly
  - edge case: `unloadNamespace` raises "namespace 'x' is imported by 'y'" →
    warning emitted containing package name + "Restart R", function returns
    invisibly (does not abort)
  - error path: any other error from `unloadNamespace` → same warning, no abort
- **Tests**: `tests/testthat/test-utils.R` — add `test_that("detach_package
  warns and continues when unloadNamespace fails")` stubbing
  `unloadNamespace` via `mockery::stub` to raise an error; assert
  `expect_message(..., "Restart R")` and that no error propagates. Add a
  succeeding case asserting no message.
- **Acceptance criteria**: `detach_package()` never throws; V1 passes.

### 2. Per-package failure isolation in update_pip_packages() (R2)

- **Requirements**: R2
- **Files**: `R/init_metapip.R`, `tests/testthat/test-init_metapip.R`
- **Details**:
  - Replace `Map(install_branch, missing_pkgs, default_branch[missing_pkgs])` with
    an explicit per-package loop. Wrap each `install_branch()` call in
    `tryCatch`: on error, log the error via `cli::cli_alert_danger` with the
    package name and `conditionMessage(e)`, and continue to the next package.
  - Track successes/failures in counters. After the loop, emit a summary via
    `cli::cli_alert_info`/`cli_alert_warning`: "Installed {n_success}/{n_total};
    {n_failed} failed." (Avoid asserting on the exact wording, only on structure —
    tests check counts via a captured side effect, see Tests.)
  - Coordinate with Step 1: both touch the install path. `install_branch()`
    internally calls `detach_package()`, which Step 1 makes non-throwing, so R2's
    tryCatch mainly protects against `remotes::install_github` / branch
    validation errors. Keep the return value contract: `invisible(TRUE)` if any
    installs attempted, `invisible(FALSE)` only when nothing was missing.
- **Test Scenarios**:
  - happy path: all `missing_pkgs` install successfully → summary reports all
    succeeded, returns `invisible(TRUE)`
  - edge case: one of N packages fails → loop continues, others install,
    summary reports N-1 succeeded / 1 failed, returns `invisible(TRUE)`
  - error path: all packages fail → summary reports 0 succeeded / N failed,
    returns `invisible(TRUE)` (attempted) and does NOT abort
- **Tests**: `tests/testthat/test-init_metapip.R` — stub `install_branch` so it
  fails for a specific package (e.g. `wbpip`) and succeeds for others; stub
  `compare_sha`/`get_core_pagkages`/`get_package_current_branch` to set up
  `missing_pkgs`. Assert all other packages were called and the run completes
  without error. Assert the summary message mentions the failed package.
- **Acceptance criteria**: one failing package no longer aborts the run; V2 passes.

### 3. Interactive gate on utils::menu() (R3)

- **Requirements**: R3
- **Files**: `R/init_metapip.R`, `tests/testthat/test-init_metapip.R`
- **Details**:
  - In `update_pip_packages()`, before calling `utils::menu()` when `ask` is
    TRUE, check `interactive()`. In a non-interactive session, do NOT call
    `utils::menu()`; instead emit
    `cli::cli_alert_warning("Non-interactive session: installing outdated packages by default")`
    and set `answer <- 1`.
  - Keep the existing interactive path unchanged (prompt with `utils::menu`).
  - This must compose with Step 2's loop: the decision (`answer`) feeds the
    install loop, not `Map`.
- **Test Scenarios**:
  - happy path: interactive + `ask=TRUE` → `utils::menu` is called (stub it),
    answer flows through
  - edge case: non-interactive + `ask=TRUE` + outdated packages present →
    `utils::menu` is NOT called, warning emitted, installs proceed
  - error path: non-interactive + `ask=FALSE` → no prompt, `answer` respected
- **Tests**: `tests/testthat/test-init_metapip.R` — stub `interactive` to return
  `FALSE` via `mockery::stub(update_pip_packages, "interactive", function() FALSE)`,
  set up outdated packages, assert `utils::menu` is NOT invoked (mock it and
  assert 0 calls) and that a warning containing "Non-interactive" is emitted and
  installs run. Complement with an interactive case asserting `menu` IS called.
- **Acceptance criteria**: no hang in Rscript/CI/cron; V3 passes.

## Phase 2: SHA correctness, short-circuit, and documentation

### 4. Tri-state compare_sha() + update_pip_packages handling + ss() guard (R6)

- **Requirements**: R4, R5, R6
- **Files**: `R/init_metapip.R`, `R/get_branches.R`, `tests/testthat/test-init_metapip.R`, `tests/testthat/test-get_branches.R`
- **Details**:
  - `compare_sha()` (`R/init_metapip.R`): change the NA-handling. Currently
    `if (is.na(local_sha)) return(FALSE)`. Replace so that when `RemoteSha` is
    `NA` (no git metadata, e.g. CRAN install), return the character `"unknown"`.
    Keep `NULL` return when `gh_sha` is NULL (branch not found) and the boolean
    comparison otherwise. The function now returns a tri-state: `NULL` /
    `"unknown"` / `TRUE` / `FALSE`.
  - `update_pip_packages()`: the current logic does
    `logical_vec <- unlist(pkgs_vec)` then
    `missing_pkgs <- logical_vec[logical_vec == FALSE]`. Adapt to the tri-state:
    - `NULL` entries (branch not found) → handled by the existing `null_vec` block (unchanged).
    - `"unknown"` entries → treat as "skip with warning" (do NOT include in
      `missing_pkgs`). Emit one consolidated
      `cli::cli_alert_warning("Cannot verify SHA for {.pkg {unknown_pkgs}}; skipping (installed without git metadata)")`.
    - `FALSE` entries → these are genuinely out-of-date → `missing_pkgs`.
    - `TRUE` entries → up-to-date → skip silently.
    Because the vector is now mixed-type (logical + character), do NOT rely on
    `unlist()` + `== FALSE`. Build `missing_pkgs` and `unknown_pkgs` explicitly
    by iterating `pkgs_vec` and classifying each element (NULL / "unknown" /
    TRUE / FALSE).
  - `get_latest_branch_update()` (`R/get_branches.R`): after `fsubset(branch_name
    != "gh-pages")` and `roworder`, guard `ss(1L)`: if the filtered result has
    0 rows, return an empty data.frame with the same column names as `res`
    would have had (`package`, `branch_name`, `name`, `last_update_time`). Use
    `collapse` helpers consistent with the file (e.g. construct via
    `data.frame` with zero rows and correct column types, or
    `fsubset(res, FALSE)` after computing columns). Do not let `ss(1L)` error.
- **Test Scenarios**:
  - happy path (compare_sha): `RemoteSha` present and matches `gh_sha` → TRUE;
    present and differs → FALSE
  - edge case (compare_sha): `RemoteSha` is NA → returns `"unknown"` (not FALSE)
  - edge case (update_pip_packages): a package with `"unknown"` is NOT in
    `missing_pkgs`, is not reinstalled, and a warning naming it is emitted
  - error path (get_latest_branch_update): all branches are `gh-pages` → returns
    0-row data.frame with correct columns, no error
- **Tests**:
  - `tests/testthat/test-init_metapip.R`: extend the existing `compare_sha`
    test to add a case where `utils::packageDescription` returns `NA` →
    `expect_equal(compare_sha(...), "unknown")`. Add a `test_that` for
    `update_pip_packages` with a mocked `compare_sha` returning `"unknown"`
    for one package and `FALSE` for another; assert the unknown one is skipped
    (its `install_branch` not called) and a warning is emitted, while the FALSE
    one is installed.
  - `tests/testthat/test-get_branches.R`: add `test_that("get_latest_branch_update
    returns empty df when all branches are gh-pages")` stubbing
    `get_branch_info` to return only `gh-pages` rows; assert 0 rows, correct
    column names, no error.
- **Acceptance criteria**: V4, V5, V6 pass.

### 5. SHA short-circuit in install_latest_branch() (P3)

- **Requirements**: R7
- **Files**: `R/install_pip_packages.R`, `tests/testthat/test-install_latest_branch.R`
- **Details**:
  - In `install_latest_branch()`, for each package, before calling
    `install_branch()`, call `compare_sha(package, branch_name)`. If it returns
    `TRUE`, emit `cli::cli_alert_info("{.pkg {package}} already at HEAD of
    {.field {branch_name}}; skipping")` and skip the install. If `FALSE`,
    install. If `"unknown"` or `NULL`, proceed to install (cannot verify →
    install to be safe), but do not treat as an error.
  - Preserve the current `lapply(cli::cli_progress_along(...)) |> rowbind()`
    data-gathering step; the short-circuit is a per-package decision applied
    when mapping `install_branch` over `dat$package`/`dat$branch_name`. Replace
    the `Map(\(x, y) install_branch(x, y), ...)` with a loop or `mapply` that
    consults `compare_sha` first.
  - Note: this step depends on Step 4 (tri-state `compare_sha`) so it consumes
    the new return contract.
- **Test Scenarios**:
  - happy path: package at HEAD → `compare_sha` returns TRUE → skipped with
    info message, `install_branch` NOT called for it
  - edge case: package out of date → `compare_sha` returns FALSE →
    `install_branch` called
  - edge case: `compare_sha` returns `"unknown"` → `install_branch` called (no
    crash, no skip-by-mistake)
  - error path: `compare_sha` returns NULL (branch missing) → `install_branch`
    not called (consistent with branch-not-found), handled gracefully
- **Tests**: `tests/testthat/test-install_latest_branch.R` — un-skip the file
  (remove the top-level `skip("Avoid testing installation for now")`), stub
  `get_latest_branch_update`/`check_github_token`/`install_branch` and
  `compare_sha`. Assert that when `compare_sha` returns TRUE for a package,
  `install_branch` is not called for it, and an info "skipping" message is
  emitted; when FALSE, `install_branch` is called.
- **Acceptance criteria**: V7 passes; the test file is no longer skipped.

### 6. Document CRAN dependency gap, recommend renv (S5, deferred)

- **Requirements**: R8
- **Files**: `R/init_metapip.R`, `README.md`, `NEWS.md`
- **Details**:
  - In `update_pip_packages()`, after the summary (Step 2) or in the
    out-of-date branch, emit a single `cli::cli_alert_info()` noting that
    metapip installs PIP packages from GitHub branches and resolves
    non-PIP dependencies from CRAN independently, which can cause version skew;
    recommend using `renv` as a companion tool for coordinated dependency
    resolution. Keep it to one concise alert; do not spam on every run
    (emit once per `update_pip_packages()` call when installs occur).
  - `README.md`: add a brief note (1–3 sentences) under an appropriate section
    (e.g. near installation or a new "Dependency management" note) describing
    the same limitation and recommending `renv`.
  - `NEWS.md`: add a bullet under the unreleased version describing the
    robustness fixes from this milestone (R1–R6, P3, S5) so users see the
    behavior changes.
- **Test Scenarios**:
  - happy path: when installs occur, the CRAN-gap info alert is emitted once
  - edge case: when no installs occur (all up-to-date), the alert is not
    emitted (or is acceptable to skip — confirm preference, but default: emit
    only when installs run)
- **Tests**: `tests/testthat/test-init_metapip.R` — add a `test_that` asserting
  the CRAN-gap info message is emitted when the install path runs (mock
  `install_branch` success); assert it is NOT emitted when all packages are
  up-to-date (mock `compare_sha` returning TRUE for all).
- **Acceptance criteria**: V8 passes (alert + README note present).

## Phase 3: Integration verification

### 7. Integration verification + full check

- **Requirements**: R9, R10
- **Files**: `tests/testthat/test-init_metapip.R` (integration test), `NEWS.md`
- **Details**:
  - Add an integration `test_that("init_metapip(ask=FALSE) completes with
    interdependent mocked packages")` that mocks `check_github_token`,
    `get_core_pagkages`, `get_package_current_branch`, `compare_sha` (mix of
    TRUE/FALSE/"unknown"), `install_branch` (succeeds; one fails to exercise
    R2 isolation), `metapip_attach`, and `interactive` (FALSE). Assert the call
    completes without error and `metapip_attach` is reached.
  - Add a focused test: `update_pip_packages(ask=FALSE, answer=2)` returns
    `invisible(FALSE)` without hanging in non-interactive mode (mock
    `interactive()` FALSE, set up outdated packages, assert `menu` not called
    and return value `FALSE` — note: with `ask=FALSE` the menu is already
    skipped; the key assertion is that answer=2 is honored and no prompt occurs).
  - Run `Rscript -e "devtools::document()"` if roxygen comments changed
    (Steps 1, 4 may touch `@keywords internal` / param docs) and commit
    regenerated `man/` + `NAMESPACE`.
- **Test Scenarios**:
  - happy path: full mocked `init_metapip(ask=FALSE)` completes, attach called
  - edge case: one install fails → run still completes (R2), attach still called
- **Tests**: as above; plus the suite-wide commands.
- **Acceptance criteria**: V9, V10 pass.

## Testing Strategy

- Unit tests with `mockery::stub` for all network-touching functions
  (`check_github_token`, `gh::gh`, `remotes::install_github`,
  `latest_commit_for_branch`, `get_branch_info`, `install_branch`,
  `compare_sha`, `interactive`, `utils::menu`, `unloadNamespace`). No live
  GitHub calls in unit tests.
- Tri-state `compare_sha` is the linchpin: Steps 4, 5, 6, and 7 all depend on
  its new return contract — test it first and thoroughly.
- Existing tests in `test-init_metapip.R` (`compare_sha` + `init_metapip
  forwards its answer argument`) and `test-install_pip_packages.R` must keep
  passing. The `compare_sha` test will be extended, not removed.
- `test-install_latest_branch.R` is currently skipped at the top of the file;
  Step 5 un-skips it and replaces with mocked tests.
- Final gate: `Rscript -e "devtools::test()"` then
  `Rscript -e "rcmdcheck::rcmdcheck()"` (both per `AGENTS.md`).

## Documentation Checklist

- [ ] `R/utils.R` — if `detach_package` gets roxygen `@keywords internal` (it's
      currently undocumented), keep `man/` in sync via `devtools::document()`.
- [ ] `R/init_metapip.R` — update `@param`/`@returns` for
      `update_pip_packages()` if behavior/return semantics change (tri-state
      handling, summary). Regenerate docs.
- [ ] `README.md` — add the CRAN-gap / `renv` companion-tool note (S5).
- [ ] `NEWS.md` — add an unreleased-version section listing R1, R2, R3, R6, P3, S5.
- [ ] `NAMESPACE` / `man/` — regenerated only if roxygen comments changed.

## Risks & Mitigations

| ID | Risk | Impact | Mitigation |
|----|------|--------|------------|
| K1 | R1 & R2 touch the same code path (`install_branch` → `detach_package` and `update_pip_packages` → `install_branch`); edits can conflict | merge conflict, rework | Implement R1 first (Step 1), commit, then R2 (Step 2) builds on the non-throwing `detach_package`. Keep commits ordered R1 → R2. |
| K2 | `unlist(pkgs_vec)` + `== FALSE` in `update_pip_packages` breaks once `compare_sha` returns mixed logical/character | runtime error / wrong `missing_pkgs` | Step 4 explicitly classifies elements; do not rely on `unlist()`. Add tests for all tri-state branches before refactoring the caller. |
| K3 | Stubbing `interactive()` / `utils::menu` via mockery may not fully isolate the hang in some testthat setups | flaky or hanging tests | Stub `interactive` and `utils::menu` (mock to count calls); assert 0 menu calls in non-interactive path. If mockery cannot stub `interactive`, wrap the check behind an internal helper (e.g. `is_interactive_session()`) and stub that. |
| K4 | `compare_sha` change is consumed by `install_latest_branch` (Step 5) and integration (Step 7) — ordering risk | later steps break if contract drifts | Define the tri-state contract in Step 4 tests FIRST; Steps 5/7 depend on it. |
| K5 | `get_latest_branch_update` empty-df return changes column shape seen by `core_metadata()` (which does `sapply(latest_commit, "[[", ...)`) | `core_metadata` errors on 0-row result | Confirm empty df has identical column names; check `core_metadata` handling. Add/keep a guard or test that `core_metadata` tolerates an empty latest-commit row. |
| K6 | "Unblock the Core" (V1–V4) not merged to `master` → verification assumes fixes that aren't there | false green / false red | Blocked-Stop C6: confirm `master` state before final verification; ask if absent. |
| K7 | `renv` recommendation wording or placement in README is subjective | minor rework | Keep to 1–3 sentences; confirm in review. |

## Out of Scope

- Full `renv` integration / lock manifest / SHA-pinned installs (milestone 3,
  `lock-down-supply-chain`).
- httr2 fetch / UTC timestamp parsing (milestone 4, `resilient-networking-time`).
- API memoization / `requireNamespace` checks / CI hardening / 0.1.0 release
  (milestone 5, `performance-tests-release`).
- The V1–V4 core bug fixes (milestone 1, `unblock-the-core`) — this plan
  assumes they are merged.
- Any change to public API signatures (only behavior + internal helpers).

## Completion Contract

### Outcome

The install/update pipeline is robust: `detach_package()` never aborts an
install, one failing package no longer kills an `update_pip_packages()` run,
non-interactive sessions no longer hang on `utils::menu()`, `compare_sha()`
returns `"unknown"` for CRAN-installed packages (which are skipped, not
perpetually reinstalled), `get_latest_branch_update()` no longer errors when
only `gh-pages` exists, `install_latest_branch()` skips packages already at
HEAD, and the CRAN dependency gap is documented in-app and in the README. All
changes ship as one PR to `master`; `devtools::test()` and `rcmdcheck` pass.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Phase | Required |
|----|-------------------|------------------|-------|----------|
| V1 | `detach_package()` logs restart advisory and does NOT abort on `unloadNamespace()` error | `test-utils.R` mockery test | 1 | yes |
| V2 | `update_pip_packages()` continues past one failing package + prints N succeeded / M failed summary | `test-init_metapip.R` | 1 | yes |
| V3 | `update_pip_packages(ask=TRUE)` non-interactive → installs by default with warning, no `utils::menu()` call | `test-init_metapip.R` | 1 | yes |
| V4 | `compare_sha()` returns `"unknown"` (not FALSE) when `RemoteSha` is NA | `test-init_metapip.R` | 2 | yes |
| V5 | `update_pip_packages()` treats `"unknown"` as skip-with-warning, not reinstall | `test-init_metapip.R` | 2 | yes |
| V6 | `get_latest_branch_update()` returns empty correct-column data.frame when all branches are `gh-pages` | `test-get_branches.R` | 2 | yes |
| V7 | `install_latest_branch()` skips packages already at HEAD with info message | `test-install_latest_branch.R` (un-skipped) | 2 | yes |
| V8 | `update_pip_packages()` emits CRAN-gap info alert; README has companion-tool note | `test-init_metapip.R` + README grep | 2 | yes |
| V9 | `init_metapip(ask=FALSE)` completes without error with interdependent mocked packages | `test-init_metapip.R` integration | final | yes |
| V10 | Full test suite + `rcmdcheck` pass | `Rscript -e "devtools::test()"`, `rcmdcheck::rcmdcheck()` | final | yes |

### Constraints

| ID | Constraint | Check | Phase |
|----|------------|-------|-------|
| C1 | Single mergeable PR targeting `master` | one PR, base = master | final |
| C2 | Implement in order R1 → R2 → R3 → R6 → P3 → S5 | commit log order | all |
| C3 | Coordinate R1/R2 (same code area) to avoid merge conflicts | clean merge | 1 |
| C4 | Use collapse, cli, glue conventions consistent with package | code review | all |
| C5 | Mock network calls with mockery; no live GitHub in unit tests | testthat runs offline | all |
| C6 | "Unblock the Core" (V1–V4) merged to `master` before final verification | confirm master state | final |

### Boundaries

- **Allowed**: `R/utils.R`, `R/install_pip_packages.R`, `R/init_metapip.R`,
  `R/get_branches.R`, `README.md`, `NEWS.md`, `tests/testthat/*`, regenerated
  `man/` + `NAMESPACE` via `devtools::document()`.
- **Out of scope**: full `renv` integration, SHA-pinned installs / lock
  manifest (milestone 3), httr2 fetch / UTC timestamps (milestone 4),
  memoization (milestone 5), the V1–V4 core fixes (milestone 1), public API
  signature changes.

### Iteration Policy

1. If a fix surfaces an additional robustness issue in the same code area, ask
   before expanding scope (deviation-policy: `ask`).
2. If a mocking strategy fails to isolate a network call cleanly, ask for
   guidance before falling back to live calls.
3. If "Unblock the Core" (V1–V4) is not yet merged to `master`, ask whether to
   proceed against current `master` or wait.
4. If `core_metadata()` breaks due to the `get_latest_branch_update()` empty-df
   change (K5), ask before modifying `core_metadata` (that may belong to a
   different milestone scope).

### Blocked-Stop Conditions

- A required V1–V4 dependency (milestone 1) blocks verification of a fix → stop
  and ask.
- A fix would require a breaking change to a public API signature → stop and ask.
- Tests cannot be written without live GitHub credentials → stop and ask.
- `core_metadata()` regresses as a side effect of K5 and the fix crosses into
  another milestone's scope → stop and ask.
