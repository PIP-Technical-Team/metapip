---
date: 2026-08-12
title: "Unblock the Core — fix V1–V4 and R7 with a red-phase regression harness"
status: completed
completed-date: 2026-08-12
completed-phases: [1, 2]
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [unblock-the-core, bugfix, regression-tests, red-green-refactor, critical]
execution-report: .cg-docs/work-reports/2026-08-12-unblock-the-core.md
---

# Plan: Unblock the Core

## Objective

Make the three headline metapip workflows functional again by fixing the
four critical bugs (V1–V4) and the medium robustness bug (R7) identified in the
engineering review (`inst/TMP/metapip-review.html`), using a red-green-refactor
approach: write failing regression tests first, then implement the fixes in
dependency order, then verify with the full test suite and `rcmdcheck`.

## Context

The engineering review verified (R 4.5.2) that:
- `install_pip_packages()` (V1) references an undefined `pkg` variable and
  mishandles an unnamed scalar `branch`, so it silently installs nothing.
- `package_branches()` (V2) builds its branch column from `utils::stack()`
  row names (which are `1,2,3,…`, **not** the package/branch combos) and then
  applies a regex with an invalid character range (`z-_`) and a `.Version`
  suffix that never exists — so every branch cell is `NA` and the status table
  is empty. **The review's regex-only swap is therefore necessary but not
  sufficient**: `get_complete_data()` must be rebuilt from the named list.
- `get_custom_branch(package = )` (V3) filters on an undefined `branch` (the
  parameter is `package`) and always errors.
- `fs::path_file()` (V4) is used but `fs` is not in `DESCRIPTION` at all, and
  `stringr::str_extract()` is used while `stringr` is only in `Suggests`.
- `.onAttach()` (R7) computes `needed` then does nothing, so `library(metapip)`
  does not attach core packages, and `init_metapip()` hardcodes `answer = 1`
  instead of forwarding its own `answer` argument.

Conventions: collapse (import collapse except `fdroplevels`), `cli` for
user-facing messages, `glue` for string interpolation. Tests use testthat 3rd
edition; existing tests stub network calls with `mockery`.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Write red-phase regression tests for V1–V4 and R7 *before* any code change; tests must fail against current code | user #1 |
| R2 | Each fix is a separate plan step with clear acceptance criteria | user #2 |
| R3 | Fixes implemented in dependency order V1 → V2 → V3 → V4 → R7 (user-specified; V2 changes the data schema consumed by `common_data()`/`split_packages_into_list()`, so its rewrite must land before V3/V4/R7 are verified) | user #3 |
| R4 | After all fixes, run full `devtools::test()` and `rcmdcheck::rcmdcheck()` | user #4 |
| R5 | Use collapse conventions (import collapse except fdroplevels); `cli` for messages; `glue` for interpolation | user #5 |
| R6 | Produce a single mergeable PR targeting `master` | user #6 |
| R7 | `install_pip_packages()` installs each package with the correct branch; error messages name the package, not the loop index | bug V1 |
| R8 | `package_branches()`/`get_complete_data()` returns correct `package`/`branch`/`version` | bug V2 |
| R9 | `get_custom_branch(package = )` returns the branch without error | bug V3 |
| R10 | Runtime deps are declared: `fs` usage removed (→ `basename()`), `stringr` runtime usage removed (→ base R) | bug V4 |
| R11 | `.onAttach()` attaches core packages; `init_metapip()` forwards its `answer` argument | bug R7 |

phases: 2  # convenience hint -- may be stale; always recount from ## Phase headers

## Phase 1: Functional bug fixes (V1–V3) + red harness

### 1. Write red-phase regression tests (V1–V4, R7)

- **Requirements**: R1, R2, R3, R5
- **Files**: `tests/testthat/test-install_pip_packages.R` (new),
  `tests/testthat/test-get_complete_data.R` (new),
  `tests/testthat/test-get_custom_branch.R` (new),
  `tests/testthat/test-undeclared-deps.R` (new),
  `tests/testthat/test-attach.R` (replace `2*2` placeholder),
  `tests/testthat/test-init_metapip.R` (extend)
- **Details**: Add failing tests that capture correct behavior. Stub network /
  side-effecting functions with `mockery` so no GitHub token or install is
  needed. Tests:
  - V1: stub `check_github_token` (returns `list(password="x")`) and
    `install_branch` (mock recording calls); assert `install_pip_packages(branch = "test")`
    calls `install_branch` once per core package with `package` = each core name
    and `branch = "test"`. Also assert a named `branch` vector is honored and
    that any failure message contains the package name.
  - V2: `metapip:::get_complete_data(list(pipapi = c(PROD="0.1.0", DEV="0.1.1"), wbpip = c(PROD="0.2.0")))`
    must return `branch = c("PROD","DEV","PROD")`,
    `package = c("pipapi","pipapi","wbpip")`, `version` matching.
  - V3: `withr::with_options(list(metapip.custom_branch = list(pipapi_branch="DEV")), { expect_equal(get_custom_branch(package = "pipapi"), list(pipapi = "DEV")) })`.
  - V4: `get_core_pagkages(exclude = NA)` returns `core` minus the cwd package
    when `getwd()` ends in a core name (stub `getwd()`). Make the test fail
    pre-fix by stubbing `fs::path_file` to `stop("fs used")`; current code
    triggers the stub, so the test errors and fails. After the fix the stub is
    untouched and the test passes. Add a guard noting `rcmdcheck` must not flag
    undeclared `fs`/`stringr`.
  - R7: (a) stub `update_pip_packages`, assert `init_metapip(answer = 2)` calls
    it with `answer = 2`; (b) stub `metapip_attach`, assert `.onAttach()`
    invokes it when core packages are not attached.
- **Test Scenarios**: happy path (correct calls/values), edge case (named vs
  unnamed `branch`; `exclude = NA` with cwd a core pkg), error path (current
  code silently swallows V1 errors so the mock is never called → test fails).
- **Tests**: `Rscript -e 'devtools::test()'` — these specific files must FAIL
  before Steps 2–6 and PASS after.
- **Acceptance criteria**: `devtools::test()` reports failures for the new tests
  against current code (proves they are red).

### 2. Fix V1 — `install_pip_packages()`

- **Requirements**: R2, R3, R5, R7
- **Files**: `R/install_pip_packages.R`
- **Details**: Inside the `lapply` loop:
  - Call `install_branch(package = pgk, branch = brn)` (rename the undefined `pkg` → the
    local `pgk`; use named `branch` to avoid positional fragility).
  - Resolve the branch per package: if `branch` has no names, recycle the scalar
    for every package (`brn <- if (is.null(names(branch))) branch[1] else unname(branch[pgk])`);
    this fixes `branch[pgk]` → `NA` for the documented `branch = "test"` usage.
  - Use `pgk` (the package name) — not the numeric loop index `x` — in the
    `cli_alert_danger`/`cli_alert_warning` messages via `glue`/`cli` glue syntax.
- **Test Scenarios**: happy path (explicit `branch = "test"` installs all core);
  edge case (named `branch = c(pipapi = "DEV", wbpip = "PROD")` honored);
  error path (a genuine install failure still reports the package name, not `x`).
- **Tests**: `test-install_pip_packages.R` (Step 1).
- **Acceptance criteria**: `install_pip_packages(branch = "test")` calls
  `install_branch` once per core package with the correct `package`/`branch`, and
  failure messages contain the package name.

### 3. Fix V2 — `package_branches()` / `get_complete_data()`

- **Requirements**: R2, R3, R5, R8
- **Files**: `R/package_branches.R` (rewrite `get_complete_data()`)
- **Details**: `utils::stack()` discards the branch names (row names become
  `1,2,…`), so the regex cannot recover them. Rebuild the table directly from the
  named list:
  ```r
  get_complete_data <- function(all_package_version) {
    branch  <- unlist(lapply(all_package_version, names))
    package <- rep(names(all_package_version), lengths(all_package_version))
    version <- unlist(all_package_version, use.names = FALSE)
    data.frame(package = package, branch = branch, version = version)
  }
  ```
  This also removes the only `stringr::str_extract()` call (helps R10). Keep the
  downstream `common_data()` (`PROD`/`DEV_v2`/`QA` filter + `pivot`) and
  `split_packages_into_list()` behavior intact.
- **Test Scenarios**: happy path (`get_complete_data()` returns correct
  package/branch/version); edge case (packages with differing branch counts);
  error path (empty/NA version still yields a row with the right branch).
- **Tests**: `test-get_complete_data.R` (Step 1) + existing
  `test-package_branches.R`.
- **Acceptance criteria**: `metapip:::get_complete_data()` returns a
  3-column data.frame (`package`, `branch`, `version`); `common_data()` keeps
  the column schema (`package`, branch names like `PROD`/`DEV`); `split_packages_into_list()`
  returns a named list of 2-column data.frames (`branch`, `version`);
  `package_branches()` no longer yields an all-`NA` status table. Existing
  `test-package_branches.R` assertions for `length(out, 4)`, `length(out$local, 4)`,
  and `length(out$common, 4)` must remain true after the fix (the data.frame
  class, list structure, and column names are preserved).

### 4. Fix V3 — `get_custom_branch()`

- **Requirements**: R2, R3, R5, R9
- **Files**: `R/get_branches.R`
- **Details**: In `get_custom_branch()`, change
  `existing_branches[names(existing_branches) %in% branch]` to
  `%in% package` (the parameter is `package`). No other change to the option
  handling.
- **Test Scenarios**: happy path (`get_custom_branch(package = "pipapi")` returns
  the pipapi branch); edge case (`package = NULL` returns all custom branches);
  error path (unknown package still produces the existing `cli_abort`).
- **Tests**: `test-get_custom_branch.R` (Step 1).
- **Acceptance criteria**: `get_custom_branch(package = "pipapi")` returns the
  branch without "object 'branch' not found".

## Phase 2: Dependency hygiene, attach, verification

### 5. Fix V4 — undeclared dependencies

- **Requirements**: R2, R3, R5, R10
- **Files**: `R/init_metapip.R`, `R/package_branches.R`, `DESCRIPTION`
- **Details**:
  - `R/init_metapip.R:153`: replace `fs::path_file(getwd())` with
    `basename(getwd())` (base R equivalent).
  - `R/package_branches.R`: `get_complete_data()` no longer uses `stringr`
    (Step 3), so no runtime `stringr` dependency remains. Confirm no other
    `fs::`/`stringr::` usages exist in `R/` (verified: only these two sites).
  - `vignettes/package_development.Rmd`: replace `fs::path_temp()`,
    `fs::path()`, `fs::dir_create()` with base R equivalents
    (`tempdir()`, `file.path()`, `dir.create(..., recursive = TRUE)`)
    so no `fs` dependency is needed.
  - `DESCRIPTION`: no change to `Imports` is required (neither `fs` nor `stringr`
    is a runtime need). Leave `stringr` in `Suggests` only if a test uses it;
    otherwise it can stay for test tooling. Do **not** add `fs`.
- **Test Scenarios**: happy path (`get_core_pagkages(exclude = NA)` returns the
  right subset using only base R); edge case (`getwd()` not a core package →
  full `core`); error path (`rcmdcheck` raises no "unknown / undeclared" NOTE for
  `fs`/`stringr`).
- **Tests**: `test-undeclared-deps.R` (Step 1) + `rcmdcheck` (Step 7).
- **Acceptance criteria**: no `fs`/`stringr` runtime calls; `rcmdcheck` shows no
  undeclared-dependency NOTE.

### 6. Fix R7 — `.onAttach()` stub + `init_metapip()` answer

- **Requirements**: R2, R3, R5, R11
- **Files**: `R/zzz.R`, `R/init_metapip.R`
- **Details**:
  - `R/zzz.R` `.onAttach()`: after computing `needed <- core[!is_attached(core)]`,
    actually attach them by calling `metapip_attach(needed)` (guarded by
    `if (length(needed) > 0)`), so `library(metapip)` loads core packages.
  - `R/init_metapip.R`: pass `answer = answer` (not the hardcoded `1`) into
    `update_pip_packages()`.
  - `R/init_metapip.R` `update_pip_packages()`: remove the internal
    `answer <- 1` reassignment (around line 88) so the forwarded `answer`
    is honored by the `if (answer == 1)` branch.
- **Test Scenarios**: happy path (`.onAttach()` triggers `metapip_attach` when
  core unloaded; `init_metapip(answer = 2)` forwards `answer = 2`); edge case
  (all core already attached → `.onAttach()` early-returns without error);
  error path (missing core packages → `metapip_attach` warns but does not abort
  the attach of installed ones).
- **Tests**: `test-attach.R`, `test-init_metapip.R` (Step 1).
- **Acceptance criteria**: `.onAttach()` invokes `metapip_attach`; `init_metapip()`
  forwards its `answer` argument.

### 7. Verify — full test suite and `rcmdcheck`

- **Requirements**: R3, R4, R6
- **Files**: `tests/testthat/test-package_branches.R`
  (add `skip("avoid live network")`), `tests/testthat/test-get_branches.R`
  (add `skip("avoid live network")` to lines ~38–60)
- **Details**: (1) Add `skip("avoid live network")` to the pre-existing
  network-dependent tests in `test-package_branches.R` (root cause: `get_package_version()`
  calls `read.dcf(url(...))`) and `test-get_branches.R` (root cause: `get_branch_info()` /
  `get_latest_branch_update()` call `gh::gh` / `latest_commit_for_branch` without stubs)
  so the suite is CI-safe without GitHub. (2) Run full `devtools::test()` and
  `rcmdcheck::rcmdcheck()`; confirm zero failures, zero `ERROR`, and no `NOTE`
  for undeclared `fs`/`stringr`. (3) Wire changes into branch
  `fix/unblock-the-core` and open one PR targeting `master`.
- **Test Scenarios**: happy path (green suite + clean check); edge case (re-run
  after each fix to keep the build green incrementally).
- **Tests**: `Rscript -e "devtools::test()"` and
  `Rscript -e "rcmdcheck::rcmdcheck()"`.
- **Acceptance criteria**: `devtools::test()` passes; `rcmdcheck` is clean.

## Testing Strategy

- Red-green-refactor: all regression tests authored in Step 1 fail against the
  current (broken) code, then pass as each fix lands.
- Network isolation: stub `check_github_token`, `install_branch`, `get_branches`,
  `update_pip_packages`, `metapip_attach`, and `getwd()` with `mockery`/`withr`
  so tests need no GitHub token, network, or real package installs.
- Keep the existing `skip("Avoid testing installation for now")` live-install
  tests untouched; they are out of scope for this milestone.
- Run tests incrementally after Steps 2–6 to keep the build green per phase.

## Documentation Checklist

- [ ] `NEWS.md`: add an entry under a new "Bug fixes" section for V1–V4 and R7.
- [ ] `man/` + `NAMESPACE` regenerated if any `@export`/signature changes
      (none expected; `devtools::document()` only if touched).
- [ ] No README/user-facing behavior change beyond the fixes themselves.

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Attaching core packages in `.onAttach()` slows `library(metapip)` or fails when core pkgs are missing | Medium | Medium | `metapip_attach()` already warns (does not abort) for missing core pkgs; only attach currently-unloaded packages |
| Rebuilding `get_complete_data()` changes downstream `common_data()`/`split_packages_into_list()` assumptions | Medium | High | Verify `PROD`/`DEV_v2`/`QA` filter and `pivot`/`split` still work; add a unit test for `get_complete_data()` |
| Tests require GitHub token / network (`check_github_token`) | Medium | Medium | Stub all network/side-effect funcs with `mockery`; keep live-install `skip()` as-is |
| `get_custom_branch` option side effects leak across tests | Low | Medium | Use `withr::with_options()` and restore originals via `on.exit()` |
| `rcmdcheck` NOTE for removed `stringr`/`fs` usage | Low | Low | Keep `stringr` in `Suggests` (test tooling); remove only runtime use; no `fs` import added |

## Out of Scope

- SHA-pinned installs, team lock manifest, least-privilege tokens (milestone 3).
- `httr2` HTTP fetch, UTC timestamp parsing, `ss(1L)`/compare_sha edge cases
  (milestones 4–5).
- API memoization, CI hardening, version bump to 0.1.0 (milestone 5).
- Any change to `install_latest_branch()` beyond what V1's fix implies.

## Completion Contract

### Outcome

The three headline metapip workflows (`install_pip_packages`,
`package_branches`, `get_custom_branch`) and the `.onAttach()`/init flow work
again, guarded by red-phase regression tests, with the full test suite and
`rcmdcheck` passing, delivered as one PR to `master`.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required | Phase |
|----|-------------------|------------------|----------|-------|
| V1 | Red tests for V1–V4, R7 FAIL on current code (pre-fix) | `devtools::test()` on new test files shows failures | yes | 1 |
| V2 | Red tests PASS after fixes | `devtools::test()` → 0 failures | yes | 2 |
| V3 | `get_complete_data()` returns correct `package`/`branch`/`version` | `test-get_complete_data.R` | yes | 1 |
| V4 | `install_pip_packages(branch="test")` calls `install_branch` per package | `test-install_pip_packages.R` | yes | 1 |
| V5 | `get_custom_branch(package=)` returns branch w/o error | `test-get_custom_branch.R` | yes | 1 |
| V6 | `.onAttach()` calls `metapip_attach`; `init_metapip()` forwards `answer` | `test-attach.R`, `test-init_metapip.R` | yes | 2 |
| V7 | No undeclared `fs`/`stringr` runtime deps | `rcmdcheck::rcmdcheck()` clean | yes | 2 |
| V8 | Single mergeable PR to `master` | branch `fix/unblock-the-core` + PR | yes | final |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | No new external runtime deps; `fs` removed, `stringr` runtime use removed | `DESCRIPTION` diff + `rcmdcheck` |
| C2 | Fixes applied in order V1→V2→V3→V4→R7 before verification | commit sequence |
| C3 | collapse conventions; `cli` messages; `glue` interpolation | code review |
| C4 | Single PR targeting `master` | PR target branch |
| C5 | No network/github logic changes beyond the bug fixes | scope review |

### Boundaries

- Allowed: edit `R/install_pip_packages.R`, `R/package_branches.R`,
  `R/get_branches.R`, `R/init_metapip.R`, `R/zzz.R`, `DESCRIPTION`, and
  `tests/testthat/*`.
- Out of scope: SHA pinning, token least-privilege, `httr2` fetch, UTC
  timestamps, memoization, CI hardening, 0.1.0 release (later milestones).

### Iteration Policy

1. Implement Step 1 (red tests) and confirm they fail before touching source.
2. Implement Steps 2–6 in order; re-run `devtools::test()` after each.
3. Run Step 7 (`devtools::test()` + `rcmdcheck`) only after all fixes land.
4. If a fix reveals a deeper issue (e.g., `get_complete_data` downstream
   breakage), extend the corresponding test before adjusting the fix.

### Blocked-Stop Conditions

- `devtools::test()` still fails after all fixes → stop, do not open PR.
- `rcmdcheck` reports `ERROR` or undeclared-dependency `NOTE` → stop.
- A fix requires changing network/github logic outside the verified bugs → stop
  and escalate scope to the user (deviation-policy: ask).
