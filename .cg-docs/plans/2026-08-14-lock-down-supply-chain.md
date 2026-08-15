---
date: 2026-08-14
title: "Lock Down the Supply Chain — SHA-pinned installs, PIP_LOCK manifest, least-privilege tokens, full pagination"
status: completed
completed-date: 2026-08-14
completed-phases: [1, 2, 3, 4]
scope: "Deep"
brainstorm: null
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [security, supply-chain, sha-pinning, lockfile, tokens, pagination, metapip]
phases: 4
review-revisions:
  - "v2 (2026-08-14): addresses plan-critic P1.1, P1.2, P2.1–P2.4, P3.1–P3.5"
execution-report: .cg-docs/work-reports/2026-08-14-lock-down-supply-chain.md
---

# Plan: Lock Down the Supply Chain

## Objective

Close the supply-chain security findings from the engineering review
(`inst/TMP/metapip-review.html`) across four verified issues (S1–S4) and seven
roadmap features. Make installs deterministic via SHA pinning and a committed
team lock manifest (`PIP_LOCK.csv`), remove over-privileged credential
handling, drop the leaked Codecov badge token, and fix GitHub API pagination.
All fixes ship as a single mergeable PR targeting `master`.

## Context

- Milestone: `lock-down-supply-chain` (roadmap.json, milestone 3 of 5), 7
  features: `sha-pinned-install`, `pip-lock-manifest`, `lock-driven-init`,
  `install-latest-dev-only`, `least-privilege-tokens`, `remove-codecov-token`,
  `full-gh-pagination`.
- Depends on milestone 2 ("Harden the Install Pipeline", `done`) — its
  tri-state `compare_sha()`, per-package failure isolation, interactive gate,
  and SHA short-circuit are already in the code and are EXTENDED, not
  replaced. Also depends on milestone 1 ("Unblock the Core", still `planned`
  in roadmap) being merged before final verification (see C6).
- Current branch: `lock-down-the-supply-chain` (feature worktree off `master`).
- Conventions: data.table-collapse dialect, `cli`/`glue` messaging, `mockery`
  for unit-test stubs, testthat 3rd edition in `tests/testthat/`.
- Design decisions (from strategy session): SHA-pinned by default with
  `force = TRUE` override for live HEAD; a team lock manifest
  (`PIP_LOCK.csv`, columns `package,branch,sha`) committed to the repo IS the
  team's agreement on "the right version"; `init_metapip()` is lock-driven,
  `update_pip_packages()` refreshes the lock, `pip_snapshot()` writes the
  lock, `install_latest_branch()` becomes developer-only.
- **Determinism scope**: R7 (`install_branch` SHA pinning) gives _session-level_
  determinism — two users installing at the same moment get the same commit.
  R10/R12 (`PIP_LOCK` + lock-driven `init_metapip`) give _team-level_
  determinism — all team members install the same committed SHAs regardless
  of when they run. The plan intentionally layers both.

Key code facts (current state):
- `R/install_pip_packages.R:107-119` — `install_branch()` installs via
  `remotes::install_github("PIP-Technical-Team/{package}@{branch}")` (mutable
  branch ref), no SHA pin, no `force`, no idempotency. Calls
  `check_github_token()`, `detach_package()`, `get_branches()`.
- `R/init_metapip.R:46-152` — `update_pip_packages()` uses
  `mapply(compare_sha, pkgs, default_branch[pkgs])`, classifies tri-state into
  `missing_pkgs`/`unknown_pkgs`, has per-package failure isolation + interactive
  gate + CRAN-gap info alert. `init_metapip()` just delegates to
  `update_pip_packages()` then `metapip_attach()`.
- `R/init_metapip.R:156-173` — `compare_sha()` is tri-state
  (`NULL`/`"unknown"`/`TRUE`/`FALSE`); uses `latest_commit_for_branch(...)$sha`
  and `utils::packageDescription(..., "RemoteSha")`.
- `R/utils.R:91-106` — `check_github_token()` returns `invisible(creds)` with
  the live PAT in `creds$password`; `print()` leaks the token.
- `R/get_branches.R:19-31` — `get_branches()` calls `check_github_token()`
  and passes `.token = creds$password` to `gh::gh(...)` with default
  `.limit = NULL` (only first page, 30 branches).
- `R/core_metadata.R:62-69` — `latest_commit_for_branch(package, branch)`
  returns the raw gh commit object including `$sha` (usable for pinning).
- `README.Rmd:19` — Codecov badge URL carries `?token=5DMKVN16NM`.
- Existing tests: `test-get_branches.R` (stub `gh::gh`, `install_branch`),
  `test-init_metapip.R` (tri-state `compare_sha`, failure isolation, gate),
  `test-install_latest_branch.R` (un-skipped, short-circuit), `test-utils.R`.
- Brain gotcha: `mapply(compare_sha, ...)` does not reliably intercept a
  mocked `compare_sha` returning different values per argument — keep mock
  return values simple (single value) and test each path in a separate block
  (`.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`).

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | `get_branches()` returns all branches even for repos with >30 branches (`.limit = Inf`) | S4 |
| R2 | Codecov badge URL in README carries no `?token=` parameter; `README.Rmd` and `README.md` stay in sync | S3 |
| R3 | New `gh_token()` helper returns the GitHub token when available and `NULL` when missing, with no error/abort; this is the single source of token retrieval (no separate `gh_auth()`) | S2 |
| R4 | Read-only `gh::gh()` calls (`get_branches()` and related) no longer require a token — work without `GITHUB_PAT`/creds (public org) | S2 |
| R5 | `check_github_token()` returns a redacted list so `print()` shows `""` instead of the PAT | S2 |
| R6 | Only `install_branch()` and `install_pip_packages()` are gated by `check_github_token()`; read-only functions use `gh_token()` best-effort | S2 |
| R7 | `install_branch()` pins to the resolved branch HEAD SHA by default and installs via `@<sha>` (session-level determinism) | S1-A |
| R8 | `install_branch()` skips install with an info message when the installed `RemoteSha` already matches the resolved SHA (idempotency) | S1-A |
| R9 | `install_branch(force = TRUE)` bypasses pinning and installs live branch HEAD (`@<branch>`) with a bypass warning | S1-A |
| R10 | New `pip_snapshot()` resolves all core package branches to current HEAD SHAs and writes/updates `PIP_LOCK.csv` (`package,branch,sha` with header); default write target is the source-tree `inst/` path (see P1.1 fix below) | S1-B |
| R11 | `pip_snapshot()` is exported and documented with roxygen2 | S1-B |
| R12 | `init_metapip()` reads `PIP_LOCK` and installs each package at its recorded SHA; falls back to branch HEAD and suggests `pip_snapshot()` when the lock is absent | S1-C |
| R13 | `update_pip_packages()` resolves current HEAD SHAs, updates `PIP_LOCK`, prompts to install at the new SHAs; preserves milestone-2 failure isolation + interactive gate | S1-C |
| R14 | `install_latest_branch()` emits a developer-only "bypasses the team lockfile" warning and calls `install_branch(..., force = TRUE)` internally | S1-D |
| R15 | Each fix has a mockery-based unit test; no live GitHub calls; `tempfile()` for `PIP_LOCK` in tests | Requirements (Testing) |
| R16 | Full `devtools::test()` and `rcmdcheck::rcmdcheck()` pass offline | Requirements (Verify) |
| R17 | `compound-gpid.md` charter updated: read-only functions no longer require a GitHub token (install functions still require one for rate-limit guard) | P2.1 |
| R18 | `check_github_token()` gate rationale documented as rate-limit guard (5000 vs 60 req/hr authenticated vs unauthenticated) — not security enforcement | P2.2 |
| R19 | Existing `install_branch` test at `test-get_branches.R:18-29` updated with stubs for `latest_commit_for_branch` + `utils::packageDescription` so it survives Phase 3 entry | P2.3 |
| R20 | Explicit idempotency short-circuit tests: `install_branch` when installed `RemoteSha == target_sha` skips with info; when `RemoteSha` is `NA` proceeds with install | P2.4 |
| R21 | `NEWS.md` updated; `man/`+`NAMESPACE` regenerated for the new export and changed signatures | Conventions |
| R22 | All changes ship as a single mergeable PR targeting `master` | Requirement (PR) |

### Review findings addressed

| Finding | Severity | Resolution |
|---------|----------|------------|
| P1.1 `pip_lock_path()` conflates read/write paths | P1 | `pip_lock_path()` is read-only (system.file); `pip_snapshot(path=)` default uses `getOption("metapip.lock_path")` falling back to `system.file(...)` for dev sessions. Installs never write the lock. See Step 5 details. |
| P1.2 null SHA when `latest_commit_for_branch()` fails | P1 | `install_branch(force=FALSE)` guards: if `target_sha` is `NULL` after resolution, emit `cli_abort("Could not resolve SHA for {package}@{branch}")` and return `invisible(NULL)`. See Step 4 details. |
| P2.1 charter says token required | P2 | R17 added: update `compound-gpid.md` constraint wording. |
| P2.2 install gate rationale unclear | P2 | R18 added: document as rate-limit guard in roxygen + plan. |
| P2.3 existing test will break | P2 | R19 added: update existing test with stubs before Phase 3. |
| P2.4 no explicit idempotency tests | P2 | R20 added: explicit idempotency test scenarios. |
| P3.1 R7 solo ≠ team determinism | P3 | Clarified in R7 wording and Context section. |
| P3.2 lock reflects resolved HEADs | P3 | Clarified in R10/R12 descriptions. |
| P3.3 collapse gh_auth into gh_token | P3 | R3 updated: single `gh_token()` helper, no `gh_auth()`. |
| P3.4 .token removal is one site | P3 | Step 3 details corrected: `.token = creds$password` removed from the `gh::gh()` call in `get_branches()` (the sole call site). |
| P3.5 pagination cap at 100 | P3 | R1 changed to `.limit = Inf`; Step 1 updated accordingly. |

## Implementation Steps

## Phase 1: Low-risk quick wins (pagination + README token)

### 1. Full gh() pagination in get_branches() (S4)

- **Requirements**: R1
- **Files**: `R/get_branches.R`, `tests/testthat/test-get_branches.R`
- **Details**:
  - In `get_branches()` (`R/get_branches.R:22-24`), replace the `gh::gh()` call's
    `.limit = NULL` with `.limit = Inf`. This tells the `gh` package to paginate
    through ALL results — no artificial cap, no silent truncation at 30.
    `gh::gh()` handles pagination internally when `.limit = Inf`.
  - Keep the existing `.token = creds$password` for now (Step 3 will replace it
    with `gh_token()`). This step changes only the pagination argument so it
    stays minimal and conflict-free with the later token refactor.
  - Do not change the `vapply(out, \`[[\`, "", "name")` extraction shape.
  - **No truncation warning needed**: `.limit = Inf` fetches all pages, so
    there is no silent cap to warn about. If the GitHub API returns an error
    mid-pagination, `gh::gh()` propagates it naturally.
- **Test Scenarios**:
  - happy path: mocked `gh::gh` returns 45 branch objects → `get_branches()`
    returns a 45-length character vector (no truncation)
  - edge case: mocked `gh::gh` returns exactly 30 → returns 30 (unchanged)
  - error path: mocked `gh::gh` raises → propagates as before (no new behavior)
- **Tests**: `tests/testthat/test-get_branches.R` — extend the existing
  "get_branches works correctly" `test_that` (or add a new one) to stub
  `gh::gh` returning a 45-element list of `list(name = ...)` and assert
  `length(get_branches("pipapi", display = FALSE)) == 45`. Assert the call
  received `.limit = Inf` via `mockery::mock()` capture if feasible; otherwise
  assert the non-truncated count.
- **Acceptance criteria**: V1 passes; a 45-branch repo returns all 45.

### 2. Remove Codecov badge token from README (S3)

- **Requirements**: R2
- **Files**: `README.Rmd`, `README.md`, `NEWS.md`
- **Details**:
  - In `README.Rmd:19`, replace the badge URL
    `https://codecov.io/gh/PIP-Technical-Team/metapip/graph/badge.svg?token=5DMKVN16NM`
    with the tokenless form
    `https://codecov.io/gh/PIP-Technical-Team/metapip/graph/badge.svg`. Keep the
    link target (the `(...)` href) as-is.
  - Regenerate `README.md` from `README.Rmd` (`rmarkdown::render("README.Rmd")`
    or, since `eval = FALSE`, a careful hand-edit to mirror the Rmd). The two
    files MUST stay in sync with no `?token=` in either.
  - Add a `NEWS.md` bullet under the unreleased/next version noting the
    tokenless badge (rolled into the Step 8 NEWS pass; a placeholder here is
    fine).
- **Test Scenarios**:
  - happy path: `grep("token=", c(readLines("README.Rmd"), readLines("README.md")))`
    returns 0 hits
- **Tests**: no unit test (documentation); verified by the V2 grep check.
- **Acceptance criteria**: V2 passes — neither README file contains `token=`.

## Phase 2: Least-privilege token handling (S2)

### 3. Least-privilege tokens — gh_token() + redacted check_github_token() (S2)

- **Requirements**: R3, R4, R5, R6
- **Files**: `R/utils.R`, `R/get_branches.R`, `R/core_metadata.R`, `R/package_branches.R`, `tests/testthat/test-utils.R`, `tests/testthat/test-get_branches.R`
- **Details**:
  - New `gh_token()` helper in `R/utils.R` (internal, `@keywords internal`):
    - Honor `Sys.getenv("GITHUB_PAT")` / `"GITHUB_TOKEN"` first, returning the
      first non-empty value.
    - Fallback: safely attempt `gitcreds::gitcreds_get()` inside `tryCatch`; on
      success return `creds$password` (the token string); on any error
      (`gitcreds_nogit_error`, `gitcreds_no_credentials`, or other), return
      `NULL`. Never abort.
    - This is the SINGLE source of token retrieval. There is no separate
      `gh_auth()` — `gh_token()` returning `NULL` makes `gh::gh()` operate
      unauthenticated, which is valid for the public `PIP-Technical-Team` org.
    - `@return` roxygen: "Character string (the token) or `NULL` when no
      credentials are available."
  - `check_github_token()` (`R/utils.R:91-106`): keep it as the STRICT gate for
    install functions — it still aborts when no credentials are available.
    **Rationale (R18)**: this is a rate-limit guard (5000 authenticated vs 60
    unauthenticated req/hr), not security enforcement. The PIP-Technical-Team
    org is public, but install reliability requires the higher limit.
    Change its return value to a REDACTED list: return
    `list(name = "", password = "", protocol = "", ...)` with `password = ""`
    (and any secret fields blanked). Give the return a class
    `c("metapip_token", "list")` and add a `print.metapip_token` method that
    prints redacted fields. The REAL token is never in the return value —
    `remotes::install_github()` resolves gitcreds itself, so
    `check_github_token()` remains a validation gate, not a token carrier.
    Document the redaction in the `@return` roxygen and add a `@note`
    explaining the rate-limit guard rationale.
  - **Single call-site token removal (P3.4)**: In `get_branches()`
    (`R/get_branches.R:22-24`), replace `.token = creds$password` with
    `.token = gh_token()` (best-effort: passes the token if present, `NULL` if
    absent). This is the sole `gh::gh()` call site that currently carries
    `.token = creds$password` in `get_branches()`. Other read-only call sites
    (`core_metadata()` releases/commits calls in `R/core_metadata.R`;
    `package_branches()` transitively via `get_branches()`) are already
    covered.
  - Remove the leading `check_github_token()` call from `get_branches()`,
    `get_branch_info()`, `get_latest_branch_update()`, `core_metadata()`, and
    `package_branches()` — these are read-only for a public org and must work
    without a PAT.
  - Keep `check_github_token()` ONLY at the entry of `install_branch()` and
    `install_pip_packages()` (and `install_latest_branch()`'s install path).
  - Coordinate with Step 1: Step 1 left `.token = creds$password` in place;
    this step rewrites it to `.token = gh_token()`. Do Step 3 after Step 1 per
    the C2 ordering.
- **Test Scenarios**:
  - happy path (gh_token): creds present → `gh_token()` returns the string;
    creds absent → returns `NULL`, no error
  - edge case (read-only): `get_branches("pipapi")` with NO `GITHUB_PAT` and no
    gitcreds → returns branches (unauthenticated gh call succeeds against
    public repo, mock stubs `gh::gh`)
  - edge case (redaction): `print(check_github_token())` (with mocked creds
    whose `password = "secret")` outputs a string containing `""`, NOT
    `"secret"`
  - error path: gitcreds raises `gitcreds_nogit_error` → `gh_token()` returns
    `NULL`, no abort; `check_github_token()` still aborts (strict gate)
- **Tests**:
  - `tests/testthat/test-utils.R`: `test_that("gh_token returns the token when
    available")` — mock `gitcreds::gitcreds_get` to return
    `list(password = "abc")`; `expect_equal(gh_token(), "abc")`.
    `test_that("gh_token returns NULL when no creds and does not abort")` —
    mock `gitcreds::gitcreds_get` to raise
    `gitcreds_no_credentials`; `expect_null(gh_token())`. Use
    `withr::with_envvar(c(GITHUB_PAT = ""))` to ensure no env fallback leaks.
    `test_that("check_github_token redacts the PAT on print")` — mock
    `gitcreds::gitcreds_get` to return `list(name = "x", password = "secret",
    protocol = "https")`; capture `print(check_github_token())` output and
    `expect_false(grepl("secret", out))`; assert a redacted marker (`""`) is
    shown.
  - `tests/testthat/test-get_branches.R`: update the "get_branches works
    correctly" test so it does NOT rely on `check_github_token` (stub
    `gh_token` to `NULL` and assert `get_branches("pipapi")` still returns the
    mocked branches). Add `test_that("get_branches works without
    GITHUB_PAT")` running under `withr::with_envvar(c(GITHUB_PAT = ""))` with
    `gh_token` returning `NULL` and `gh::gh` stubbed.
- **Acceptance criteria**: V3, V4, V5, V6 pass; read-only functions no longer
  require a PAT; `check_github_token()` never prints the token.

## Phase 3: Supply-chain lock system (S1 A–D)

### 4. SHA-pinned install_branch() with force override + idempotency (S1-A)

- **Requirements**: R7, R8, R9, R19, R20
- **Files**: `R/install_pip_packages.R`, `tests/testthat/test-get_branches.R`
- **Details**:
  - Extend `install_branch()` signature to
    `install_branch(package = "pipapi", branch = NULL, force = FALSE, sha = NULL)`.
    `force` and `sha` are new optional params (non-breaking for existing
    positional callers).
  - Keep `check_github_token()` (install gate, per Step 3) and
    `check_package_condition(package)`. Resolve `branch` to
    `get_package_current_branch(package)` when `NULL` as today. Keep the
    single-branch and `get_branches()` validity check.
  - Behavior when `force = TRUE`: emit
    `cli::cli_alert_warning("force = TRUE bypasses the team lock; installing live HEAD of {.field {branch}}")`
    and install via `remotes::install_github("PIP-Technical-Team/{package}@{branch}")`
    (live branch HEAD). Skip SHA pinning and idempotency.
  - Behavior when `force = FALSE` (default):
    1. Resolve the target SHA:
       `target_sha <- sha %||% latest_commit_for_branch(package, branch)$sha`.
       (`sha` is supplied by the lock-driven callers in Steps 5/6; when
       `NULL`, resolve the branch HEAD SHA — "pin to current HEAD by default".)
    2. **Null-SHA guard (P1.2)**: if `is.null(target_sha)`, emit
       `cli::cli_abort("Could not resolve SHA for {.pkg {package}}@{branch}. Check network access or pass {.arg sha} explicitly.")`
       and return `invisible(NULL)`. This prevents a silent install at
       `@NULL` which would fail inside `remotes::install_github` with an
       opaque error.
    3. Idempotency check: read
       `local_sha <- utils::packageDescription(package, fields = "RemoteSha")`
       (suppressWarnings). If `!is.na(local_sha) && identical(local_sha, target_sha)`,
       emit `cli::cli_alert_info("{.pkg {package}} already at SHA {.code {target_sha}}; skipping")`
       and return `invisible(NULL)` WITHOUT installing.
    4. Otherwise install via
       `remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{target_sha}"))`.
  - Keep `detach_package(package)` before install (milestone-2 robustness).
  - Preserve the existing "Installing branch ... from package ..." info
    message, adjusted to mention the SHA when pinning (e.g. "...@{target_sha}").
  - Update roxygen `@param`/`@return` for the new `force` and `sha` params and
    regenerate docs in Step 8.
- **Test Scenarios**:
  - happy path: `force=FALSE`, installed `RemoteSha` differs from resolved SHA
    → `remotes::install_github` called with `@<sha>` (capture the arg)
  - edge case (idempotent, R20): installed `RemoteSha` == resolved SHA →
    `remotes::install_github` NOT called, "skipping" info emitted
  - edge case (no local install, R20): `packageDescription` returns `NA` →
    installs at resolved SHA (not skipped)
  - edge case (null-SHA, P1.2): `latest_commit_for_branch` returns
    `list(sha = NULL)` → `cli_abort` emitted, `remotes::install_github` NOT
    called, function returns `invisible(NULL)`
  - edge case (sha override): `sha = "deadbeef"` supplied → installs at that
    SHA regardless of branch HEAD
  - error path: `force=TRUE` → installs `@<branch>` (live), warning emitted,
    no SHA resolution/idempotency
- **Tests**: `tests/testthat/test-get_branches.R` — rewrite/extend the
  existing "install_branch works correctly" `test_that` (R19: this test
  currently lacks stubs for `latest_commit_for_branch` and
  `utils::packageDescription`, which will break when Phase 3 SHA resolution is
  added). Stub `latest_commit_for_branch` to return `list(sha = "abc123")`,
  stub `remotes::install_github` (capture args), stub `check_github_token`,
  `get_branches`, `detach_package`, `utils::packageDescription` (return `NA`
  by default). Assert: default call installs with an arg containing `@abc123`;
  `force=TRUE` installs with an arg containing `@<branch>` (not `@abc123`) and
  a warning is emitted; idempotent case — stub `utils::packageDescription` to
  return `"abc123"` and assert `remotes::install_github` is NOT called and an
  info "skipping" is emitted (R20); null-SHA case — stub
  `latest_commit_for_branch` to return `list(sha = NULL)` and assert
  `cli::cli_abort` is triggered and install is NOT called (P1.2).
- **Acceptance criteria**: V7, V8 pass; default install is SHA-pinned and
  idempotent; `force=TRUE` installs live HEAD; null-SHA aborts cleanly.

### 5. pip_snapshot() + PIP_LOCK manifest (S1-B)

- **Requirements**: R10, R11
- **Files**: `R/init_metapip.R` (or a new `R/pip_snapshot.R`), `inst/` (new
  `PIP_LOCK.csv` optional seed), `tests/testthat/test-pip_snapshot.R`, `NAMESPACE`
- **Details**:
  - **P1.1 fix — separate read/write paths**:
    - `pip_lock_path()` (internal, `@keywords internal`): READ-ONLY helper.
      Returns `system.file("PIP_LOCK.csv", package = "metapip")`. Used by
      `init_metapip()` and `update_pip_packages()` to read the committed lock.
      Returns `""` when the file is absent (installed package without a lock);
      callers guard with `file.exists(lock_path) && nzchar(lock_path)`.
    - `pip_snapshot(path = NULL)`: WRITE helper. Default `path` is
      `getOption("metapip.lock_path", system.file("PIP_LOCK.csv", package = "metapip"))`.
      In a `devtools::load_all()` session, `system.file()` points to the
      source `inst/` directory, so the default write target is the source-tree
      `inst/PIP_LOCK.csv` (committable). In an installed-package context, it
      writes to the library copy. The `path` arg lets callers override (e.g.,
      tests use `tempfile()`). This separation means install functions NEVER
      write to the lock; only `pip_snapshot()` and `update_pip_packages()`
      (via `pip_snapshot()` semantics) write.
  - For each core package (`get_core_pagkages()`), resolve its current
    branch via `get_package_current_branch(package)` and the branch HEAD SHA
    via `latest_commit_for_branch(package, branch)$sha`.
  - Build a data.frame with columns `package`, `branch`, `sha` (character;
    use `stringsAsFactors = FALSE` / collapse-friendly construction). Skip
    packages whose branch/SHA could not be resolved (record a
    `cli_alert_warning` naming them) rather than writing `NA` SHAs into the
    lock.
  - Write with `utils::write.csv(lock_df, path, row.names = FALSE)`. Emit
    `cli::cli_alert_success("Wrote PIP_LOCK to {.path {path}} ({n} packages)")`.
  - Return `invisible(path)`.
  - Seed `inst/PIP_LOCK.csv` is OPTIONAL for the first commit (the file is
    created by `pip_snapshot()`); do not commit an empty/stub lock unless the
    maintainer runs `pip_snapshot()` to populate it. Add `inst/PIP_LOCK.csv`
    to `.Rbuildignore` ONLY if it should not ship — but per design it SHOULD
    ship (read via `system.file`), so do NOT add to `.Rbuildignore`.
  - Add `@export` + full roxygen (`@param path`, `@return`,
    `@examples \dontrun{ pip_snapshot() }`).
  - `init_metapip()` and `update_pip_packages()` (Step 6) consume the lock via
    `pip_lock_path()` + `utils::read.csv(..., stringsAsFactors = FALSE)`.
  - **Lock semantics (P3.2)**: The lock records the resolved HEAD SHAs at the
    time `pip_snapshot()` runs. It does NOT reflect what is installed locally.
    Two team members who both run `init_metapip()` from the same committed
    lock get the same SHAs regardless of their local install state.
- **Test Scenarios**:
  - happy path: mocked `latest_commit_for_branch` returns distinct SHAs for
    all core packages → `pip_snapshot(tmp)` writes a CSV with a header
    `package,branch,sha` and one row per core package; `read.csv(tmp)` round-
    trips to the same data
  - edge case: one package's branch unresolvable → that row is omitted, a
    warning names it, others are written
  - error path: `path` is in a non-writable location → error propagates
    (do not swallow); document that callers choose a writable `path`
- **Tests**: `tests/testthat/test-pip_snapshot.R` (new) — use
  `tf <- tempfile(fileext = ".csv")`; stub `get_core_pagkages`,
  `get_package_current_branch`, `latest_commit_for_branch`; call
  `pip_snapshot(path = tf)`; `expect_true(file.exists(tf))`;
  `expect_equal(read.csv(tf), <expected df>)` (column names
  `package,branch,sha`); `expect_equal(nrow(read.csv(tf)), length(core))`. Add
  the unresolvable-branch edge case.
- **Acceptance criteria**: V9 passes; `pip_snapshot()` is exported and writes
  a valid CSV lock.

### 6. Lock-driven init_metapip() and lock-updating update_pip_packages() (S1-C)

- **Requirements**: R12, R13
- **Files**: `R/init_metapip.R`, `tests/testthat/test-init_metapip.R`
- **Details**:
  - `init_metapip()`: change from a pure delegate to lock-driven:
    1. `lock_path <- pip_lock_path()`. If `file.exists(lock_path) && nzchar(lock_path)`:
       read `lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)`. For
       each row, call `install_branch(package = lock$package[i],
       branch = lock$branch[i], sha = lock$sha[i], force = FALSE)` — installs at
       the recorded SHA (Step 4's `sha` override path). Wrap in the same
       per-package `tryCatch` failure isolation as `update_pip_packages()`
       (milestone 2). Then `metapip_attach()`.
    2. If the lock is ABSENT: fall back to the current branch-HEAD behavior —
       resolve each core package's branch, install at the resolved HEAD SHA
       via `install_branch(package, branch)` (Step 4 default) — and emit
       `cli::cli_alert_info("No PIP_LOCK found; installing at branch HEAD. Run {.fn pip_snapshot} to create a team lock manifest.")`.
       Then `metapip_attach()`.
    - Preserve the `exclude` / `ask` / `answer` params for compatibility; in
      lock-driven mode `ask` gates an interactive "install N packages from
      the lock?" prompt reusing the milestone-2 `interactive()` gate.
  - `update_pip_packages()`: evolve to refresh the lock:
    1. Resolve current HEAD SHA for each core package's branch via
       `latest_commit_for_branch(package, branch)$sha` (build the candidate
       lock table).
    2. Use the tri-state `compare_sha()` (milestone 2) to classify each
       package: `TRUE` (already at HEAD), `FALSE` (outdated), `"unknown"`
       (CRAN/no git metadata), `NULL` (branch not found). Keep the existing
       `null_vec`/`unknown_pkgs`/`missing_pkgs` classification and warnings.
    3. Write/update `PIP_LOCK.csv` via `pip_snapshot()` semantics (or directly
       write the candidate lock table through `pip_lock_path()`). The lock now
       records the resolved HEAD SHAs (the new agreed versions). Emit
       `cli::cli_alert_info("Updated {.path PIP_LOCK} — commit this change.")`.
    4. Preserve the milestone-2 interactive gate (`ask`/`interactive()`/
       `utils::menu()` non-interactive default) and per-package failure
       isolation in the install loop. Install `missing_pkgs` at their new
       resolved SHA via `install_branch(pkg, branch, sha = resolved_sha)`.
    5. Keep the CRAN-gap `renv` info alert (milestone 2).
    - IMPORTANT: because `update_pip_packages()` still uses
      `mapply(compare_sha, ...)`, apply the brain gotcha in tests — keep mock
      `compare_sha` return values simple (single value per test block); do not
      rely on per-argument conditional mocks.
  - The relationship: `init_metapip()` = deterministic install-from-lock;
    `update_pip_packages()` = refresh-lock-then-install-outdated. They no
    longer share a single delegation path. Update `init_metapip()`'s body
    accordingly and adjust the existing `init_metapip forwards its answer
    argument` test if it asserted pure delegation.
- **Test Scenarios**:
  - happy path (init, lock present): mocked `pip_lock_path` → a tempfile
    CSV with 2 packages; `install_branch` stubbed (capture `sha` arg) → called
    with the recorded SHAs; `metapip_attach` reached
  - edge case (init, lock absent): `pip_lock_path` → nonexistent → installs at
    branch HEAD (default `install_branch`), emits "No PIP_LOCK found /
    pip_snapshot" suggestion, `metapip_attach` reached
  - happy path (update): mocked `latest_commit_for_branch` + simple
    `compare_sha` returning `FALSE` → `PIP_LOCK` (tempfile) is written with
    resolved SHAs, outdated packages installed at those SHAs, summary printed
  - edge case (update): `compare_sha` returns `"unknown"` (single value,
    per the brain gotcha) → those packages skipped with warning, NOT in
    `missing_pkgs`, lock still written
  - edge case (update): one install fails → failure isolation (milestone 2)
    preserved, run completes, lock still updated
  - edge case (update): non-interactive + `ask=TRUE` → no `utils::menu` call,
    default install (milestone 2 gate preserved)
- **Tests**: `tests/testthat/test-init_metapip.R` — add `test_that` blocks
  using `tempfile(fileext = ".csv")` for the lock. Stub `pip_lock_path` (or
  `system.file`) to the tempfile, `install_branch` (capture `sha`), `metapip_attach`,
  `get_core_pagkages`, `get_package_current_branch`, `latest_commit_for_branch`,
  and `compare_sha` (single-value mocks). Keep the existing milestone-2 tests
  (failure isolation, interactive gate, unknown-skip, CRAN-gap, up-to-date)
  passing by ensuring the new lock-writing path is additive where they assert.
  Where an existing test asserts `init_metapip` delegates to
  `update_pip_packages`, update it to reflect lock-driven behavior.
- **Acceptance criteria**: V10, V11 pass; `init_metapip()` is lock-driven with
  fallback; `update_pip_packages()` refreshes the lock and preserves
  milestone-2 robustness.

### 7. install_latest_branch() becomes developer-only (S1-D)

- **Requirements**: R14
- **Files**: `R/install_pip_packages.R`, `tests/testthat/test-install_latest_branch.R`
- **Details**:
  - At the top of `install_latest_branch()`, emit
    `cli::cli_alert_warning("install_latest_branch() bypasses the team lockfile. Use {.fn pip_snapshot} + {.fn init_metapip} for team-consistent installs.")`.
  - In the per-package install decision (after the existing `compare_sha`
    short-circuit), call `install_branch(pkg, brn, force = TRUE)` so the
    latest-branch install deliberately bypasses the lock and installs live
    branch HEAD (consistent with its "latest branch" intent). Suppress the
    inner `force = TRUE` warning from Step 4 here to avoid double-warning,
    since `install_latest_branch` already warned about bypassing the lock.
  - Keep the existing `get_latest_branch_update`-based gathering and
    `compare_sha` short-circuit (skip packages already at HEAD) from
    milestone 2.
- **Test Scenarios**:
  - happy path: warning "bypasses the team lockfile" emitted once;
    `install_branch` called with `force = TRUE` (capture the arg) for packages
    not at HEAD
  - edge case: package at HEAD (`compare_sha` TRUE) → skipped (milestone-2
    short-circuit intact), not installed
- **Tests**: `tests/testthat/test-install_latest_branch.R` — update the
  existing tests to assert the bypass warning is emitted and that
  `install_branch` is called with `force = TRUE` (capture args via a mock).
  Keep the "already at HEAD → skipped" and "unknown → install" cases
  (adjusting the install call to reflect `force = TRUE`).
- **Acceptance criteria**: V12 passes; `install_latest_branch()` is
  developer-only with `force=TRUE` internally.

## Phase 4: Integration verification, docs, and full check

### 8. Integration verification, roxygen, NEWS, full check

- **Requirements**: R15, R16, R17, R18, R19, R20, R21, R22
- **Files**: `tests/testthat/test-init_metapip.R`, `NEWS.md`, `man/`, `NAMESPACE`, `compound-gpid.md`, `DESCRIPTION` (only if a new dependency is needed — none expected)
- **Details**:
  - **Charter update (R17)**: In `compound-gpid.md`, find the constraint that
    says "Requires valid GitHub credentials/token" and update it to note that
    read-only functions (`get_branches`, `core_metadata`, `package_branches`,
    `get_branch_info`, `get_latest_branch_update`) work without a token for
    the public `PIP-Technical-Team` org, while install functions
    (`install_branch`, `install_pip_packages`) still require one for
    rate-limit reliability.
  - **Install gate rationale (R18)**: In `check_github_token()` roxygen
    `@note`, document: "This gate is a rate-limit guard (5000 authenticated
    vs 60 unauthenticated GitHub API requests/hour). Install reliability
    requires the higher limit. The PIP-Technical-Team org is public; this is
    not a security gate."
  - Add an integration `test_that("init_metapip(ask=FALSE) installs from a
    mock PIP_LOCK and attaches")` with a `tempfile` lock, mocked
    `pip_lock_path`/`install_branch`/`metapip_attach`; assert installs happen
    at the recorded SHAs and attach is reached. Add
    `test_that("update_pip_packages() writes PIP_LOCK and installs outdated
    at new SHAs")` with a tempfile lock and simple `compare_sha` mocks (per
    the brain gotcha).
  - Run `Rscript -e "devtools::document()"` (roxygen comments changed in
    Steps 3, 4, 5: new `gh_token`/`pip_snapshot` exports/params,
    `install_branch` `force`/`sha` params, `check_github_token` redaction
    `@return`). Commit regenerated `man/` + `NAMESPACE`. Verify `pip_snapshot`
    is in `NAMESPACE` and `gh_token` is NOT (internal).
  - `NEWS.md`: add an unreleased/next-version section listing the supply-chain
    changes: SHA-pinned `install_branch()` with `force` override + idempotency,
    `PIP_LOCK.csv` manifest + `pip_snapshot()`, lock-driven `init_metapip()` /
    lock-refreshing `update_pip_packages()`, developer-only
    `install_latest_branch()`, least-privilege tokens (read-only calls work
    without PAT, redacted `print(check_github_token())`), full `get_branches()`
    pagination, tokenless Codecov badge.
  - Verify no new dependency is required (`remotes`, `gh`, `gitcreds`, `cli`,
    `glue`, `collapse` already in Imports). If `read.csv`/`write.csv`/tempfile
    suffice, no DESCRIPTION change.
  - Final gate: `Rscript -e "devtools::test()"` then
    `Rscript -e "rcmdcheck::rcmdcheck()"` (per `AGENTS.md`). Confirm no live
    GitHub calls run in the suite (all network stubbed).
- **Test Scenarios**:
  - happy path: full mocked `init_metapip` from a lock completes; attach
    reached
  - happy path: `update_pip_packages` writes lock and installs outdated
- **Tests**: as above; plus the suite-wide commands.
- **Acceptance criteria**: V13, V14 pass; full suite + `rcmdcheck` green
  offline.

## Testing Strategy

- Unit tests with `mockery::stub` for all network/credential-touching
  functions (`gh::gh`, `remotes::install_github`, `gitcreds::gitcreds_get`,
  `latest_commit_for_branch`, `get_branches`, `install_branch`,
  `check_github_token`/`gh_token`, `compare_sha`, `interactive`,
  `utils::menu`, `packageDescription`). No live GitHub calls in unit tests.
- Use `tempfile(fileext = ".csv")` for all `PIP_LOCK` tests; never write to
  the real `inst/PIP_LOCK.csv` in tests. Stub `pip_lock_path()`/`system.file`
  to point tests at the tempfile.
- Brain gotcha (apply throughout): `update_pip_packages()` uses
  `mapply(compare_sha, ...)`; a mocked `compare_sha` returning different
  values per argument is not reliably intercepted. Keep mock return values
  simple (single value) and test each code path (`TRUE`/`FALSE`/`"unknown"`/
  `NULL`) in a separate `test_that` block. Source:
  `.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md`.
- Preserve all milestone-2 tests (failure isolation, interactive gate,
  unknown-skip, CRAN-gap, up-to-date, short-circuit) — the lock-writing path
  is additive; where a test asserts `init_metapip` delegates to
  `update_pip_packages`, update it to the lock-driven contract.
- `install_branch` tests live in `test-get_branches.R` (existing location);
  extend rather than relocate to minimize churn.
- **R19 pre-fix**: Before adding SHA resolution to `install_branch()`, update
  the existing test at `test-get_branches.R:18-29` to stub
  `latest_commit_for_branch` (return `list(sha = "abc123")`) and
  `utils::packageDescription` (return `NA`). This prevents the test from
  breaking when Phase 3 SHA logic activates.
- **R20 idempotency tests**: Add dedicated `test_that` blocks for: (a)
  `RemoteSha == target_sha` → skip with info; (b) `RemoteSha` is `NA` →
  proceeds with install; (c) `RemoteSha` is `NULL` (uninstalled package) →
  proceeds with install.
- Final gate: `Rscript -e "devtools::test()"` then
  `Rscript -e "rcmdcheck::rcmdcheck()"` (per `AGENTS.md`).

## Documentation Checklist

- [ ] `R/utils.R` — roxygen for new `gh_token()` (`@keywords internal`), and
      updated `check_github_token()` `@return` (redaction) + `@note`
      (rate-limit guard rationale, R18).
- [ ] `R/install_pip_packages.R` — `@param force`, `@param sha` for
      `install_branch()`; `install_latest_branch()` developer-only note.
- [ ] `R/init_metapip.R` — `pip_snapshot()` full roxygen (`@export`,
      `@param path`, `@return`, `@examples`); `init_metapip()`/`update_pip_packages()`
      lock-driven behavior in `@description`/`@returns`.
- [ ] `compound-gpid.md` — charter constraint update for token requirement
      (R17).
- [ ] `README.Rmd` + `README.md` — tokenless Codecov badge (Step 2); consider a
      short "Lock manifest" note pointing to `pip_snapshot()`/`PIP_LOCK.csv`.
- [ ] `NEWS.md` — unreleased-version section (Step 8).
- [ ] `man/` + `NAMESPACE` — regenerated via `devtools::document()`;
      `pip_snapshot` exported, `gh_token` internal.

## Risks & Mitigations

| ID | Risk | Impact | Mitigation |
|----|------|--------|------------|
| K1 | S2 (Step 3) and S4 (Step 1) both edit `get_branches()`'s `gh::gh` call | merge conflict / rework | C2 ordering: Step 1 adds `.limit = Inf` only (keeps `.token`), Step 3 rewrites the token. Keep commits ordered S4 → S2. |
| K2 | Redacting `check_github_token()` return breaks callers that read `creds$password` for installs | runtime error in install path | Audit all `check_github_token()` consumers; `remotes::install_github` resolves gitcreds itself, so the gate's return value is not a token carrier. Replace any `creds$password` reads with `gh_token()` (Step 3). Add a test asserting installs still work with redaction. |
| K3 | `mapply(compare_sha, ...)` in `update_pip_packages()` resists per-argument mocking → tests unreliable | flaky/false-green tests | Apply the brain gotcha: single-value mocks, one path per `test_that`. Test the lock-writing classification via direct lock-file assertions, not via conditional `compare_sha` mocks. |
| K4 | Lock-driven `init_metapip()` diverges from `update_pip_packages()`; existing delegation tests break | test churn / wrong behavior | Step 6 explicitly updates the delegation test to the lock-driven contract; keep `update_pip_packages()` as the lock-refresh path. Document the new relationship in roxygen. |
| K5 | `pip_lock_path()` via `system.file()` returns `""` when the lock is absent | `file.exists("")` is FALSE (correct), but `read.csv("")` errors | Guard with `file.exists(lock_path) && nzchar(lock_path)` before reading; fall back to branch-HEAD path. Test the absent-lock branch explicitly. |
| K6 | "Unblock the Core" (V1–V4) not merged to `master` → verification assumes fixes that aren't there | false green / false red | Blocked-Stop C6: confirm `master` state before final verification; ask if absent. |
| K7 | `system.file("PIP_LOCK.csv")` under `load_all` points to source `inst/` — `pip_snapshot()` default writes there, which may be read-only in some setups | write failure on installed package | P1.1 fix: `pip_snapshot(path=)` default uses `getOption("metapip.lock_path", system.file(...))`. Document the maintainer workflow (run `pip_snapshot()` in a `load_all` dev session, then commit `inst/PIP_LOCK.csv`). Tests always use `tempfile()`. |
| K8 | Adding `force`/`sha`/`path` params to public functions changes `R CMD check` examples/docs surface | check NOTEs | Update roxygen `@param`/`@examples` in Steps 4–6; regenerate in Step 8. |
| K9 | Removing `check_github_token()` from read-only functions lowers GitHub rate-limit headroom in CI without a token | throttled/flaky CI | `gh_token()` still passes the token when present (best-effort), so authenticated rate limits apply when creds exist. Only the REQUIREMENT is removed, not the opportunistic use. |
| K10 | `core_metadata()` reads columns (`last_commit_author_name`, `last_update_time`) that `get_branch_info`/`get_latest_branch_update` rename; pre-existing inconsistency could surface after token/pagination changes | `core_metadata` errors | C7 boundary: do NOT touch `core_metadata()` column handling (milestone 4 / R5). If a regression appears, stop and ask rather than fixing across milestones. |
| K11 | Null-SHA guard in `install_branch` could mask upstream issues in `latest_commit_for_branch` | silent data loss | The `cli_abort` message names the package and branch explicitly, making the failure visible and actionable. The caller can pass `sha =` explicitly to override. |
| K12 | Existing `install_branch` test (R19) lacks stubs and will break when SHA resolution is added | CI red on Phase 3 entry | Pre-fix the test in Step 4 BEFORE adding SHA resolution logic, stubbing `latest_commit_for_branch` and `utils::packageDescription`. |

## Out of Scope

- httr2 fetch / UTC timestamp parsing / `get_latest_branch_update()` timestamp
  correctness (milestone 4, `resilient-networking-time`).
- API memoization / `requireNamespace` checks / CI hardening / 0.1.0 release
  (milestone 5, `performance-tests-release`).
- The V1–V4 core bug fixes (milestone 1, `unblock-the-core`) — this plan
  assumes they are merged (C6).
- Changing `core_metadata()`'s timestamp/column assumptions (milestone 4).
- Full `renv` integration beyond the existing `renv` companion-tool note.
- Any breaking change to public API signatures beyond adding optional params
  (`force`, `sha`, `path`).

## Completion Contract

### Outcome

The metapip supply chain is locked down: `install_branch()` pins to a commit
SHA by default (with `force = TRUE` override and idempotent skip), a committed
`PIP_LOCK.csv` manifest drives deterministic team installs via `init_metapip()`
and is refreshed by `update_pip_packages()` / `pip_snapshot()`,
`install_latest_branch()` is a developer-only tool, read-only GitHub calls no
longer require a PAT and `check_github_token()` never prints the token,
`get_branches()` paginates fully, and the README Codecov badge carries no
token. All changes ship as one PR to `master`; `devtools::test()` and
`rcmdcheck` pass offline.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Phase | Required |
|----|-------------------|------------------|-------|----------|
| V1 | `get_branches("pipapi")` returns all branches even if >30 exist (`.limit = Inf`) | `test-get_branches.R` (mock gh returning 45 branches) | 1 | yes |
| V2 | README Codecov badge URL has no `?token=`; `README.Rmd` and `README.md` in sync | `grep -n "token=" README.Rmd README.md` (0 hits) | 1 | yes |
| V3 | `gh_token()` returns the token when present, `NULL` when absent (no abort) | `test-utils.R` | 2 | yes |
| V4 | `get_branches("pipapi")` works with no `GITHUB_PAT` / no creds (public org) | `test-get_branches.R` | 2 | yes |
| V5 | `print(check_github_token())` shows `""` (redacted), never the PAT | `test-utils.R` | 2 | yes |
| V6 | Only `install_branch()` and `install_pip_packages()` call `check_github_token()`; read-only fns use `gh_token()` | `grep` + `test-utils.R`/`test-get_branches.R` | 2 | yes |
| V7 | `install_branch("pipapi","DEV")` installs at resolved HEAD SHA (`@<sha>`) and skips when installed `RemoteSha` already matches | `test-get_branches.R` | 3 | yes |
| V8 | `install_branch(..., force=TRUE)` installs live branch HEAD (`@<branch>`) with a bypass warning | `test-get_branches.R` | 3 | yes |
| V9 | `pip_snapshot()` writes/updates `PIP_LOCK.csv` (`package,branch,sha`) for all core packages; exported + roxygen-documented | `test-pip_snapshot.R` (tempfile) | 3 | yes |
| V10 | `init_metapip(ask=FALSE)` reads `PIP_LOCK` and installs each package at its recorded SHA; falls back to branch HEAD + suggests `pip_snapshot()` when absent | `test-init_metapip.R` (tempfile lock) | 3 | yes |
| V11 | `update_pip_packages()` resolves HEAD SHAs, updates `PIP_LOCK`, preserves milestone-2 failure isolation + interactive gate | `test-init_metapip.R` (tempfile lock) | 3 | yes |
| V12 | `install_latest_branch()` emits "bypasses the team lockfile" warning and calls `install_branch(..., force=TRUE)` | `test-install_latest_branch.R` (updated) | 3 | yes |
| V13 | Full test suite + `rcmdcheck` pass offline (no live GitHub) | `Rscript -e "devtools::test()"`, `rcmdcheck::rcmdcheck()` | 4 | yes |
| V14 | `NEWS.md` updated; `man/`+`NAMESPACE` regenerated for `pip_snapshot` export + changed signatures | git diff | 4 | yes |

### Constraints

| ID | Constraint | Check | Phase |
|----|------------|-------|-------|
| C1 | Single mergeable PR targeting `master` | one PR, base = master | final |
| C2 | Implement in order S4 → S3 → S2 → S1A → S1B → S1C → S1D | commit log order | all |
| C3 | `PIP_LOCK.csv` lives at `inst/PIP_LOCK.csv`, read via `system.file()`; `pip_snapshot()` writes via `getOption("metapip.lock_path", system.file(...))`; tests use `tempfile()` and a `path` arg | code review + tests | 3 |
| C4 | Preserve milestone-2 behaviors: tri-state `compare_sha`, per-package failure isolation, interactive gate, CRAN-gap alert | existing tests stay green | 3 |
| C5 | Use collapse/cli/glue + mockery; no live GitHub in unit tests | offline testthat | all |
| C6 | "Unblock the Core" (V1–V4) on `master` before final verification | confirm master state | final |
| C7 | Do not modify `core_metadata()` column/timestamp handling (milestone 4 scope) | scope boundary | all |

### Boundaries

- **Allowed**: `R/get_branches.R`, `R/install_pip_packages.R`, `R/init_metapip.R`,
  `R/utils.R`, `R/core_metadata.R` (only `latest_commit_for_branch`, no behavior
  change), `README.Rmd`, `README.md`, `NEWS.md`, `inst/PIP_LOCK.csv` (new),
  `compound-gpid.md` (charter wording update), `tests/testthat/*`, regenerated
  `man/`+`NAMESPACE`.
- **Out of scope**: httr2 fetch / UTC timestamps (milestone 4), API memoization
  / `requireNamespace` / CI hardening / 0.1.0 release (milestone 5), the V1–V4
  core fixes (milestone 1), changing `core_metadata()` timestamp/column
  handling, full `renv` integration.

### Iteration Policy

1. If a token/SHA refactor surfaces an additional credential or
   install-robustness issue in the same code area, ask before expanding scope
   (deviation-policy: `ask`).
2. If a mocking strategy fails to isolate `remotes::install_github` /
   `gitcreds` cleanly, ask before falling back to live calls.
3. If "Unblock the Core" (V1–V4) is not on `master`, ask whether to proceed
   against current `master` or wait.
4. If updating `update_pip_packages()` for lock-writing would break an
   existing milestone-2 test's assertion intent, ask before changing that
   test's intent rather than silently rewriting it.

### Blocked-Stop Conditions

- A required milestone-1 (V1–V4) dependency blocks verification of a fix → stop
  and ask.
- A fix would require a breaking change to a public API signature beyond
  adding optional params (`force`, `sha`, `path`) → stop and ask.
- Tests cannot be written without live GitHub credentials → stop and ask.
- `core_metadata()` regresses as a side effect and the fix crosses into
  milestone-4 scope → stop and ask.
