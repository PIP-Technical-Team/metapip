---
date: 2026-08-13
depth: light
parent-review: .cg-docs/reviews/2026-08-12-unblock-the-core-review.md
type: verification
plan: .cg-docs/plans/2026-08-13-harden-install-pipeline.md
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: open
  P3.1: open
  P3.2: open
  P3.3: fixed
  P3.4: fixed
  P3.5: open
---

# Verify Review Report

**Review mode**: light (verify pass)
**Files reviewed**: 10
**Findings**: 11 (P0: 0, P1: 0, P2: 6, P3: 5)

## P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/init_metapip.R:3` — Malformed roxygen `@param` link syntax: `if [getwd]` uses Markdown link syntax instead of inline code.
  **Fix**: Change to `` if `getwd()` ``

- **[P2.2]** [cg-code-quality] `R/init_metapip.R:4` — Typo in roxygen `@param` description: `"excluded be default"` should be `"excluded by default"`.

- **[P2.3]** [cg-code-quality] `R/init_metapip.R:11` — Grammar error in roxygen `@returns`: `"return invisible() output"` should be `"returns invisible() output"`.

- **[P2.4]** [cg-code-quality] `R/install_pip_packages.R:110` — Uses `=` for assignment instead of `<-` in function body.

- **[P2.5]** [cg-testing] `test-init_metapip.R` — Untested branch: all packages up-to-date path (compare_sha returns TRUE for all).

- **[P2.6]** [cg-testing] `test-get_branches.R:49-52, 66-69` — Empty test blocks with no active assertions (pre-existing).

## P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/init_metapip.R` — Inconsistent roxygen `#'` spacing (missing space after `#'`).

- **[P3.2]** [cg-code-quality] `R/init_metapip.R:86` — Line exceeds 80 characters.

- **[P3.3]** [cg-testing] `tests/testthat/test-init_metapip.R:148` — Unused variable `install_calls`.

- **[P3.4]** [cg-testing] `tests/testthat/test-init_metapip.R:118` — Dead mock never reached (`latest_commit_for_branch` stub when `compare_sha` is already mocked).

- **[P3.5]** [cg-testing] `tests/testthat/test-install_latest_branch.R` — All tests use single package; multi-package iteration untested.

## Passed

- No P0 or P1 findings
- No cross-file breakage detected
- All mocking targets sit at correct I/O boundary
- All assertions are specific and non-tautological
