---
date: 2026-08-14
plan: .cg-docs/plans/2026-08-14-lock-down-supply-chain.md
active-deviation-policy: ask
runtime-deviation-policy: null
status: completed
completed-phases: [1, 2, 3, 4]
current-phase: null
evidence:
  V1: passed
  V2: passed
  V3: passed
  V4: passed
  V5: passed
  V6: passed
  V7: passed
  V8: passed
  V9: passed
  V10: passed
  V11: passed
  V12: passed
  V13: passed
  V14: passed
constraints:
  C1: pending
  C2: passed
  C3: passed
  C4: passed
  C5: passed
  C6: passed
  C7: passed
deviations:
  - "D1 (2026-08-14): plan body edit — added R15, R16 to Step 8 **Requirements** so the plan passes cg-render-artifact --validate-only preflight (renderer requires every requirement ID mapped to a step). User approved via decision prompt. Impact: plan validation now passes (exit 0)."
  - "D2 (2026-08-14): vignette fix — added \\VignetteDepends{metapip} + library(metapip) to vignettes/package-specific-options.Rmd so R CMD check's vignette code-run resolves exported functions. Pre-existing failure outside plan 'Allowed' files; user approved. Impact: rcmdcheck ERROR resolved."
  - "D3 (2026-08-14): C6 resolution — merged origin/master into the feature branch (user approved). Master contained the milestone-1 .onAttach test fix (0b6d008) missing on the branch; merge preview was conflict-free (one NEWS.md conflict resolved preserving both sides). Also brought milestone-4 networking commits into the tree so the eventual PR diff contains only this plan's changes. Impact: test-attach.R passes; V13 unblocked."
  - "D4 (2026-08-14): implement-robustness note — replaced `%||%` (not imported into the package NAMESPACE) with an explicit NULL check in install_branch() so SHA resolution does not depend on a loaded dependency namespace."
accepted-exceptions: []
failing-steps: []
---

# Execution Report: Lock Down the Supply Chain

## Plan Reference

`.cg-docs/plans/2026-08-14-lock-down-supply-chain.md`

## Run: 2026-08-14

- Started: 2026-08-14T17:59:45-04:00
- Completed: 2026-08-14T18:45:00-04:00
- Mode: ALL phases, review:auto
- Artifact preflight: `cg-render-artifact --validate-only` failed initially
  (R15/R16 unmapped), plan body fixed per user approval (see Deviations D1),
  re-validated clean (exit 0).

## Completed Steps

1. get_branches() full pagination (`.limit = Inf`) — R/get_branches.R, tests (V1).
2. Tokenless codecov badge — README.Rmd + README.md in sync, NEWS placeholder (V2).
3. gh_token() + redacted check_github_token() + read-only fns without PAT —
   R/utils.R, R/get_branches.R, R/core_metadata.R, R/package_branches.R, tests (V3-V6).
4. SHA-pinned install_branch(force, sha) + idempotency + null-SHA guard —
   R/install_pip_packages.R, tests (V7, V8).
5. pip_snapshot() + pip_lock_path() — R/pip_snapshot.R, inst semantics, tests (V9).
6. Lock-driven init_metapip() + lock-refreshing update_pip_packages() —
   R/init_metapip.R, tests (V10, V11).
7. install_latest_branch() developer-only (warn + force=TRUE) — R/install_pip_packages.R, tests (V12).
8. Charter R17, gate-note R18, roxygen + NEWS + man/NAMESPACE regen, full
   devtools::test() + rcmdcheck (V13, V14). Pre-existing vignette JSON-header
   issue fixed per user approval (D2); C6 resolved by merging master (D3).

## Deviations

- D1 (2026-08-14): plan body edit — added `R15, R16` to Step 8 **Requirements**
  so the plan passes the `cg-render-artifact --validate-only` preflight (renderer
  requires every requirement ID mapped to a step). User approved via the blocked
  plan question. Impact: validation now passes.
- D2 (2026-08-14): vignette fix — added `\VignetteDepends{metapip}` +
  `library(metapip)` to `vignettes/package-specific-options.Rmd` so R CMD check's
  vignette code-run resolves exported functions. Pre-existing failure outside the
  plan's allowed-files list; user approved. Impact: rcmdcheck ERROR resolved.
- D3 (2026-08-14): C6 resolution — merged `origin/master` into the feature branch
  (user approved). Master contained the milestone-1 `.onAttach` test fix
  (`0b6d008`) missing on the branch; merge preview was conflict-free (one NEWS.md
  conflict resolved preserving both sides). Brought milestone-4 networking commits
  into the tree so the eventual PR diff contains only this plan's changes.
  Impact: test-attach.R passes; V13 unblocked.
- D4 (2026-08-14): implement-robustness note — replaced `%||%` (not imported into
  the package NAMESPACE) with an explicit NULL check in `install_branch()` so SHA
  resolution does not depend on a loaded dependency namespace.

## Accepted Exceptions

(No accepted exceptions)

## Evidence Table

| ID | Phase | Status | Artifact |
|----|-------|--------|----------|
| V1 | 1 | passed | test-get_branches.R (mock gh returning 45 branches) |
| V2 | 1 | passed | grep -n "token=" README.Rmd README.md (0 hits) |
| V3 | 2 | passed | test-utils.R (gh_token) |
| V4 | 2 | passed | test-get_branches.R (no GITHUB_PAT) |
| V5 | 2 | passed | test-utils.R (redacted print) |
| V6 | 2 | passed | grep + test-utils.R/test-get_branches.R |
| V7 | 3 | passed | test-get_branches.R (SHA-pinned install) |
| V8 | 3 | passed | test-get_branches.R (force=TRUE) |
| V9 | 3 | passed | test-pip_snapshot.R (tempfile) |
| V10 | 3 | passed | test-init_metapip.R (tempfile lock) |
| V11 | 3 | passed | test-init_metapip.R (tempfile lock) |
| V12 | 3 | passed | test-install_latest_branch.R |
| V13 | final | passed | devtools::test() green (150 pass); rcmdcheck 0 errors (2 pre-existing warnings/notes: R>=4.1.0, pandoc-missing vignette build) |
| V14 | final | passed | git diff (NEWS.md, man/, NAMESPACE: pip_snapshot exported, gh_token internal) |

## Constraints Check

| ID | Constraint | Status |
|----|------------|--------|
| C1 | Single mergeable PR targeting master | pending (op 5 /cg-commit-push-pr) |
| C2 | Implement in order S4 → S3 → S2 → S1A → S1B → S1C → S1D | passed |
| C3 | PIP_LOCK.csv lives at inst/, read via system.file(); tests use tempfile() | passed |
| C4 | Preserve milestone-2 behaviors | passed |
| C5 | Use collapse/cli/glue + mockery; no live GitHub in unit tests | passed |
| C6 | "Unblock the Core" on master before final verification | passed (master merged; test-attach green) |
| C7 | Do not modify core_metadata() column/timestamp handling | passed |

## Remaining Uncertainty

- C1: PR creation happens in operation 5 (/cg-commit-push-pr).
- V13: rcmdcheck still reports pre-existing environmental WARNING/NOTE
  (R >= 4.1.0 lambda note; pandoc missing so vignette HTML/`inst/doc` not built;
  `.git` hidden-file note; MIT + file LICENSE note). No errors.

## Final Status

completed
