---
date: 2026-08-14
depth: light
parent-review: .cg-docs/reviews/2026-08-12-unblock-the-core-review.md
type: verification
findings:
  P1.1: open
  P1.2: open
  P1.3: open
  P2.1: open
  P2.2: open
  P2.3: open
  P3.1: open
---

# Verify Review: Lock Down the Supply Chain (mode:verify)

**Review mode**: verification (light depth, forced by mode:verify)
**Parent review**: `.cg-docs/reviews/2026-08-12-unblock-the-core-review.md`
**Files reviewed**: R/{get_branches,utils,install_pip_packages,init_metapip,pip_snapshot,core_metadata,package_branches}.R, tests/testthat/test-{get_branches,utils,init_metapip,install_latest_branch,pip_snapshot}.R, README, NEWS, charter, man/, vignettes
**Findings**: 7 confirmed open (P1: 3, P2: 3, P3: 1)

## Prior fix verification (passes)

- Unblock-the-core prior `fixed` findings (P0.1, P1.1–P1.5, P2.1–P2.6): no regression found in current code; suppressed per suppression policy (out of changed-file scope).
- get_branches() pagination, gh_token() env precedence, SHA pinning/idempotency, token redaction, and all committed tests (173 PASS, 0 FAIL) verified.

## P1 — CRITICAL (open, awaiting fix-triage)

- **[P1.1]** `R/pip_snapshot.R:20-26,59` (+`R/init_metapip.R:26-28`) — default lock path resolves to `""` (no `inst/`, no committed `inst/PIP_LOCK.csv`); `write.csv(lock_df, "")` writes the CSV to stdout silently while `cli_alert_success` claims success; lock-driven init/refresh never triggers. Fix: ship a committed `inst/PIP_LOCK.csv` or abort loudly on empty path. [manual]
- **[P1.2]** `R/pip_snapshot.R:57` + `R/init_metapip.R:177` — `rowbind(list())` crashes (`subscript out of bounds`) when all SHA resolutions fail. Fix: guard before row-binding; add all-skipped test. [manual]
- **[P1.3]** `R/init_metapip.R:67-81` — no-lock fallback ignores `ask`/`answer`, silently installing all core packages (milestone-2 gate regression). Fix: honor gate in fallback. [manual]

## P2 — IMPORTANT (open, awaiting fix-triage)

- **[P2.1]** `R/utils.R` — `check_github_token()` ignores `GITHUB_PAT`/`GITHUB_TOKEN` env vars (gitcreds-only), inconsistent with `gh_token()`, blocking env-var-only installs on fresh machines/CI. Fix: consult `gh_token()` first. [safe_auto]
- **[P2.2]** `test-init_metapip.R` — seven `update_pip_packages` tests non-hermetic once a committed lock ships (would overwrite source lock). Fix: set `metapip.lock_path` to tempfile. [safe_auto]
- **[P2.3]** `R/pip_snapshot.R:64-67` — `pip_lock_path()` roxygen missing title → no Rd generated. Fix: add title + regenerate. [safe_auto]

## P3 — MINOR (open)

- **[P3.1]** `R/install_pip_packages.R:146` — dead `return()` after `cli::cli_abort()`. Fix: delete. [safe_auto]

## ✅ Passed

- cg-code-quality + cg-testing: no new regressions beyond the branch's own confirmed-open findings; test suite green.

## Note

- `mode:verify` cycle NOT converged — findings remain open for this branch's review (`.cg-docs/reviews/2026-08-14-lock-down-supply-chain-review.md`). Next step: `/cg-fix-triage`.
