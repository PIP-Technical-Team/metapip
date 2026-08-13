# 🧠 Project Brain — Chronological Log

_Generated 2026-08-13 · 8 artifacts (newest first) + 28 roadmap features_

## 2026-08-13

- **[2026-08-12-resilient-networking-time-review](.cg-docs/reviews/2026-08-12-resilient-networking-time-review.md)** · `review` · _—_ · `2026-08-13`
  > **Review mode**: standard (auto-routed by `/cg-work review:auto`) **Files reviewed**: R/package_branches.R, R/get_bra…
- **[2026-08-12-unblock-the-core-verify-review](.cg-docs/reviews/2026-08-12-unblock-the-core-verify-review.md)** · `review` · _—_ · `2026-08-13`
  > **Review mode**: verification (light depth, forced by mode:verify) **Parent review**: `.cg-docs/reviews/2026-08-12-un…
- **[Resilient httr2 fetching, explicit-UTC timestamps, and safe subsetting in R](.cg-docs/solutions/data-quality/2026-08-13-resilient-httr2-utc-fetching.md)** · `solution` · _—_ · `2026-08-13`
  > Three fragility classes in an R package's remote-data layer: 1. `read.dcf(url(y))` had no timeout, no HTTP error hand…

## 2026-08-12

- **[2026-08-12-unblock-the-core-review](.cg-docs/reviews/2026-08-12-unblock-the-core-review.md)** · `review` · _—_ · `2026-08-12`
  > **Review mode**: standard **Files reviewed**: 15 **Findings**: 17 (P0: 1, P1: 5, P2: 7, P3: 4)
- **[Rebuilding data frames from named lists in R without losing branch names](.cg-docs/solutions/data-quality/2026-08-12-rebuild-named-list-branches.md)** · `solution` · _—_ · `2026-08-12`
  > `get_complete_data()` used `utils::stack()` to rebuild a data frame from a named list where inner vectors carried bra…
- **[Remediation Roadmap — Engineering Review Findings](.cg-docs/strategy/2026-08-12-remediation-roadmap.md)** · `strategy` · _—_ · `2026-08-12`
  > - **Project**: metapip (PIP R package manager) - **Team**: DECDG / GPID — World Bank - **Language**: R | **Type**: Pa…
- **[Resilient Networking & Time — httr2 fetch, UTC timestamps, empty-branch guard](.cg-docs/plans/2026-08-12-resilient-networking-time.md)** · `plan` · _completed_ · `2026-08-12`
  > Replace fragile `read.dcf(url())` HTTP fetching with httr2 (timeouts, error handling, connection safety), fix UTC tim…
- **[Unblock the Core — fix V1–V4 and R7 with a red-phase regression harness](.cg-docs/plans/2026-08-12-unblock-the-core.md)** · `plan` · _completed_ · `2026-08-12`
  > Make the three headline metapip workflows functional again by fixing the four critical bugs (V1–V4) and the medium ro…

## Roadmap Features

- **[Per-session API memoization for get_branches\(\) / latest_commit_for_branch\(\) \(P1\)](roadmap.json#api-memoization)** · `feature` · _idea_ · `—`
  > Per-session API memoization for get_branches() / latest_commit_for_branch() (P1)
- **[Declare undeclared dependencies — fs → basename\(\), stringr → Imports or base R \(V4\)](roadmap.json#declare-undeclared-deps)** · `feature` · _idea_ · `—`
  > Declare undeclared dependencies — fs → basename(), stringr → Imports or base R (V4)
- **[Document CRAN dependency gap, recommend renv companion \(S5 deferred\)](roadmap.json#document-cran-gap)** · `feature` · _idea_ · `—`
  > Document CRAN dependency gap, recommend renv companion (S5 deferred)
- **[Fix get_custom_branch\(\) undefined variable \(V3\)](roadmap.json#fix-get-custom-branch)** · `feature` · _idea_ · `—`
  > Fix get_custom_branch() undefined variable (V3)
- **[Fix install_pip_packages\(\) \(V1: undefined pkg, unnamed branch → NA, error messages\)](roadmap.json#fix-install-pip-packages)** · `feature` · _idea_ · `—`
  > Fix install_pip_packages() (V1: undefined pkg, unnamed branch → NA, error messages)
- **[Fix .onAttach\(\) stub and init_metapip\(\) hardcoded answer \(R7\)](roadmap.json#fix-onattach-and-answer)** · `feature` · _idea_ · `—`
  > Fix .onAttach() stub and init_metapip() hardcoded answer (R7)
- **[Fix package_branches\(\) regex \(V2: invalid char range, wrong suffix\)](roadmap.json#fix-package-branches-regex)** · `feature` · _idea_ · `—`
  > Fix package_branches() regex (V2: invalid char range, wrong suffix)
- **[Full gh\(\) pagination — per_page=100, .limit=100 \(S4\)](roadmap.json#full-gh-pagination)** · `feature` · _idea_ · `—`
  > Full gh() pagination — per_page=100, .limit=100 (S4)
- **[Guard ss\(1L\) on empty data in get_latest_branch_update\(\) \(R6 edge\)](roadmap.json#guard-empty-ss)** · `feature` · _done_ · `—`
  > Guard ss(1L) on empty data in get_latest_branch_update() (R6 edge)
- **[Replace read.dcf\(url\(\)\) with httr2 — timeouts, error handling, connection safety \(R4\)](roadmap.json#httr2-fetch)** · `feature` · _done_ · `—`
  > Replace read.dcf(url()) with httr2 — timeouts, error handling, connection safety (R4)
- **[install_latest_branch\(\) becomes developer-only with warning \(S1\)](roadmap.json#install-latest-dev-only)** · `feature` · _idea_ · `—`
  > install_latest_branch() becomes developer-only with warning (S1)
- **[interactive\(\) gate on utils::menu\(\) \(R3\)](roadmap.json#interactive-gate)** · `feature` · _idea_ · `—`
  > interactive() gate on utils::menu() (R3)
- **[Least-privilege token handling — read-only functions work without PAT, redacted print\(\) \(S2\)](roadmap.json#least-privilege-tokens)** · `feature` · _idea_ · `—`
  > Least-privilege token handling — read-only functions work without PAT, redacted print() (S2)
- **[Lock-driven init_metapip\(\) and lock-updating update_pip_packages\(\) \(S1\)](roadmap.json#lock-driven-init)** · `feature` · _idea_ · `—`
  > Lock-driven init_metapip() and lock-updating update_pip_packages() (S1)
- **[Mark network tests with skip_if_offline\(\) + skip_on_cran\(\)](roadmap.json#network-test-gating)** · `feature` · _idea_ · `—`
  > Mark network tests with skip_if_offline() + skip_on_cran()
- **[Per-package failure isolation in update_pip_packages\(\) loop \(R2\)](roadmap.json#per-package-failure-isolation)** · `feature` · _idea_ · `—`
  > Per-package failure isolation in update_pip_packages() loop (R2)
- **[PIP_LOCK manifest format + pip_snapshot\(\) writer \(S1\)](roadmap.json#pip-lock-manifest)** · `feature` · _idea_ · `—`
  > PIP_LOCK manifest format + pip_snapshot() writer (S1)
- **[Remove committed Codecov badge token from README \(S3\)](roadmap.json#remove-codecov-token)** · `feature` · _idea_ · `—`
  > Remove committed Codecov badge token from README (S3)
- **[Replace installed.packages\(\) with requireNamespace\(\) per-package checks \(P2\)](roadmap.json#require-namespace-checks)** · `feature` · _idea_ · `—`
  > Replace installed.packages() with requireNamespace() per-package checks (P2)
- **[Safe unloadNamespace\(\) with tryCatch + restart advisory \(R1\)](roadmap.json#safe-unload-namespace)** · `feature` · _idea_ · `—`
  > Safe unloadNamespace() with tryCatch + restart advisory (R1)
- **[SHA-pinned install_branch\(\) with force=TRUE override \(S1\)](roadmap.json#sha-pinned-install)** · `feature` · _idea_ · `—`
  > SHA-pinned install_branch() with force=TRUE override (S1)
- **[SHA short-circuit in install_latest_branch\(\) \(P3\)](roadmap.json#sha-short-circuit)** · `feature` · _idea_ · `—`
  > SHA short-circuit in install_latest_branch() (P3)
- **[Tri-state compare_sha\(\) — treat CRAN-installed as unknown not missing \(R6\)](roadmap.json#tri-state-compare-sha)** · `feature` · _idea_ · `—`
  > Tri-state compare_sha() — treat CRAN-installed as unknown not missing (R6)
- **[Un-skip and rewrite install tests with mockery stubs](roadmap.json#unskip-install-tests)** · `feature` · _idea_ · `—`
  > Un-skip and rewrite install tests with mockery stubs
- **[UTC-correct timestamp parsing in get_latest_branch_update\(\) and core_metadata\(\) \(R5\)](roadmap.json#utc-timestamps)** · `feature` · _done_ · `—`
  > UTC-correct timestamp parsing in get_latest_branch_update() and core_metadata() (R5)
- **[Version bump to 0.1.0, NEWS.md rewrite, API hygiene \(typos, globalVariables, roxygen\)](roadmap.json#version-bump-010)** · `feature` · _idea_ · `—`
  > Version bump to 0.1.0, NEWS.md rewrite, API hygiene (typos, globalVariables, roxygen)
- **[Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy](roadmap.json#windows-ci)** · `feature` · _idea_ · `—`
  > Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy
- **[Write red-phase regression tests for V1–V4](roadmap.json#write-red-phase-tests)** · `feature` · _idea_ · `—`
  > Write red-phase regression tests for V1–V4
