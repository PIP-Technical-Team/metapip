# 🧠 Project Brain — Part 1

_Generated 2026-08-17_

## Core Objective / Release Objective / Fix

_Keywords: `core
objective` · `release
objective` · `fix`_ · 12 entities

- **[Fix install_pip_packages\(\) \(V1: undefined pkg, unnamed branch → NA, error messages\)](roadmap.json#fix-install-pip-packages)** · `feature` · _idea_ · `—`
  > Fix install_pip_packages() (V1: undefined pkg, unnamed branch → NA, error messages)
- **[Fix package_branches\(\) regex \(V2: invalid char range, wrong suffix\)](roadmap.json#fix-package-branches-regex)** · `feature` · _idea_ · `—`
  > Fix package_branches() regex (V2: invalid char range, wrong suffix)
- **[Fix get_custom_branch\(\) undefined variable \(V3\)](roadmap.json#fix-get-custom-branch)** · `feature` · _idea_ · `—`
  > Fix get_custom_branch() undefined variable (V3)
- **[Declare undeclared dependencies — fs → basename\(\), stringr → Imports or base R \(V4\)](roadmap.json#declare-undeclared-deps)** · `feature` · _idea_ · `—`
  > Declare undeclared dependencies — fs → basename(), stringr → Imports or base R (V4)
- **[Fix .onAttach\(\) stub and init_metapip\(\) hardcoded answer \(R7\)](roadmap.json#fix-onattach-and-answer)** · `feature` · _idea_ · `—`
  > Fix .onAttach() stub and init_metapip() hardcoded answer (R7)
- **[Write red-phase regression tests for V1–V4](roadmap.json#write-red-phase-tests)** · `feature` · _idea_ · `—`
  > Write red-phase regression tests for V1–V4
- **[Per-session API memoization for get_branches\(\) / latest_commit_for_branch\(\) \(P1\)](roadmap.json#api-memoization)** · `feature` · _done_ · `—`
  > Per-session API memoization for get_branches() / latest_commit_for_branch() (P1)
- **[Replace installed.packages\(\) with requireNamespace\(\) per-package checks \(P2\)](roadmap.json#require-namespace-checks)** · `feature` · _done_ · `—`
  > Replace installed.packages() with requireNamespace() per-package checks (P2)
- **[Un-skip and rewrite install tests with mockery stubs](roadmap.json#unskip-install-tests)** · `feature` · _done_ · `—`
  > Un-skip and rewrite install tests with mockery stubs
- **[Mark network tests with skip_if_offline\(\) + skip_on_cran\(\)](roadmap.json#network-test-gating)** · `feature` · _done_ · `—`
  > Mark network tests with skip_if_offline() + skip_on_cran()
- **[Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy](roadmap.json#windows-ci)** · `feature` · _done_ · `—`
  > Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy
- **[Version bump to 0.1.0, NEWS.md rewrite, API hygiene \(typos, globalVariables, roxygen\)](roadmap.json#version-bump-010)** · `feature` · _done_ · `—`
  > Version bump to 0.1.0, NEWS.md rewrite, API hygiene (typos, globalVariables, roxygen)

## Lock Down / Supply Chain Objective / Token

_Keywords: `lock down` · `supply chain
objective` · `token`_ · 7 entities

- **[SHA-pinned install_branch\(\) with force=TRUE override \(S1\)](roadmap.json#sha-pinned-install)** · `feature` · _done_ · `—`
  > SHA-pinned install_branch() with force=TRUE override (S1)
- **[PIP_LOCK manifest format + pip_snapshot\(\) writer \(S1\)](roadmap.json#pip-lock-manifest)** · `feature` · _done_ · `—`
  > PIP_LOCK manifest format + pip_snapshot() writer (S1)
- **[Lock-driven init_metapip\(\) and lock-updating update_pip_packages\(\) \(S1\)](roadmap.json#lock-driven-init)** · `feature` · _done_ · `—`
  > Lock-driven init_metapip() and lock-updating update_pip_packages() (S1)
- **[install_latest_branch\(\) becomes developer-only with warning \(S1\)](roadmap.json#install-latest-dev-only)** · `feature` · _done_ · `—`
  > install_latest_branch() becomes developer-only with warning (S1)
- **[Least-privilege token handling — read-only functions work without PAT, redacted print\(\) \(S2\)](roadmap.json#least-privilege-tokens)** · `feature` · _done_ · `—`
  > Least-privilege token handling — read-only functions work without PAT, redacted print() (S2)
- **[Remove committed Codecov badge token from README \(S3\)](roadmap.json#remove-codecov-token)** · `feature` · _done_ · `—`
  > Remove committed Codecov badge token from README (S3)
- **[Full gh\(\) pagination — per_page=100, .limit=100 \(S4\)](roadmap.json#full-gh-pagination)** · `feature` · _done_ · `—`
  > Full gh() pagination — per_page=100, .limit=100 (S4)

## Install Pipeline Objective / Interactive / Failure

_Keywords: `install pipeline
objective` · `interactive` · `failure`_ · 6 entities

- **[Safe unloadNamespace\(\) with tryCatch + restart advisory \(R1\)](roadmap.json#safe-unload-namespace)** · `feature` · _done_ · `—`
  > Safe unloadNamespace() with tryCatch + restart advisory (R1)
- **[Per-package failure isolation in update_pip_packages\(\) loop \(R2\)](roadmap.json#per-package-failure-isolation)** · `feature` · _done_ · `—`
  > Per-package failure isolation in update_pip_packages() loop (R2)
- **[interactive\(\) gate on utils::menu\(\) \(R3\)](roadmap.json#interactive-gate)** · `feature` · _done_ · `—`
  > interactive() gate on utils::menu() (R3)
- **[Tri-state compare_sha\(\) — treat CRAN-installed as unknown not missing \(R6\)](roadmap.json#tri-state-compare-sha)** · `feature` · _done_ · `—`
  > Tri-state compare_sha() — treat CRAN-installed as unknown not missing (R6)
- **[SHA short-circuit in install_latest_branch\(\) \(P3\)](roadmap.json#sha-short-circuit)** · `feature` · _done_ · `—`
  > SHA short-circuit in install_latest_branch() (P3)
- **[Document CRAN dependency gap, recommend renv companion \(S5 deferred\)](roadmap.json#document-cran-gap)** · `feature` · _done_ · `—`
  > Document CRAN dependency gap, recommend renv companion (S5 deferred)

## Fcase / Mapply / Local_Status

_Keywords: `fcase` · `mapply` · `local_status`_ · 4 entities

- **[collapse does not export fcase/fifelse and join\(\) never produces local_status](.cg-docs/solutions/bugs/2026-08-14-collapse-fcase-fifelse-not-exported.md)** · `solution` · _—_ · `2026-08-14`
  > `package_branches()` crashed at runtime with `could not find function "fcase"`. A rewrite to `fifelse()` failed ident…
- **[Resilient httr2 fetching, explicit-UTC timestamps, and safe subsetting in R](.cg-docs/solutions/data-quality/2026-08-13-resilient-httr2-utc-fetching.md)** · `solution` · _—_ · `2026-08-13`
  > Three fragility classes in an R package's remote-data layer: 1. `read.dcf(url(y))` had no timeout, no HTTP error hand…
- **[Parse untrusted remote version strings safely with compareVersion + tryCatch](.cg-docs/solutions/data-quality/2026-08-14-untrusted-version-parse-compareversion.md)** · `solution` · _—_ · `2026-08-14`
  > `package_branches()` reads `Version:` fields straight from `raw.githubusercontent.com/.../DESCRIPTION` files via `rea…
- **[Tri-state compare_sha and mockery stubbing with mapply](.cg-docs/solutions/testing-patterns/2026-08-13-tri-state-compare-sha-mockery.md)** · `solution` · _—_ · `2026-08-13`
  > When testing `update_pip_packages()`, which uses `mapply(compare_sha, pkgs, default_branch[pkgs])`, mocking `compare_…

## Init_Metapip.R / Fix / Test-Init_Metapip.R

_Keywords: `init_metapip.r` · `fix` · `test-init_metapip.r`_ · 4 entities

- **[2026-08-12-unblock-the-core-review](.cg-docs/reviews/2026-08-12-unblock-the-core-review.md)** · `review` · _—_ · `2026-08-12`
  > **Review mode**: standard **Files reviewed**: 15 **Findings**: 17 (P0: 1, P1: 5, P2: 7, P3: 4)
- **[2026-08-12-unblock-the-core-verify-review-2](.cg-docs/reviews/2026-08-12-unblock-the-core-verify-review-2.md)** · `review` · _—_ · `2026-08-14`
  > **Review mode**: light (verify pass) **Files reviewed**: 18 (milestone changes + tests) **Findings**: 16 (P0: 0, P1: …
- **[2026-08-13-harden-install-pipeline-verify-review](.cg-docs/reviews/2026-08-13-harden-install-pipeline-verify-review.md)** · `review` · _—_ · `2026-08-13`
  > **Review mode**: light (verify pass) **Files reviewed**: 10 **Findings**: 11 (P0: 0, P1: 0, P2: 6, P3: 5)
- **[2026-08-14-lock-down-supply-chain-review](.cg-docs/reviews/2026-08-14-lock-down-supply-chain-review.md)** · `review` · _—_ · `2026-08-14`
  > **Review mode**: full (auto-routed from /cg-work review:auto — security-risk: tokens/credentials/install paths) **Fil…

## Resilient Networking / Time Objective / Get_Latest_Branch_Update

_Keywords: `resilient networking` · `time
objective` · `get_latest_branch_update`_ · 3 entities

- **[Replace read.dcf\(url\(\)\) with httr2 — timeouts, error handling, connection safety \(R4\)](roadmap.json#httr2-fetch)** · `feature` · _done_ · `—`
  > Replace read.dcf(url()) with httr2 — timeouts, error handling, connection safety (R4)
- **[UTC-correct timestamp parsing in get_latest_branch_update\(\) and core_metadata\(\) \(R5\)](roadmap.json#utc-timestamps)** · `feature` · _done_ · `—`
  > UTC-correct timestamp parsing in get_latest_branch_update() and core_metadata() (R5)
- **[Guard ss\(1L\) on empty data in get_latest_branch_update\(\) \(R6 edge\)](roadmap.json#guard-empty-ss)** · `feature` · _done_ · `—`
  > Guard ss(1L) on empty data in get_latest_branch_update() (R6 edge)

## Update_Pip_Packages\(\) / Compare_Sha / Install_Branch

_Keywords: `update_pip_packages()` · `compare_sha` · `install_branch`_ · 3 entities

- **[Harden the Install Pipeline — safe unload, failure isolation, interactive gate, tri-state SHA](.cg-docs/plans/2026-08-13-harden-install-pipeline.md)** · `plan` · _completed_ · `2026-08-13`
  > Make the metapip install/update flow robust across six verified issues from the engineering review (`inst/TMP/metapip…
- **[Lock Down the Supply Chain — SHA-pinned installs, PIP_LOCK manifest, least-privilege tokens, full pagination](.cg-docs/plans/2026-08-13-lock-down-supply-chain.md)** · `plan` · _active_ · `2026-08-13`
  > Close the supply-chain security findings from the engineering review (`inst/TMP/metapip-review.html`) across four ver…
- **[Lock Down the Supply Chain — SHA-pinned installs, PIP_LOCK manifest, least-privilege tokens, full pagination](.cg-docs/plans/2026-08-14-lock-down-supply-chain.md)** · `plan` · _completed_ · `2026-08-14`
  > Close the supply-chain security findings from the engineering review (`inst/TMP/metapip-review.html`) across four ver…
