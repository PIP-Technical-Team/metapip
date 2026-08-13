# 🧠 Project Brain — Part 1

_Generated 2026-08-13_

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
- **[Per-session API memoization for get_branches\(\) / latest_commit_for_branch\(\) \(P1\)](roadmap.json#api-memoization)** · `feature` · _idea_ · `—`
  > Per-session API memoization for get_branches() / latest_commit_for_branch() (P1)
- **[Replace installed.packages\(\) with requireNamespace\(\) per-package checks \(P2\)](roadmap.json#require-namespace-checks)** · `feature` · _idea_ · `—`
  > Replace installed.packages() with requireNamespace() per-package checks (P2)
- **[Un-skip and rewrite install tests with mockery stubs](roadmap.json#unskip-install-tests)** · `feature` · _idea_ · `—`
  > Un-skip and rewrite install tests with mockery stubs
- **[Mark network tests with skip_if_offline\(\) + skip_on_cran\(\)](roadmap.json#network-test-gating)** · `feature` · _idea_ · `—`
  > Mark network tests with skip_if_offline() + skip_on_cran()
- **[Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy](roadmap.json#windows-ci)** · `feature` · _idea_ · `—`
  > Windows CI matrix + pin actions/checkout@v4 + document GH_TOKEN policy
- **[Version bump to 0.1.0, NEWS.md rewrite, API hygiene \(typos, globalVariables, roxygen\)](roadmap.json#version-bump-010)** · `feature` · _idea_ · `—`
  > Version bump to 0.1.0, NEWS.md rewrite, API hygiene (typos, globalVariables, roxygen)

## Lock Down / Supply Chain Objective / Token

_Keywords: `lock down` · `supply chain
objective` · `token`_ · 7 entities

- **[SHA-pinned install_branch\(\) with force=TRUE override \(S1\)](roadmap.json#sha-pinned-install)** · `feature` · _idea_ · `—`
  > SHA-pinned install_branch() with force=TRUE override (S1)
- **[PIP_LOCK manifest format + pip_snapshot\(\) writer \(S1\)](roadmap.json#pip-lock-manifest)** · `feature` · _idea_ · `—`
  > PIP_LOCK manifest format + pip_snapshot() writer (S1)
- **[Lock-driven init_metapip\(\) and lock-updating update_pip_packages\(\) \(S1\)](roadmap.json#lock-driven-init)** · `feature` · _idea_ · `—`
  > Lock-driven init_metapip() and lock-updating update_pip_packages() (S1)
- **[install_latest_branch\(\) becomes developer-only with warning \(S1\)](roadmap.json#install-latest-dev-only)** · `feature` · _idea_ · `—`
  > install_latest_branch() becomes developer-only with warning (S1)
- **[Least-privilege token handling — read-only functions work without PAT, redacted print\(\) \(S2\)](roadmap.json#least-privilege-tokens)** · `feature` · _idea_ · `—`
  > Least-privilege token handling — read-only functions work without PAT, redacted print() (S2)
- **[Remove committed Codecov badge token from README \(S3\)](roadmap.json#remove-codecov-token)** · `feature` · _idea_ · `—`
  > Remove committed Codecov badge token from README (S3)
- **[Full gh\(\) pagination — per_page=100, .limit=100 \(S4\)](roadmap.json#full-gh-pagination)** · `feature` · _idea_ · `—`
  > Full gh() pagination — per_page=100, .limit=100 (S4)

## Install Pipeline Objective / Interactive / Failure

_Keywords: `install pipeline
objective` · `interactive` · `failure`_ · 6 entities

- **[Safe unloadNamespace\(\) with tryCatch + restart advisory \(R1\)](roadmap.json#safe-unload-namespace)** · `feature` · _idea_ · `—`
  > Safe unloadNamespace() with tryCatch + restart advisory (R1)
- **[Per-package failure isolation in update_pip_packages\(\) loop \(R2\)](roadmap.json#per-package-failure-isolation)** · `feature` · _idea_ · `—`
  > Per-package failure isolation in update_pip_packages() loop (R2)
- **[interactive\(\) gate on utils::menu\(\) \(R3\)](roadmap.json#interactive-gate)** · `feature` · _idea_ · `—`
  > interactive() gate on utils::menu() (R3)
- **[Tri-state compare_sha\(\) — treat CRAN-installed as unknown not missing \(R6\)](roadmap.json#tri-state-compare-sha)** · `feature` · _idea_ · `—`
  > Tri-state compare_sha() — treat CRAN-installed as unknown not missing (R6)
- **[SHA short-circuit in install_latest_branch\(\) \(P3\)](roadmap.json#sha-short-circuit)** · `feature` · _idea_ · `—`
  > SHA short-circuit in install_latest_branch() (P3)
- **[Document CRAN dependency gap, recommend renv companion \(S5 deferred\)](roadmap.json#document-cran-gap)** · `feature` · _idea_ · `—`
  > Document CRAN dependency gap, recommend renv companion (S5 deferred)

## Resilient Networking / Time Objective / Get_Latest_Branch_Update

_Keywords: `resilient networking` · `time
objective` · `get_latest_branch_update`_ · 3 entities

- **[Replace read.dcf\(url\(\)\) with httr2 — timeouts, error handling, connection safety \(R4\)](roadmap.json#httr2-fetch)** · `feature` · _done_ · `—`
  > Replace read.dcf(url()) with httr2 — timeouts, error handling, connection safety (R4)
- **[UTC-correct timestamp parsing in get_latest_branch_update\(\) and core_metadata\(\) \(R5\)](roadmap.json#utc-timestamps)** · `feature` · _done_ · `—`
  > UTC-correct timestamp parsing in get_latest_branch_update() and core_metadata() (R5)
- **[Guard ss\(1L\) on empty data in get_latest_branch_update\(\) \(R6 edge\)](roadmap.json#guard-empty-ss)** · `feature` · _done_ · `—`
  > Guard ss(1L) on empty data in get_latest_branch_update() (R6 edge)
