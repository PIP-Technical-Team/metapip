---
date: "2026-08-12"
title: "Remediation Roadmap — Engineering Review Findings"
trigger: "new-project"
outcome: "roadmap-updated"
---

# Strategy Session: Remediation Roadmap — Engineering Review Findings

## Context at Session Start

- **Project**: metapip (PIP R package manager)
- **Team**: DECDG / GPID — World Bank
- **Language**: R | **Type**: Package | **Review**: Standard
- **Charter Objective**: Meta R package for managing all PIP R packages via GitHub API
- **Current Focus (before session)**: "I am working on optimizing the package." (placeholder)
- **Roadmap (before session)**: 1 placeholder milestone ("I am working on optimizing the package.") with 1 generic feature — no actionable scope.
- **Starting material**: Engineering review completed same day (`inst/TMP/metapip-review.html`) — 25 verified findings (4 critical, 6 high, 11 medium, 4 low), 5 root causes, 6-phase remediation plan, full Compound GPID command workflow.

## Discussion Summary

### Trigger
User selected "Starting fresh on a real roadmap" — the existing roadmap was a placeholder and the engineering review provides all the structure needed.

### Finding Batching
All 25 findings were grouped into 5 implementation batches to minimize cross-dependencies and maximize execution efficiency:

1. **Batch 1: "Unblock the Core"** — V1, V2, V3, V4, R7 (critical bug fixes + regression tests). No design discussion needed — all fixes are deterministic.
2. **Batch 2: "Harden the Install Pipeline"** — R1, R2, R3, R6, S5, P3 (robustness). S5 (dependency resolution) deferred with documentation.
3. **Batch 3: "Lock Down the Supply Chain"** — S1, S2, S3, S4 (security). S1 required design alignment.
4. **Batch 4: "Resilient Networking & Time"** — R4, R5 (httr2 + timezone). No design discussion needed.
5. **Batch 5: "Performance, Tests & Release"** — P1, P2, test gaps, CI, release. No design discussion needed.

### Design Decisions

Three decisions were needed before planning:

| Decision | Question | Resolution |
|----------|----------|------------|
| D1 | S5: Full renv lockfile integration now, or defer? | **Defer** — document the gap, recommend `renv` as companion tool. |
| D2 | S1: SHA pinning contract — always pin, or keep live HEAD mode? | **Pin by default + `force = TRUE` override + team lock manifest (`PIP_LOCK`).** Rationale: packages move fast with multiple contributors; the lock manifest (committed to git) is the team's agreement on "the right version." `init_metapip()` reads the lock; `update_pip_packages()` updates it; `install_latest_branch()` becomes a developer-only tool with warning; new `pip_snapshot()` writes the lock. |
| D3 | R4: Use `httr2` (robust) or base `url()` (no dep)? | **`httr2`** — already a transitive dependency of `gh`, low-cost addition to Imports. |

### D2 Deep Dive — Lock Manifest

The team dynamics (fast-moving, multiple contributors) drove a richer solution than simple SHA pinning:

- **`PIP_LOCK`** (CSV: package, branch, sha) — committed to repo, reviewed in PRs
- **`init_metapip()`** — reads lock, installs recorded SHAs (first-time: falls back to HEAD)
- **`update_pip_packages()`** — resolves HEAD, updates lock, prompts install
- **`install_branch()`** — SHA-pinned by default; `force = TRUE` bypasses lock
- **`install_latest_branch()`** — developer-only, warns about bypassing lock
- **`pip_snapshot()`** — new helper, resolves all branches to SHAs, writes lock

## Proposed Changes

### Milestones Created

| # | Milestone | Features | Objective |
|---|-----------|----------|-----------|
| 1 | Unblock the Core | 6 | Three headline functions are broken; fix them and establish a regression harness. |
| 2 | Harden the Install Pipeline | 6 | Make the install/update flow robust: safe namespace handling, failure isolation, interactive gating. |
| 3 | Lock Down the Supply Chain | 7 | SHA-pinned installs, team lock manifest, least-privilege tokens, full pagination. |
| 4 | Resilient Networking & Time | 3 | Robust HTTP fetch with httr2, correct UTC timestamp parsing. |
| 5 | Performance, Tests & Release | 6 | Cache API calls, un-skip tests, harden CI, ship 0.1.0. |

**Total: 5 milestones, 28 features**, all at `idea` status with `plan: null`.

### Roadmap Changes

- **RETIRED**: "I am working on optimizing the package." — placeholder milestone with no actionable scope.
- **ADDED**: 5 new milestones with 28 features derived from the engineering review findings.

## Decision

Approved as proposed. All three design decisions resolved. Roadmap updated via `@cg-roadmap`.

## Charter Updates

- **Current Focus**: Updated from "I am working on optimizing the package." to the 5-milestone remediation summary with pointer to start at "Unblock the Core" via `/cg-plan`.
- **last-reviewed**: Updated to `2026-08-12T16:44:00-04:00`.
- **Archived**: Previous Current Focus to `.cg-docs/archive/charter-history.md`.
