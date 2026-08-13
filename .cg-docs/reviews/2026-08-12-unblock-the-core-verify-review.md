---
date: 2026-08-13
depth: light
parent-review: .cg-docs/reviews/2026-08-12-unblock-the-core-review.md
type: verification
findings:
  V0.1: fixed
  V1.1: fixed
  V2.1: fixed
  V2.2: fixed
  V2.3: fixed
  V2.4: fixed
  V2.5: fixed
  V3.1: fixed
  V3.2: fixed
  V3.3: fixed
---

# Verify Review: Resilient Networking & Time (http-timezone)

**Review mode**: verification (light depth, forced by mode:verify)
**Parent review**: `.cg-docs/reviews/2026-08-12-unblock-the-core-review.md`
**Files reviewed**: R/package_branches.R, R/get_branches.R, R/core_metadata.R, tests/testthat/test-{package_branches,get_branches,core_metadata,attach}.R, DESCRIPTION
**Findings**: 11 (P0: 1, P1: 1, P2: 5, P3: 3)

## Prior fix verification (passes)

- P0.1 package[.x] local lookup fix — correct, no regression.
- P1.2 get_current_branches.Rd regenerated correctly.
- P1.3/P1.5/P2.1–P2.6 — verified clean in current files.
- Suppression policy applied: no P2/P3 re-reported on already-fixed scope.
- Note: this verify scope is the http-timezone branch (milestone 4) changes; the parent review's open P2.7/P3.x items are out of the current changed-file scope.

## V0 — BLOCKING

### [V0.1] cg-code-quality R/package_branches.R:141-149 — NA compared-branch version silently reported as "up-to-date"
**Why**: `get_version_for_url()` returns NA on any failure (404/timeout/5xx indistinguishable). If PROD branch version fetch fails, `dev` has `branch="PROD", version=NA`; the `fcase` has no `is.na(version)` branch and `fselect(-branch,-version)` drops it, so the package shows "up-to-date" relative to an unknown PROD version. Silent wrong status (data integrity).
**Fix**: Add `is.na(version), paste(branch_to_compare, "version unknown")` to `fcase` before `default`; keep compared version until status computed; warn on NA.

## V1 — CRITICAL

### [V1.1] cg-code-quality R/package_branches.R:141-147 — join_and_get_status() errors on package_branches(); prior fcase fix ineffective
**Why**: (a) `fcase` is not exported by collapse (verified: absent from `getNamespaceExports("collapse")`); data.table not imported (NAMESPACE was never regenerated — `metapip-package.R:7` @rawNamespace import(data.table) is commented out). (b) `collapse::join(..., how="full")` yields no `local_status` column, so `fcase(local_status==1,...)` errors `object 'local_status' not found`. Package_branches() status path always crashes; latent because only test is skipped (`avoid live network`).
**Fix**: Regenerate NAMESPACE with `importFrom(data.table, fcase)` (or uncomment @rawNamespace), then derive a real status column before fcase (version comparison), or use `collapse::join(..., column="local_status")`.

## V2 — IMPORTANT

### [V2.1] cg-testing tests/testthat/test-get_branches.R:66-159 — 5 new get_latest_branch_update tests not hermetic
**Why**: `check_github_token()` → `gitcreds::gitcreds_get()` runs before the stubbed `get_branch_info`; on clean CI with no stored creds it aborts before the UTC/gh-pages logic under test runs. Passes locally only because this machine has credentials.
**Fix**: Add `mockery::stub(get_latest_branch_update, "check_github_token", function() invisible(list(username="x", password="x")))` to each new test (or factor a helper).

### [V2.2] cg-testing tests/testthat/test-core_metadata.R:8-30 — new core_metadata UTC test not hermetic
**Why**: `core_metadata()` calls `check_github_token()` before the stubbed get_branches/gh::gh/get_latest_branch_update; requires real gitcreds state; aborts on clean machine.
**Fix**: Add `mockery::stub(core_metadata, "check_github_token", function() invisible(list(username="x", password="x")))`.

### [V2.3] cg-testing tests/testthat/test-package_branches.R:1-8 — prior P0.1 fix unguarded by any passing assertion
**Why**: The skipped-live-network test is the only one covering the local-lookup/join path P0.1 fixed; a regression to `data.frame(package=.x)` would pass the suite silently.
**Fix**: Add a hermetic test of the local path: stub check_github_token + get_branches, neutralize get_version_for_url (with_mocked_responses), assert `out$local$package` are real package names.

### [V2.4] cg-version-control DESCRIPTION:35 — httr2 in Imports without minimum version
**Why**: Tests use `with_mocked_responses()` (httr2 >= 1.0.0); bare httr2 allows installing older versions where tests fail.
**Fix**: `Imports: httr2 (>= 1.0.0)`.

### [V2.5] cg-documentation R/get_branches.R:103-111 + man/get_latest_branch_update.Rd — new gh-pages-only behavior undocumented/not regenerated
**Why**: Exported function behavior changed (cli_warn + NA single-row return + UTC timestamp) but @description/@return not updated and man page not regenerated.
**Fix**: Document edge case in roxygen; run `devtools::document()` to regenerate.

## V3 — MINOR

### [V3.1] cg-code-quality R/package_branches.R:77-79 — resp_status != 200L check is dead code
**Why**: `req_perform()` raises httr2_http_* errors for 4xx/5xx by default, so all non-200 routes through the blanket error handler; the explicit status check never runs for the important cases.
**Fix**: Handle typed conditions (httr2_http_404 stays NA; 5xx/timeout warn) instead of blanket NA.

### [V3.2] cg-code-quality R/package_branches.R:80-86 — textConnection unnecessary; DCF warnings escape
**Why**: `read.dcf()` accepts `text=`; also read.dcf emits warnings (not errors) on malformed DCF that `error=` doesn't catch.
**Fix**: `mat <- suppressWarnings(read.dcf(text = httr2::resp_body_string(resp)))`; drop textConnection/on.exit.

### [V3.3] cg-testing tests/testthat/test-get_branches.R:86-105 — fractional-second clause not fully discriminating
**Why**: Both sides parsed with same platform parser; on Windows %OS rounds fraction symmetrically, so silently ignoring `.500` still passes (as long as tz=UTC retained).
**Fix**: Optional — assert `as.numeric(x) %% 1 == 0.5`, or adjust title; authenticate UTC regression guard is sound as-is.

## ✅ Passed
- cg-code-quality: UTC parse + tz attr preserved through empty-branch guard; test-attach.R stub correct; no regression in fixed prior findings.
- cg-testing: 88 tests pass, 0 fail, 5 skipped; timezone tests genuinely discriminate (removing tz=UTC shifts epoch by 18000s under TZ=America/New_York).
