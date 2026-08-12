---
date: 2026-08-12
depth: standard
type: standard
plan: .cg-docs/plans/2026-08-12-unblock-the-core.md
findings:
  P0.1: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P2.7: open
  P3.1: open
  P3.2: open
  P3.3: open
  P3.4: open
  P3.5: open
---

## Review Report

**Review mode**: standard
**Files reviewed**: 15
**Findings**: 17 (P0: 1, P1: 5, P2: 7, P3: 4)

### P0 — BLOCKING (immediate remediation required)

- **[P0.1]** [cg-code-quality] `R/package_branches.R:53-57` — `cli_progress_along()` returns numeric indices, but `.x` is passed directly to `utils::packageDescription(.x)` and used as `data.frame(package = .x)` instead of `package[.x]`. This causes every local package lookup to fail inside `tryCatch` (package names are numbers), producing incorrect "Not in local" status and wrong join results for all packages.
  **Why**: Silent data corruption: the local status table is built with numeric package identifiers instead of actual package names.
  **Fix**: Replace `.x` with `package[.x]` on lines 53 and 54:
  ```r
  utils::packageDescription(package[.x], fields = c("GithubRef", "Version")),
  ...
  data.frame(package = package[.x], ...)
  ```

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality] `R/package_branches.R:121-125` — `ifelse()` is used in `fmutate()` instead of `fifelse()`/`fcase()`. The project dialect (`data.table-collapse`) explicitly flags `ifelse()` usage.
  **Why**: Style/convention violation; `fcase()` is more idiomatic and performant in collapse.
  **Fix**: Rewrite using `fcase()`.

- **[P1.2]** [cg-code-quality] `R/get_branches.R:148-152` — Malformed roxygen2 `@param` block. The default value `TRUE` for `@param verbose` is orphaned on its own line after `@param package`.
  **Why**: Documentation rendering issue; the parameter default will not display correctly.
  **Fix**: Attach `TRUE` to the `@param verbose` line: `#' @param verbose logical: whether to display all current branches. Default is TRUE`

- **[P1.3]** [cg-code-quality] `R/get_branches.R:254` — Grammatically incorrect `cli_abort` message: `"package{?s} available {?is/are}"` reads backwards.
  **Why**: Poor user-facing error message.
  **Fix**: Change to `"package{?s} {?is/are} available"` (or add colon).

- **[P1.4]** [cg-code-quality] `R/install_pip_packages.R:18-19` — Missing braces around single-statement `if/else` bodies, inconsistent with the rest of the file.
  **Why**: Style consistency.
  **Fix**: Add braces around the bodies.

- **[P1.5]** [cg-testing] `test-init_metapip.R:2` — `compare_sha` regression test is entirely skipped (`skip()` on line 2), so zero assertions execute in CI.
  **Why**: No test coverage for a known bug-prone function.
  **Fix**: Remove `skip()` and stub `utils::packageDescription` so the test does not require `pipfun` to actually be installed.

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `test-undeclared-deps.R:3` — Inert `fs::path_file` stub. Both tests stub `fs::path_file` inside `get_core_pagkages`, but the source uses `basename()` from base R, not `fs`. The dead stub provides false confidence.
  **Fix**: Remove the `fs::path_file` stubs.

- **[P2.2]** [cg-testing] `test-undeclared-deps.R` — File name mismatch: the file is named `test-undeclared-deps.R` but contains only tests for `get_core_pagkages`.
  **Fix**: Rename to `test-get_core_pagkages.R`.

- **[P2.3]** [cg-testing] `test-get_custom_branch.R` — No boundary test for empty/missing option. Existing tests rely on `setup.R` pre-populating the option.
  **Fix**: Add test for `options(metapip.custom_branch = list())`.

- **[P2.4]** [cg-testing] `test-install_pip_packages.R:36-46` — Weak error assertion: only checks message contains package name, does not verify return value or that all packages were attempted.
  **Fix**: Assert return is `invisible(NULL)` and count error stub invocations.

- **[P2.5]** [cg-testing] `test-init_metapip.R:30-39` — Incomplete forwarding test: does not verify that `exclude` and `ask` are forwarded, nor that `metapip_attach` is called.
  **Fix**: Capture all arguments and `metapip_attach` calls.

- **[P2.6]** [cg-code-quality] `R/package_branches.R:79,80` — Assignment with `=` instead of `<-` in function body (`br = get_branches(...)`).
  **Fix**: Use `<-`.

- **[P2.7]** [cg-code-quality] Typo `get_core_pagkages` in multiple files (`R/get_branches.R:146`, `R/init_metapip.R:53,144,145`, `vignettes/package_development.Rmd:68`).
  **Fix**: Rename function to `get_core_packages` and update all references.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/package_branches.R:42,46,51` — Unnecessary inline comments (`# end of expr section`, etc.).
  **Fix**: Remove comments.

- **[P3.2]** [cg-code-quality] `vignettes/package_development.Rmd:18` — Commented-out code `# devtools::load_all(".")` left in setup chunk.
  **Fix**: Remove commented line.

- **[P3.3]** [cg-code-quality] `R/install_pip_packages.R:104` — Redundant `glue::glue()` wrapper inside `cli::cli_alert_info()`.
  **Fix**: Use `cli::cli_alert_info("Installing branch {branch} from package {package}")`.

- **[P3.4]** [cg-code-quality] `R/get_branches.R:28` — `cli::cat_bullet(glue::glue("{branches}"))` collapses vector into single string.
  **Fix**: Use `cli::cat_bullet("{branches}")`.

- **[P3.5]** [cg-code-quality] `R/zzz.R:32` — Missing space before `[` index operator.
  **Fix**: `options(metapip_default_options[toset])`.
