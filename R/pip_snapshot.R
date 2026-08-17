#' Snapshot current PIP package SHAs into a lock manifest
#'
#' @description
#' Resolves the current branch HEAD SHA of every core PIP package and
#' writes a `PIP_LOCK.csv` manifest with columns `package`, `branch`,
#' `sha`. Teams can commit this file to version control to make installs
#' deterministic across all members.
#'
#' The lock is read by [init_metapip()] and refreshed by
#' [update_pip_packages()].
#'
#' @param path Character. File path to write the lock to. Defaults to
#'   `getOption("metapip.lock_path")`, falling back to
#'   `system.file("PIP_LOCK.csv", package = "metapip")` (the bundled
#'   `inst/` directory). Set this to a temporary path for testing or to a
#'   project-level path for team use.
#'
#' @return The path to the written lock file (invisibly).
#'
#' @section Tolerance:
#' Packages whose SHA cannot be resolved (e.g., network error, missing
#' branch) are skipped with a warning; the lock is still written for the
#' successfully resolved packages.
#'
#' @section Workflow:
#' 1. Run `pip_snapshot()` after confirming that all packages are at the
#'    desired versions.
#' 2. Commit the resulting `PIP_LOCK.csv`.
#' 3. Team members run `init_metapip()` to install at the recorded SHAs.
#'
#' @seealso
#' [init_metapip()], [update_pip_packages()], [pip_lock_path()]
#'
#' @examples
#' \dontrun{
#' # Write to the default location
#' pip_snapshot()
#'
#' # Write to a custom path
#' pip_snapshot(path = "path/to/my/PIP_LOCK.csv")
#'
#' # Write to a temp file (for testing)
#' pip_snapshot(path = tempfile(fileext = ".csv"))
#' }
#'
#' @export
pip_snapshot <- function(path = NULL) {
  if (is.null(path)) {
    path <- getOption(
      "metapip.lock_path",
      system.file("PIP_LOCK.csv", package = "metapip")
    )
  }

  if (!nzchar(path)) {
    cli::cli_abort(
      "Could not resolve a PIP_LOCK write target. Pass {.arg path}
      explicitly or set {.fn options}
      metapip.lock_path = \"path/to/PIP_LOCK.csv\"."
    )
  }

  pkgs <- get_core_pagkages()
  branches <- get_package_current_branch(package = pkgs)

  rows <- vector("list", length(pkgs))
  skipped <- character(0)
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    brn <- unname(branches[pkg])
    if (is.na(brn)) {
      skipped <- c(skipped, pkg)
      next
    }
    sha <- latest_commit_for_branch(pkg, brn)$sha
    if (is.null(sha)) {
      skipped <- c(skipped, pkg)
      next
    }
    rows[[i]] <- data.frame(
      package = pkg, branch = brn, sha = sha,
      stringsAsFactors = FALSE
    )
  }

  resolved <- Filter(Negate(is.null), rows)
  if (length(resolved) == 0) {
    cli::cli_alert_warning(
      "Could not resolve any SHA; PIP_LOCK not written"
    )
    return(invisible(path))
  }

  if (length(skipped) > 0) {
    cli::cli_alert_warning(
      "Could not resolve SHA for {.pkg {skipped}}; skipping"
    )
  }

  lock_df <- rowbind(resolved)
  utils::write.csv(lock_df, path, row.names = FALSE)
  cli::cli_alert_success(
    "Wrote PIP_LOCK to {.path {path}} ({nrow(lock_df)} packages)"
  )
  invisible(path)
}


#' Path to the committed PIP_LOCK.csv manifest
#'
#' @description
#' Returns the path to the bundled `PIP_LOCK.csv` file that ships with
#' the installed metapip package. Returns `""` when the file is absent
#' (e.g., package installed without a lock). Callers should guard with
#' `file.exists(p) && nzchar(p)` before reading.
#'
#' @return Character scalar. Path to `PIP_LOCK.csv`, or `""` if absent.
#'
#' @seealso
#' [pip_snapshot()], [init_metapip()]
#'
#' @keywords internal
pip_lock_path <- function() {
  system.file("PIP_LOCK.csv", package = "metapip")
}