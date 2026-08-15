#' Snapshot the current PIP package SHAs into a lock manifest
#'
#' Resolves the current branch HEAD SHA of every core PIP package and writes a
#' `PIP_LOCK.csv` manifest (`package,branch,sha`) that teams can commit to make
#' installs deterministic. [init_metapip()] installs from this lock, and
#' [update_pip_packages()] refreshes it.
#'
#' @param path file path to write the lock to. Defaults to
#'   `getOption("metapip.lock_path")`, falling back to
#'   `system.file("PIP_LOCK.csv", package = "metapip")` (the source `inst/`
#'   directory in a `devtools::load_all()` session). Set it explicitly to
#'   control where the lock is written (e.g., a `tempfile()` in tests).
#'
#' @return invisible path to the written lock file
#' @export
#' @examples
#' \dontrun{
#' pip_snapshot()
#' }
pip_snapshot <- function(path = NULL) {
  if (is.null(path)) {
    path <- getOption(
      "metapip.lock_path",
      system.file("PIP_LOCK.csv", package = "metapip")
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
      package = pkg,
      branch = brn,
      sha = sha,
      stringsAsFactors = FALSE
    )
  }

  if (length(skipped) > 0) {
    cli::cli_alert_warning("Could not resolve SHA for {.pkg {skipped}}; skipping")
  }

  lock_df <- rowbind(Filter(Negate(is.null), rows))

  utils::write.csv(lock_df, path, row.names = FALSE)
  cli::cli_alert_success("Wrote PIP_LOCK to {.path {path}} ({nrow(lock_df)} packages)")
  invisible(path)
}

#' @return path to the committed `PIP_LOCK.csv`, or `""` when the file is
#'   absent (installed package without a lock). Read-only; callers should guard
#'   with `file.exists(p) && nzchar(p)` before reading.
#' @keywords internal
pip_lock_path <- function() {
  system.file("PIP_LOCK.csv", package = "metapip")
}
