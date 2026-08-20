#' Initialise and update PIP core packages
#'
#' @description
#' `init_metapip()` is the primary entry point for setting up the PIP
#' package ecosystem. It checks that all core packages are installed at
#' the correct versions and attaches them.
#'
#' When a committed `PIP_LOCK.csv` manifest is found (via
#' [pip_lock_path()]), installs every package at the SHA recorded in the
#' lock for deterministic, team-consistent results. When the lock is
#' absent, falls back to installing each package at its branch HEAD SHA
#' and suggests running [pip_snapshot()] to create a team lock.
#'
#' @param exclude Character vector. Packages to exclude from
#'   installation and attachment. When `NA` (default), the function
#'   checks if the current working directory is a core PIP package and
#'   excludes it automatically (useful during package development).
#'   Pass `NULL` to exclude nothing. Pass a character vector of package
#'   names to exclude explicitly.
#' @param ask Logical. If `TRUE` (default), prompts interactively before
#'   installing. In non-interactive sessions, always installs with a
#'   warning.
#' @param answer Numeric. Developer argument for demonstration purposes.
#'   `1` = Yes, `2` = No.
#'
#' @return `invisible()`. Called for its side effects: installing
#'   packages and attaching them.
#'
#' @details
#' **Lock-driven workflow:**
#' 1. Reads `PIP_LOCK.csv` via [pip_lock_path()].
#' 2. Filters to packages that are not excluded.
#' 3. Installs each at its recorded SHA.
#'
#' **Fallback workflow (no lock):**
#' 1. Resolves each package's configured branch.
#' 2. Installs at branch HEAD SHA.
#' 3. Suggests running [pip_snapshot()] to create a lock.
#'
#' After installation, attaches all available core packages via
#' [metapip_attach()].
#'
#' @section Errors:
#' Individual package install failures are caught and reported without
#' aborting the remaining installs.
#'
#' @seealso
#' [update_pip_packages()], [pip_snapshot()],
#' [install_branch()], [get_core_pagkages()]
#'
#' @examples
#' \dontrun{
#' # Interactive: prompts before installing
#' init_metapip()
#'
#' # Non-interactive: auto-install
#' init_metapip(ask = FALSE)
#'
#' # Exclude specific packages
#' init_metapip(exclude = c("pipdata", "pipfaker"))
#' }
#'
#' @export
init_metapip <- function(exclude = NA, ask = TRUE, answer = 1) {
  lock_path <- pip_lock_path()

  if (file.exists(lock_path) && nzchar(lock_path)) {
    lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
    pkgs <- get_core_pagkages(exclude = exclude)
    lock <- lock[lock$package %in% pkgs, , drop = FALSE]
    lock <- lock[!is.na(lock$sha), , drop = FALSE]

    if (nrow(lock) > 0) {
      if (ask) {
        if (interactive()) {
          answer <- utils::menu(
            choices = c("Yes", "No"),
            title = "Do you want to install the locked packages now?"
          )
        } else {
          cli::cli_alert_warning(
            "Non-interactive session: installing locked packages by default"
          )
          answer <- 1
        }
      }

      if (identical(answer, 1)) {
        cli::cli_alert_info(
          "Installing {nrow(lock)} package{?s} from the lock..."
        )
        for (i in seq_len(nrow(lock))) {
          tryCatch(
            install_branch(
              package = lock$package[i],
              branch = lock$branch[i],
              sha = lock$sha[i],
              force = FALSE
            ),
            error = function(e) {
              cli::cli_alert_danger(
                "Failed to install {.pkg {lock$package[i]}}:
                {conditionMessage(e)}"
              )
            }
          )
        }
      } else {
        cli::cli_alert_danger("Skipping installation.")
      }
    }
  } else {
    cli::cli_alert_info(
      "No PIP_LOCK found; installing at branch HEAD.
      Run {.fn pip_snapshot} to create a team lock manifest."
    )
    pkgs <- get_core_pagkages(exclude = exclude)
    branches <- get_package_current_branch(package = pkgs)

    if (ask) {
      if (interactive()) {
        answer <- utils::menu(
          choices = c("Yes", "No"),
          title = "Do you want to install all core packages
          at their branch HEAD now?"
        )
      } else {
        cli::cli_alert_warning(
          "Non-interactive session: installing packages by default"
        )
        answer <- 1
      }
    }

    if (identical(answer, 1)) {
      for (pkg in pkgs) {
        tryCatch(
          install_branch(
            package = pkg, branch = unname(branches[pkg])
          ),
          error = function(e) {
            cli::cli_alert_danger(
              "Failed to install {.pkg {pkg}}:
              {conditionMessage(e)}"
            )
          }
        )
      }
    } else {
      cli::cli_alert_danger("Skipping installation.")
    }
  }

  metapip_attach()
  invisible()
}


#' Refresh the lock manifest and install outdated packages
#'
#' @description
#' `update_pip_packages()` is the companion to [pip_snapshot()] and
#' [init_metapip()]. It:
#'
#' 1. Compares each core package's locally installed SHA against its
#'    branch HEAD on GitHub.
#' 2. Refreshes the `PIP_LOCK.csv` manifest with the resolved branch
#'    HEAD SHAs.
#' 3. Installs any outdated packages at their newly resolved SHAs (with
#'    confirmation).
#'
#' Per-package installation failures are isolated and reported in a
#' summary (N succeeded, M failed).
#'
#' @return Logical (invisibly). `TRUE` when packages were installed,
#'   `FALSE` when all packages were up-to-date or the user declined.
#'
#' @details
#' The function checks for three local states:
#' - `TRUE`: Local SHA matches branch HEAD (up-to-date).
#' - `FALSE`: Local SHA differs (outdated).
#' - `"unknown"`: No `RemoteSha` metadata (e.g., CRAN install); package
#'   is skipped with a warning.
#'
#' @seealso
#' [init_metapip()], [pip_snapshot()], [compare_sha()]
#'
#' @examples
#' \dontrun{
#' update_pip_packages()
#'
#' # Non-interactive (for CI/scripts)
#' update_pip_packages(ask = FALSE)
#' }
#'
#' @export
#' @rdname init_metapip
update_pip_packages <- function(exclude = NA, ask = TRUE, answer = 1) {
  pkgs <- get_core_pagkages(exclude = exclude)
  default_branch <- get_package_current_branch(package = pkgs)

  pkgs_vec <- mapply(compare_sha, pkgs, default_branch[pkgs],
                     SIMPLIFY = FALSE, USE.NAMES = TRUE)
  null_vec <- Filter(is.null, pkgs_vec) |>
    names()
  null_branch <- default_branch[null_vec]

  if (length(null_vec) > 0) {
    max_left_width <- max(nchar(null_vec))
    cli::cli_alert_danger(
      "The following packages do not have the corresponding branch
      available on GitHub:"
    )
    for (i in seq_along(null_vec)) {
      null_vec_padded <- sprintf("%-*s", max_left_width, null_vec[i])
      cli::cli_alert(
        "{.pkg {null_vec_padded}} -->  {.val {null_branch[i]}}"
      )
    }
  }

  missing_pkgs <- character(0)
  unknown_pkgs <- character(0)

  for (nm in names(pkgs_vec)) {
    val <- pkgs_vec[[nm]]
    if (is.null(val)) {
      next
    } else if (identical(val, "unknown")) {
      unknown_pkgs <- c(unknown_pkgs, nm)
    } else if (identical(val, FALSE)) {
      missing_pkgs <- c(missing_pkgs, nm)
    }
  }

  if (length(unknown_pkgs) > 0) {
    cli::cli_alert_warning(
      "Cannot verify SHA for {.pkg {unknown_pkgs}}; skipping
      (installed without git metadata)"
    )
  }

  # Refresh the lock manifest with resolved branch HEAD SHAs.
  lock_rows <- list()
  resolved_sha <- character(0)
  lock_skipped <- character(0)
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    brn <- unname(default_branch[pkg])
    sha <- latest_commit_for_branch(pkg, brn)$sha
    if (is.null(sha)) {
      lock_skipped <- c(lock_skipped, pkg)
      next
    }
    resolved_sha[[pkg]] <- sha
    lock_rows[[pkg]] <- data.frame(
      package = pkg, branch = brn, sha = sha,
      stringsAsFactors = FALSE
    )
  }

  if (length(lock_skipped) > 0) {
    cli::cli_alert_warning(
      "Could not resolve SHA for {.pkg {lock_skipped}};
      not written to PIP_LOCK"
    )
  }

  lock_path <- getOption("metapip.lock_path", pip_lock_path())
  if (length(lock_rows) > 0) {
    lock_df <- rowbind(lock_rows)
    if (nrow(lock_df) > 0 && nzchar(lock_path)) {
      utils::write.csv(lock_df, lock_path, row.names = FALSE)
      cli::cli_alert_info(
        "Updated {.path PIP_LOCK} - commit this change."
      )
    }
  } else {
    cli::cli_alert_warning(
      "Could not resolve any SHA; PIP_LOCK not updated"
    )
  }

  if (length(missing_pkgs) > 0) {
    cli::cli_alert_warning(
      "The following packages do not have the updated version of
      default branch installed:
      {cli::qty(length(missing_pkgs))}{.pkg {missing_pkgs}}"
    )
    if (ask) {
      if (interactive()) {
        answer <- utils::menu(
          choices = c("Yes", "No"),
          title = "Do you want to install them now?"
        )
      } else {
        cli::cli_alert_warning(
          "Non-interactive session: installing outdated packages by default"
        )
        answer <- 1
      }
    }

    if (answer == 1) {
      cli::cli_alert_info("Installing missing packages...")
      n_success <- 0L
      n_failed <- 0L
      n_total <- length(missing_pkgs)
      failed_pkgs <- character(0)

      for (pkg in missing_pkgs) {
        tryCatch(
          {
            install_branch(pkg, default_branch[pkg],
                           sha = resolved_sha[[pkg]])
            n_success <- n_success + 1L
          },
          error = function(e) {
            n_failed <<- n_failed + 1L
            failed_pkgs <<- c(failed_pkgs, pkg)
            cli::cli_alert_danger(
              "Failed to install {.pkg {pkg}}:
              {conditionMessage(e)}"
            )
          }
        )
      }

      if (n_failed > 0) {
        cli::cli_alert_warning(
          "Installed {n_success}/{n_total}; {n_failed} failed:
          {.pkg {failed_pkgs}}"
        )
      } else {
        cli::cli_alert_info(
          "Installed {n_success}/{n_total} packages successfully"
        )
      }
      cli::cli_alert_info(
        "Note: metapip installs PIP packages from GitHub branches and
         resolves non-PIP dependencies from CRAN independently. For
         coordinated dependency resolution, consider using {.pkg renv}
         as a companion tool."
      )
      return(invisible(TRUE))
    } else {
      cli::cli_alert_danger("Skipping installation.")
      return(invisible(FALSE))
    }
  }

  cli::cli_inform("All packages are up-to-date")
  return(invisible(FALSE))
}


#' Compare local installed SHA against GitHub branch HEAD
#'
#' @description
#' Checks whether the locally installed version of a package matches the
#' current HEAD of a branch on GitHub.
#'
#' @param package Character. Core PIP package name.
#' @param branch Character scalar. Branch name.
#'
#' @return
#' - `TRUE` if the local SHA matches the GitHub HEAD.
#' - `FALSE` if the local SHA differs.
#' - `"unknown"` if no `RemoteSha` metadata is found locally (e.g.,
#'   installed from CRAN).
#' - `NULL` if the GitHub branch cannot be resolved (e.g., network
#'   error or missing branch).
#'
#' @keywords internal
compare_sha <- function(package, branch) {
  out <- latest_commit_for_branch(package, branch)
  gh_sha <- out$sha

  if (is.null(gh_sha)) {
    return(NULL)
  }

  local_sha <- package |>
    utils::packageDescription(fields = "RemoteSha") |>
    suppressWarnings()

  if (is.na(local_sha)) return("unknown")

  local_sha == gh_sha
}


#' Get the list of core PIP ecosystem packages
#'
#' @description
#' Returns the character vector of core PIP package names, with optional
#' exclusions. When `exclude = NA` (the default), auto-detects if the
#' current working directory is a core PIP package and excludes it --
#' useful for package development workflows where you don't want to
#' update the package you're actively working on.
#'
#' @param exclude Exclusion specification:
#'   - `NULL`: return all core packages (no exclusions).
#'   - `NA` (default): auto-detect and exclude the package whose source
#'     is the current working directory.
#'   - Character vector: explicitly named packages to exclude.
#'
#' @return Character vector of core PIP package names, in the standard
#'   ordering.
#'
#' @section Errors:
#' Aborts if any explicitly named `exclude` package is not in the core
#' set.
#'
#' @examples
#' # All core packages
#' get_core_pagkages()
#'
#' # Exclude specific packages
#' get_core_pagkages(exclude = "pipdata")
#'
#' # Auto-detect (for package development)
#' get_core_pagkages(exclude = NA)
#'
#' @export
get_core_pagkages <- function(exclude = NULL) {
  if (is.null(exclude)) return(core)

  if (is.na(exclude)) {
    current_project <- getwd() |>
      basename()
    if (current_project %in% core) {
      return(core[!(core %in% current_project)])
    } else {
      return(core)
    }
  }

  if (all(exclude %in% core)) {
    return(core[!(core %in% exclude)])
  } else {
    wrong_exclude <- exclude[!exclude %in% core]
    cli::cli_abort(c(
      x = "package{?s} {.pkg {wrong_exclude}} {?is/are} not part of
      PIP ecosystem",
      i = "available packages are {.pkg {core}}"
    ))
  }
}

#' Get core PIP ecosystem package
#'
#' @inheritParams init_metapip
#'
#' @returns character vector with names of PIP packages
#' @note `get_core_packages` is an alias for [get_core_pagkages()]. The
#'   `get_core_pagkages` spelling (typo) is retained and deprecated for
#'   backward compatibility.
#' @export
#'
#' @examples
#' get_core_packages()
#' get_core_packages(exclude = "pipdata")
get_core_packages <- function(exclude = NULL) {
  get_core_pagkages(exclude = exclude)
}
