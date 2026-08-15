#' Initializes and updates the pip core packages
#'
#' @param exclude character: packages to exclude from attaching. if `getwd()` is
#'  one of the core PIP packages, that package will be excluded by default. To
#'  avoid that, set exclude to `NULL`.
#' @param ask logical. Ask the user if she wants to install outdated packages. Default TRUE
#'
#' @description Based on options() settings provides an option to download latest
#' package versions from the branch
#'
#' @returns `init_metapip()` returns invisible() output
#' @details `init_metapip()` is lock-driven: when a committed `PIP_LOCK.csv`
#'   manifest is found (via [pip_lock_path()]) it installs every package at the
#'   SHA recorded in the lock, giving team-level deterministic installs. When
#'   the lock is absent it falls back to installing each package at its branch
#'   HEAD SHA and suggests running [pip_snapshot()] to create a team lock.
#' @examples
#' \dontrun{
#'   init_metapip()
#'}
#'
#'@export
init_metapip <- function(exclude = NA,
                         ask     = TRUE,
                         answer  = 1) {
  lock_path <- pip_lock_path()

  if (file.exists(lock_path) && nzchar(lock_path)) {
    lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
    pkgs <- get_core_pagkages(exclude = exclude)
    lock <- lock[lock$package %in% pkgs, , drop = FALSE]

    if (nrow(lock) > 0) {
      if (ask) {
        if (interactive()) {
          answer <- utils::menu(
            choices = c("Yes", "No"),
            title = "Do you want to install the locked packages now?"
          )
        } else {
          cli::cli_alert_warning("Non-interactive session: installing locked packages by default")
          answer <- 1
        }
      }

      if (identical(answer, 1)) {
        cli::cli_alert_info("Installing {nrow(lock)} package{?s} from the lock...")
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
                "Failed to install {.pkg {lock$package[i]}}: {conditionMessage(e)}"
              )
            }
          )
        }
      } else {
        cli::cli_alert_danger("Skipping installation.")
      }
    }
  } else {
    cli::cli_alert_info("No PIP_LOCK found; installing at branch HEAD. Run {.fn pip_snapshot} to create a team lock manifest.")
    pkgs <- get_core_pagkages(exclude = exclude)
    branches <- get_package_current_branch(package = pkgs)
    for (pkg in pkgs) {
      tryCatch(
        install_branch(package = pkg, branch = unname(branches[pkg])),
        error = function(e) {
          cli::cli_alert_danger(
            "Failed to install {.pkg {pkg}}: {conditionMessage(e)}"
          )
        }
      )
    }
  }

  # Finally load all the packages once it is installed.
  metapip_attach()
  invisible()
}




#' Update PIP package
#'
#' @param answer numeric: Developers  argument. Only works for demonstration
#'   purposes.
#' @returns `update_pip_packages()` return logical vector. TRUE if missing package
#'   were update. FALSE if all packages are up to date of the user selects not
#'   to update.
#' @export
#' @rdname init_metapip
#'
#' @examples
#' \dontrun{
#' update_pip_packages(ask = FALSE,
#' answer = 2) # this is to make it work in examples and vignettes.
#' }
update_pip_packages <- \(exclude = NA,
                         ask = TRUE,
                         answer = 1) {
  pkgs <- get_core_pagkages(exclude = exclude)
  default_branch <- get_package_current_branch(package = pkgs)

  pkgs_vec <- mapply(compare_sha, pkgs, default_branch[pkgs],
                     SIMPLIFY = FALSE, USE.NAMES = TRUE)
  null_vec    <- Filter(is.null, pkgs_vec) |>
    names()

  null_branch <- default_branch[null_vec]

  if (length(null_vec) > 0) {
    max_left_width <- max(nchar(null_vec))
    cli::cli_alert_danger("The following packages do not have the corresponding
                          branch available on GitHub:")

    for (i in seq_along(null_vec)) {
      null_vec_padded <- sprintf("%-*s", max_left_width, null_vec[i])
      cli::cli_alert("{.pkg {null_vec_padded}} -->  {.val {null_branch[i]}}")
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
      "Cannot verify SHA for {.pkg {unknown_pkgs}}; skipping (installed without git metadata)"
    )
  }

  # Refresh the lock manifest with the resolved branch HEAD SHAs.
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
      package = pkg,
      branch = brn,
      sha = sha,
      stringsAsFactors = FALSE
    )
  }

  if (length(lock_skipped) > 0) {
    cli::cli_alert_warning(
      "Could not resolve SHA for {.pkg {lock_skipped}}; not written to PIP_LOCK"
    )
  }

  lock_df <- rowbind(lock_rows)
  lock_path <- getOption("metapip.lock_path", pip_lock_path())
  if (nrow(lock_df) > 0 && nzchar(lock_path)) {
    utils::write.csv(lock_df, lock_path, row.names = FALSE)
    cli::cli_alert_info("Updated {.path PIP_LOCK} - commit this change.")
  }

  if (length(missing_pkgs) > 0) {
    cli::cli_alert_warning(
      "The following packages do not have the updated version of default branch
      installed: {cli::qty(length(missing_pkgs))}{.pkg {missing_pkgs}}"
    )
    if (ask) {
      if (interactive()) {
        answer <- utils::menu(
          choices = c("Yes", "No"),
          title = "Do you want to install them now?"
        )
      } else {
        cli::cli_alert_warning("Non-interactive session: installing outdated packages by default")
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
            install_branch(
              pkg,
              default_branch[pkg],
              sha = resolved_sha[[pkg]]
            )
            n_success <- n_success + 1L
          },
          error = function(e) {
            n_failed <<- n_failed + 1L
            failed_pkgs <<- c(failed_pkgs, pkg)
            cli::cli_alert_danger(
              "Failed to install {.pkg {pkg}}: {conditionMessage(e)}"
            )
          }
        )
      }

      if (n_failed > 0) {
        cli::cli_alert_warning(
          "Installed {n_success}/{n_total}; {n_failed} failed: {.pkg {failed_pkgs}}"
        )
      } else {
        cli::cli_alert_info("Installed {n_success}/{n_total} packages successfully")
      }
      cli::cli_alert_info(
        "Note: metapip installs PIP packages from GitHub branches and resolves
         non-PIP dependencies from CRAN independently. For coordinated dependency
         resolution, consider using {.pkg renv} as a companion tool."
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


#' Get core PIP ecosystem package
#'
#' @inheritParams init_metapip
#'
#' @returns character vector with names of PIP packages
#' @export
#'
#' @examples
#' get_core_pagkages()
#' get_core_pagkages(exclude = "pipdata")
get_core_pagkages <- \(exclude = NULL) {

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
    cli::cli_abort(c(x = "package{?s} {.pkg {wrong_exclude}} {?is/are} not part of PIP ecosystem",
                     i = "available packages are {.pkg {core}}"))
  }

}
