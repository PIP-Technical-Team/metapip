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
#' @examples
#' \dontrun{
#'   init_metapip()
#'}
#'
#'@export
init_metapip <- function(exclude = NA,
                         ask     = TRUE,
                         answer  = 1) {
  update_pip_packages(exclude = exclude,
                      ask = ask,
                      answer  = answer)
  # Finally load all the packages once it is installed.
  metapip_attach()
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
            install_branch(pkg, default_branch[pkg])
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
