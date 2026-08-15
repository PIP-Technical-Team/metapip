#' Install latest branch from a package
#' @description
#' Based on the last commit of the branch it installs the latest branch of the package.
#' This is a developer-only tool: it deliberately bypasses the team
#' `PIP_LOCK` manifest and installs the live HEAD of each branch. Use
#' [pip_snapshot()] + [init_metapip()] for team-consistent installs.
#'
#'
#' @param package one (or more) of core packages. default NULL would install latest branch for all packages
#'
#' @return NULL
#' @examples
#' \dontrun{
#'   install_latest_branch()
#'   install_latest_branch(c("pipfun", "pipapi"))
#'}
#' @export
#'
install_latest_branch <- function(package = NULL) {
  check_github_token()
  cli::cli_alert_warning("install_latest_branch() bypasses the team lockfile. Use {.fn pip_snapshot} + {.fn init_metapip} for team-consistent installs.")
  if(!is.null(package)) is_core(package)
  else package <- core
  dat <- lapply(cli::cli_progress_along(package),
                \(i) {
                  get_latest_branch_update(package[i], display = FALSE)
                  }) |>
    rowbind()

  for (i in seq_len(nrow(dat))) {
    pkg <- dat$package[i]
    brn <- dat$branch_name[i]
    sha_result <- compare_sha(pkg, brn)

    if (identical(sha_result, TRUE)) {
      cli::cli_alert_info("{.pkg {pkg}} already at HEAD of {.field {brn}}; skipping")
      next
    }

    suppressMessages(install_branch(pkg, brn, force = TRUE))
  }

  NULL
}


#' Install one (or more) pip core packages from a branch.
#'
#' @description
#' This installs packages like pipapi, pipaux, pipload, wbpip, pipfun, pipdata from a branch
#'
#' @param package one (or more) of the core package name, if NULL all the core packages are installed from the branch
#' @param branch valid branch name (default "PROD")
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' install_pip_packages(branch = "test")
#' install_pip_packages(package = "wbpip", branch = "DEV")
#' }
#'
#' @export
#'
install_pip_packages <- function(package = NULL, branch = NULL) {
  check_github_token()
  if (is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }

  if(is.null(branch)) {
    branch <- get_package_current_branch(package = package)
  }
  lapply(cli::cli_progress_along(package),
         \(x) {
           tryCatch(
             expr = {
               pgk <- package[x]
               brn <- if (is.null(names(branch))) branch[1] else unname(branch[pgk])
               install_branch(package = pgk, branch = brn)
             },
             error = function(e) {
               cli::cli_alert_danger("package {.pkg {pgk}} could not be installed")
             },
             # end of error section

             warning = function(w) {
               cli::cli_alert_warning("package {.pkg {pgk}} produces warnings during installation")
             }
           ) # End of trycatch
         })
  return(invisible(NULL))
}


#' Install branch from a package
#'
#' @param package one of the core package name (default "pipapi")
#' @param branch valid branch name (default "PROD")
#' @param force logical: when TRUE, bypasses SHA pinning and the idempotency
#'   check, installing the live branch HEAD (`@<branch>`) instead. Intended for
#'   developers. Default FALSE.
#' @param sha character: optional commit SHA to install at. When supplied it
#'   overrides the resolved branch HEAD SHA. When NULL (default) and
#'   `force = FALSE`, the branch HEAD SHA is resolved and pinned.
#'
#' @return invisible NULL, or the result of `remotes::install_github()` when an
#'   install is performed
#'
#' @examples
#' \dontrun{
#'   install_branch()
#'   install_branch("pipfun", "ongoing")
#'   install_branch("pipfun", "ongoing", force = TRUE)
#'   install_branch("pipfun", "ongoing", sha = "a1b2c3d")
#'}
#'
#' @export
#'
install_branch <- function(package = "pipapi", branch = NULL, force = FALSE, sha = NULL) {
  check_github_token()
  check_package_condition(package)
  if(is.null(branch)) branch <- get_package_current_branch(package = package)
  if(length(branch) != 1L) cli::cli_abort("Please enter a single branch name.")
  detach_package(package)

  br <- get_branches(package, display = FALSE)

  if(!branch %in% br) cli::cli_abort("Not a valid branch name for the package {package}. Select one of {toString(br)}")

  if (isTRUE(force)) {
    cli::cli_alert_warning("force = TRUE bypasses the team lock; installing live HEAD of {.field {branch}}")
    cli::cli_alert_info(glue::glue("Installing branch {branch} from package {package}"))
    return(remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{branch}")))
  }

  target_sha <- sha
  if (is.null(target_sha)) {
    target_sha <- latest_commit_for_branch(package, branch)$sha
  }

  if (is.null(target_sha)) {
    cli::cli_abort("Could not resolve SHA for {.pkg {package}}@{branch}. Check network access or pass {.arg sha} explicitly.")
    return(invisible(NULL))
  }

  local_sha <- utils::packageDescription(package, fields = "RemoteSha") |>
    suppressWarnings()

  if (!is.na(local_sha) && identical(local_sha, target_sha)) {
    cli::cli_alert_info("{.pkg {package}} already at SHA {.code {target_sha}}; skipping")
    return(invisible(NULL))
  }

  cli::cli_alert_info(glue::glue("Installing branch {branch} from package {package} at {target_sha}"))
  remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{target_sha}"))
}


