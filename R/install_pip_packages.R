#' Install the live HEAD of a branch (developer-only)
#'
#' @description
#' Installs the current live HEAD of the most recently updated branch for
#' each requested package. This is a **developer-only** utility that
#' deliberately bypasses the team `PIP_LOCK` manifest.
#'
#' For reproducible team installs, use [pip_snapshot()] to create a lock
#' manifest, then [init_metapip()] to install from it.
#'
#' @param package Character vector. One or more core PIP package names.
#'   If `NULL` (default), all core packages are installed.
#'
#' @return `NULL` invisibly. Called for its side effect of installing
#'   packages.
#'
#' @section Behaviour:
#' Packages already at the HEAD SHA of their latest branch are skipped
#' with an informational message.
#'
#' @section Warning:
#' This function warns on entry that it bypasses the team lockfile.
#'
#' @seealso
#' [install_branch()], [install_pip_packages()],
#' [pip_snapshot()], [init_metapip()]
#'
#' @examples
#' \dontrun{
#' install_latest_branch()
#' install_latest_branch(c("pipfun", "pipapi"))
#' }
#'
#' @export
install_latest_branch <- function(package = NULL) {
  check_github_token()
  cli::cli_alert_warning(
    "install_latest_branch() bypasses the team lockfile.
    Use {.fn pip_snapshot} + {.fn init_metapip} for team-consistent installs."
  )
  if (!is.null(package)) is_core(package)
  else package <- core
  dat <- lapply(
    cli::cli_progress_along(package),
    \(i) get_latest_branch_update(package[i], display = FALSE)
  ) |>
    rowbind()

  for (i in seq_len(nrow(dat))) {
    pkg <- dat$package[i]
    brn <- dat$branch_name[i]
    sha_result <- compare_sha(pkg, brn)

    if (identical(sha_result, TRUE)) {
      cli::cli_alert_info(
        "{.pkg {pkg}} already at HEAD of {.field {brn}}; skipping"
      )
      next
    }

    suppressMessages(install_branch(pkg, brn, force = TRUE))
  }

  NULL
}


#' Install one or more PIP core packages from a branch
#'
#' @description
#' Convenience wrapper that installs multiple core PIP packages from a
#' specified branch (or each package's configured default branch). For a
#' fresh R installation, running `install_pip_packages()` with no arguments
#' sets up all core packages.
#'
#' @param package Character vector. One or more core PIP package names.
#'   If `NULL` (default), all core packages are installed.
#' @param branch Character scalar or named character vector. The branch to
#'   install from. When `NULL` (default), each package's configured default
#'   branch (via [get_package_current_branch()]) is used. When a scalar, all
#'   packages are installed from that single branch.
#'
#' @return `NULL` invisibly.
#'
#' @section Side effects:
#' Each package is installed via [install_branch()] with SHA pinning.
#' Failures for individual packages are caught and reported without
#' aborting the remaining installs.
#'
#' @seealso
#' [install_branch()], [init_metapip()]
#'
#' @examples
#' \dontrun{
#' # Install all packages from their default branches
#' install_pip_packages()
#'
#' # Install specific packages from a branch
#' install_pip_packages(c("pipapi", "wbpip"), branch = "test")
#' }
#'
#' @export
install_pip_packages <- function(package = NULL, branch = NULL) {
  check_github_token()
  if (is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }

  if (is.null(branch)) {
    branch <- get_package_current_branch(package = package)
  }
  lapply(cli::cli_progress_along(package),
         \(x) {
           tryCatch(
             expr = {
               pgk <- package[x]
               brn <- if (is.null(names(branch))) {
                 branch[1]
               } else {
                 unname(branch[pgk])
               }
               install_branch(package = pgk, branch = brn)
             },
             error = function(e) {
               cli::cli_alert_danger(
                 "package {.pkg {pgk}} could not be installed"
               )
             },
             warning = function(w) {
               cli::cli_alert_warning(
                 "package {.pkg {pgk}} produces warnings during installation"
               )
             }
           )
         })
  return(invisible(NULL))
}


#' Install a specific branch of a PIP package
#'
#' @description
#' Installs a single core PIP package from a specified branch on GitHub.
#' By default (`force = FALSE`), the branch HEAD SHA is resolved and pinned,
#' giving session-level deterministic installs. If the package is already at
#' the target SHA, installation is skipped.
#'
#' @param package Character scalar. Core PIP package name. Defaults to
#'   `"pipapi"`.
#' @param branch Character scalar. Branch to install. When `NULL`
#'   (default), the package's configured default branch is used.
#' @param force Logical. If `TRUE`, bypasses SHA pinning and the
#'   idempotency check, installing the live branch HEAD (`@<branch>`)
#'   directly. Intended for developers. Default `FALSE`.
#' @param sha Character scalar. Optional commit SHA to install at. When
#'   supplied, overrides the resolved branch HEAD SHA. When `NULL`
#'   (default, and `force = FALSE`), the branch HEAD SHA is resolved
#'   automatically.
#'
#' @return `NULL` invisibly when installation is skipped (already at target
#'   SHA), or the result of [remotes::install_github()] when an install is
#'   actually performed.
#'
#' @section SHA pinning:
#' When `force = FALSE` (the default):
#' 1. Resolves the branch HEAD SHA from GitHub.
#' 2. Checks if the locally installed package already matches that SHA.
#' 3. If they match, skips installation (idempotent). Otherwise, installs
#'    at the resolved (or explicitly provided) SHA.
#'
#' This ensures deterministic installs: running the same `install_branch()`
#' call twice will not re-install.
#'
#' @section Errors:
#' - Aborts if `package` is not a core PIP package.
#' - Aborts if `branch` is not in the package's available branches.
#' - Aborts if the SHA cannot be resolved (network error or invalid ref).
#'
#' @seealso
#' [install_pip_packages()], [install_latest_branch()],
#' [get_branches()]
#'
#' @examples
#' \dontrun{
#' # Install default branch of pipapi
#' install_branch()
#'
#' # Install a specific branch
#' install_branch("pipfun", "ongoing")
#'
#' # Force install (bypass SHA pinning)
#' install_branch("pipfun", "ongoing", force = TRUE)
#'
#' # Install at a specific SHA
#' install_branch("pipfun", "ongoing", sha = "a1b2c3d")
#' }
#'
#' @export
install_branch <- function(package = "pipapi", branch = NULL,
                            force = FALSE, sha = NULL) {
  check_github_token()
  check_package_condition(package)
  if (is.null(branch)) {
    branch <- get_package_current_branch(package = package)
  }
  if (length(branch) != 1L) {
    cli::cli_abort("Please enter a single branch name.")
  }
  detach_package(package)

  br <- get_branches(package, display = FALSE)

  if (!branch %in% br) {
    cli::cli_abort(
      "Not a valid branch name for the package {package}.
      Select one of {toString(br)}"
    )
  }

  if (isTRUE(force)) {
    cli::cli_alert_warning(
      "force = TRUE bypasses the team lock; installing live HEAD of
      {.field {branch}}"
    )
    cli::cli_alert_info(
      glue::glue(
        "Installing branch {branch} from package {package}"
      )
    )
    return(remotes::install_github(
      glue::glue("PIP-Technical-Team/{package}@{branch}")
    ))
  }

  target_sha <- sha
  if (is.null(target_sha)) {
    target_sha <- latest_commit_for_branch(package, branch)$sha
  }

  if (is.null(target_sha)) {
    cli::cli_abort(
      "Could not resolve SHA for {.pkg {package}}@{branch}.
      Check network access or pass {.arg sha} explicitly."
    )
  }

  local_sha <- utils::packageDescription(package, fields = "RemoteSha") |>
    suppressWarnings()

  if (!is.na(local_sha) && identical(local_sha, target_sha)) {
    cli::cli_alert_info(
      "{.pkg {package}} already at SHA {.code {target_sha}}; skipping"
    )
    return(invisible(NULL))
  }

  cli::cli_alert_info(
    glue::glue(
      "Installing branch {branch} from package {package} at {target_sha}"
    )
  )
  remotes::install_github(
    glue::glue("PIP-Technical-Team/{package}@{target_sha}")
  )
}