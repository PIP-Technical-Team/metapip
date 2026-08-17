#' Return meta information about core packages
#'
#' @description
#' For each requested core PIP package, retrieves and displays:
#' - Number of branches.
#' - Latest release tag and release date (from GitHub releases).
#' - Branch name, author, and timestamp of the most recent commit.
#'
#' @param package Character vector. One or more core PIP package names.
#'   If `NULL` (default), all core packages are included.
#'
#' @return A `data.frame` (returned invisibly) with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{no_of_branches}{Integer. Number of branches on GitHub.}
#'     \item{latest_release_tag}{Character. Most recent release tag, or `NA`
#'       if no releases exist.}
#'     \item{latest_release_time}{Character. ISO 8601 timestamp of the latest
#'       release, or `NA`.}
#'     \item{latest_commit_branch}{Character. Branch with the most recent
#'       commit.}
#'     \item{latest_commit_author}{Character. Author of that commit.}
#'     \item{latest_commit_time}{POSIXct (UTC). Timestamp of that commit.}
#'   }
#'
#' @section Progress:
#' Uses [cli::cli_progress_along()] to show progress bars for branch
#' retrieval, release lookup, and commit resolution.
#'
#' @section Errors:
#' Aborts if any `package` is not a core PIP package.
#'
#' @seealso
#' [get_branches()], [get_latest_branch_update()]
#'
#' @examples
#' \dontrun{
#' # All packages
#' core_metadata()
#'
#' # Specific packages
#' core_metadata(c("pipapi", "wbpip"))
#' }
#'
#' @export
core_metadata <- function(package = NULL) {
  if (is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }
  cli::cli_alert_info(
    "Gathering branch information of the package"
  )
  branches <- lapply(
    cli::cli_progress_along(package),
    \(i) get_branches(package[i], display = FALSE)
  )
  no_of_branches <- lengths(branches)

  cli::cli_alert_info(
    "Gathering latest tag and published date."
  )
  latest_release <- lapply(
    cli::cli_progress_along(package),
    \(i) {
      dat <- tryCatch(
        gh::gh("GET /repos/{owner}/{repo}/releases/latest",
               owner = "PIP-Technical-Team",
               repo = package[i]),
        error = \(err) data.frame(tag_name = NA, published_at = NA)
      )
      c(dat$tag_name, dat$published_at)
    }
  )

  cli::cli_alert_info("Gathering latest branch information")
  latest_commit <- lapply(
    cli::cli_progress_along(package),
    \(i) get_latest_branch_update(package[i], display = FALSE)
  )

  out <- data.frame(
    package,
    no_of_branches,
    latest_release_tag = sapply(latest_release, `[[`, 1),
    latest_release_time = sapply(latest_release, `[[`, 2),
    latest_commit_branch = sapply(latest_commit, `[[`, "branch_name"),
    latest_commit_author = sapply(
      latest_commit, `[[`, "last_commit_author_name"
    ),
    latest_commit_time = as.POSIXct(
      sapply(latest_commit, `[[`, "last_update_time"), tz = "UTC"
    )
  )
  print(colorDF::colorDF(out))
  return(invisible(out))
}


#' Get the latest commit for a specific branch (internal)
#'
#' @description
#' Fetches the latest commit object for a given branch of a package from
#' the GitHub API. Returns a fallback list of `NA` values on error so
#' callers can handle missing branches gracefully.
#'
#' @param package Character. Core PIP package name.
#' @param branch Character scalar. Branch name. No default; must be
#'   provided explicitly by internal callers.
#'
#' @return A list (the raw GitHub API response) containing at minimum:
#'   `$sha` (the commit SHA) and `$commit$author` (with `$date` and `$name`).
#'   On error, returns a list with `$commit$author$date` = `NA` and
#'   `$commit$author$name` = `NA`.
#'
#' @keywords internal
latest_commit_for_branch <- function(package, branch) {
  tryCatch(
    gh::gh("GET /repos/{owner}/{repo}/commits/{branch}",
           owner = "PIP-Technical-Team",
           repo = package, branch = branch),
    error = function(err) {
      list(commit = list(author = list(date = NA, name = NA)))
    }
  )
}