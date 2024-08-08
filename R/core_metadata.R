#' Return meta information about core packages
#' @description
#' Returns information like name of the package, number of branches it has, last release tag,
#' last release time, last commit author etc.
#'
#'
#' @param package One (or more) core packages, if NULL shows information about all of them.
#' @return a colorDF::colorDF output with details about the core packages
#' @examples
#' \dontrun{
#' core_metadata()
#' core_metadata(c("pipapi", "wbpip"))
#' }
#' @export
#'
core_metadata <- function(package = NULL) {
  check_github_token()

  if(is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }
  cli::cli_alert_info("Gathering branch information of the package")
  branches <- lapply(cli::cli_progress_along(package),
                     \(i) get_branches(package[i], display = FALSE)
                     )
  no_of_branches <- lengths(branches)

  cli::cli_alert_info("Gathering latest tag and published date.")
  latest_release <- lapply(cli::cli_progress_along(package),
                           \(i) {
    dat <- tryCatch({
      gh::gh("GET /repos/{owner}/{repo}/releases/latest",
          owner = "PIP-Technical-Team",
          repo = package[i])
      }, error = \(err) data.frame(tag_name = NA, published_at = NA)
      )

    c(dat$tag_name, dat$published_at)
  })

  cli::cli_alert_info("Gathering latest branch information")

  latest_commit <- lapply(cli::cli_progress_along(package),
                          \(i) {
                            get_latest_branch_update(package[i], display = FALSE)
                            }
                          )

  out <- data.frame(package, no_of_branches, latest_release_tag = sapply(latest_release, `[[`, 1),
                    latest_release_time = sapply(latest_release, `[[`, 2),
                    latest_commit_branch = sapply(latest_commit, `[[`, "branch_name"),
                    latest_commit_author = sapply(latest_commit, `[[`, "last_commit_author_name"),
                    latest_commit_time = as.POSIXct(sapply(latest_commit, `[[`, "last_update_time")))
  print(colorDF::colorDF(out))
  return(invisible(out))
}


latest_commit_for_branch <- function(package, branch) {
  tryCatch({
    gh::gh("GET /repos/{owner}/{repo}/commits/{branch}", owner = "PIP-Technical-Team",
           repo = package, branch = branch)
  }, error = function(err) {
    list(commit = list(author = list(date = NA, name = NA)))
  })
}
