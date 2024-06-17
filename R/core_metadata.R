#' Return meta information about core packages
#'
#' @param package One (or more) core packages, if NULL shows information about all of them.
#' @return a knitr::kable output with details about the core packages
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
  branches <- lapply(package, get_branches, display = FALSE)
  no_of_branches <- lengths(branches)

  cli::cli_alert_info("Gathering latest tag and published date.")
  latest_release <- lapply(package, \(x) {
    dat <- tryCatch({
      gh::gh("GET /repos/{owner}/{repo}/releases/latest",
          owner = "PIP-Technical-Team", repo = x)
    }, error = function(err) data.frame(tag_name = NA, published_at = NA))
    c(dat$tag_name, dat$published_at)
  })

  cli::cli_alert_info("Gathering latest branch information")
  latest_commit <- lapply(package, get_latest_branch_update)

  out <- data.frame(package, no_of_branches, latest_release_tag = sapply(latest_release, `[[`, 1),
                    latest_release_time = sapply(latest_release, `[[`, 2),
                    latest_commit_branch = sapply(latest_commit, `[[`, "branch_name"),
                    latest_commit_author = sapply(latest_commit, `[[`, "last_commit_author_name"),
                    latest_commit_time = as.POSIXct(sapply(latest_commit, `[[`, "last_update_time")))
  print(knitr::kable(out))
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
