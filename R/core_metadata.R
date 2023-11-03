#' Return meta information about core packages
#'
#' @param package One (or more) core packages, if NULL shows information about all of them.
#' @return a knitr::kable output with details about the core packages
#' @export
#'
core_metadata <- function(package = NULL) {
  check_github_token()

  if(is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }
  branches <- lapply(package, get_branches, display = FALSE)
  no_of_branches <- lengths(branches)
  latest_release <- lapply(package, \(x) {
    dat <- tryCatch({
      gh::gh("GET /repos/{owner}/{repo}/releases/latest",
          owner = "PIP-Technical-Team", repo = x)
    }, error = function(err) data.frame(tag_name = NA, published_at = NA))
    c(dat$tag_name, dat$published_at)
  })

  latest_commit <- lapply(package, \(x) {
    dat <- tryCatch({
      gh::gh("GET /repos/{owner}/{repo}/commits/{branch}", owner = "PIP-Technical-Team",
                  repo = x, branch = "PROD")
  }, error = function(err) {
    list(commit = list(author = list(date = NA, name = NA)))
    })
    dat$commit$author
  })
  out <- data.frame(package, no_of_branches, latest_release_tag = sapply(latest_release, `[[`, 1),
                    latest_release_time = sapply(latest_release, `[[`, 2),
                    latest_commit_author = sapply(latest_commit, `[[`, "name"),
                    latest_commit_time = sapply(latest_commit, `[[`, "date"))
  knitr::kable(out)
}
