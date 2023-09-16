#' Get available branches for a particular package
#'
#' @param package one of the core package name
#'
#' @return
#' @export
#'
get_branches <- function(package = "pipapi") {
  assertthat::assert_that(length(package) == 1, msg = "Please enter a single package name.")
  assertthat::assert_that(package %in% core, msg = glue::glue("The package is not one of {toString(core)}."))

  out <- gh::gh("GET /repos/{owner}/{repo}/branches", owner = getOption("pipfun.ghowner"), repo = "pipaux")
  branches <- vapply(out, `[[`, "", "name")
  cli::cli_h3("These are available branches for {package} package: ")
  cli::cli_ul(glue::glue("{branches}"))
}
