#' Get available branches for a particular package
#'
#' @param package one of the core package name (default "pipapi")
#'
#' @return an invisible character vector of branches
#'
#' @examples
#' \dontrun{
#'   get_branches()
#'   get_branches("pipr")
#'}
#'
#' @export
#'
get_branches <- function(package = "pipapi", display = TRUE) {
  check_package_condition(package)
  out <- gh::gh("GET /repos/PIP-Technical-Team/{repo}/branches", repo = package)
  branches <- vapply(out, `[[`, "", "name")
  if(isTRUE(display)) {
    cli::cli_h3("These are available branches for {package} package: ")
    cli::cli_ul(glue::glue("{branches}"))
  }
  return(invisible(branches))
}


#' Install branch from a package
#'
#' @param package one of the core package name (default "pipapi")
#' @param branch valid branch name (default "PROD")
#'
#' @examples
#' \dontrun{
#'   install_branch()
#'   install_branch("pipfun", "ongoing")
#'}
#'
#' @export
#'
install_branch <- function(package = "pipapi", branch = "PROD") {
  check_package_condition(package)
  assertthat::assert_that(length(branch) == 1, msg = "Please enter a single branch name.")
  br <- suppressMessages(get_branches(package))
  assertthat::assert_that(branch %in% br,
      msg = glue::glue("Not a valid branch name for the package {package}. Select one of {toString(br)}"))

  cli::cli_alert_info(glue::glue("Installing {branch} from package {package}"))
  remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{branch}"))
}


check_package_condition <- function(package) {
  assertthat::assert_that(length(package) == 1, msg = "Please enter a single package name.")
  is_core(package)
}

is_core <- function(package) {
  assertthat::assert_that(all(package %in% core), msg = glue::glue("The package is not one of {toString(core)}."))
}

