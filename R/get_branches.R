#' Get available branches for a particular package
#'
#' @param package one of the core package name (default "pipapi")
#' @param display (default TRUE) do you want to display the branches in the console?
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
  check_github_token()
  check_package_condition(package)
  out <- gh::gh("GET /repos/PIP-Technical-Team/{repo}/branches", repo = package)
  branches <- vapply(out, `[[`, "", "name")
  if(isTRUE(display)) {
    cli::cli_h3("These are available branches for {package} package: ")
    cli::cat_bullet(glue::glue("{branches}"))
  }
  return(invisible(branches))
}


#' Install branch from a package
#'
#' @param package one of the core package name (default "pipapi")
#' @param branch valid branch name (default "DEV")
#'
#' @examples
#' \dontrun{
#'   install_branch()
#'   install_branch("pipfun", "ongoing")
#'}
#'
#' @export
#'
install_branch <- function(package = "pipapi", branch = "DEV") {
  check_github_token()
  check_package_condition(package)
  if(length(branch) != 1L) cli::cli_abort("Please enter a single branch name.")
  br <- get_branches(package, display = FALSE)

  if(!branch %in% br) cli::cli_abort("Not a valid branch name for the package {package}. Select one of {toString(br)}")
  cli::cli_alert_info(glue::glue("Installing branch {branch} from package {package}"))
  remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{branch}"))
}


#' Get last update time of branches in a specific package
#'
#' @param package one of the core package (default "pipapi")
#' @param branch valid branch name for the specified package
#'
#' @return a dataframe
#'
#' @examples
#' \dontrun{
#' get_branch_info()
#' get_branch_info(branch = c("PROD", "QA"))
#' get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
#' }
#' @export
get_branch_info <- function(package = "pipapi", branch = NULL) {
  check_github_token()
  is_core(package)
  check_package_condition(package)

  branches <- get_branches(package, display = FALSE)
  if(!is.null(branch)) {
    if(!all(branch %in% branches)) {
      cli::cli_abort("{branch} is not a correct branch name. Please use one of {toString(branches)}.")
    }
    branches <- branch
  }
  out <- purrr::map_df(branches, \(x) {
    dat <- latest_commit_for_branch(package, x)
    dat$commit$author
  })
  res <- cbind(package, branch_name = branches, out) %>%
    dplyr::rename(last_update_time = "date", last_commit_author_name = "name") %>%
    dplyr::select(-"email")
  res
}


#' Get details of the branch which was last updated
#'
#' @inheritParams get_branch_info
#'
#' @return single row dataframe
#'
#' @examples
#' \dontrun{
#' get_latest_branch_update()
#' get_latest_branch_update("pipr")
#' }
#' @export
#'
get_latest_branch_update <- function(package = "pipapi") {
  check_github_token()
  is_core(package)
  check_package_condition(package)
  # Get info about all the branches
  out <- get_branch_info(package)
  # Return only the latest information
  out %>%
    dplyr::filter(.data$branch_name != "gh-pages") %>%
    dplyr::mutate(last_update_time = as.POSIXct(.data$last_update_time, format = "%Y-%m-%dT%T")) %>%
    dplyr::arrange(.data$last_update_time) %>%
    dplyr::slice_tail(n = 1L)
}

