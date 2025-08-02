#' Get available branches for a package
#' @description
#' Returns a vector of branch names for the package specified. If display is set to TRUE,
#' the branch names are printed in the console.
#'
#' @param package one of the core package name (default "pipapi")
#' @param display (default TRUE) do you want to display the name of the branches in the console?
#'
#' @return an invisible character vector of branches
#'
#' @examples
#' \dontrun{
#'   get_branches()
#'   get_branches("wbpip", display = FALSE)
#'}
#'
#' @export
#'
get_branches <- function(package = "pipapi", display = TRUE) {
  creds <- check_github_token()
  check_package_condition(package)
  out <- gh::gh("GET /repos/PIP-Technical-Team/{repo}/branches",
                repo = package,
                .token = creds$password)
  branches <- vapply(out, `[[`, "", "name")
  if (isTRUE(display)) {
    cli::cli_h3("These are available branches for {package} package: ")
    cli::cat_bullet(glue::glue("{branches}"))
  }
  return(invisible(branches))
}


#' Get last update time of branches in a package
#' @description
#' Returns a dataframe with information about package name, branch name, author of the last commit and
#' when was the last commit made to the branch
#'
#' @param package one of the core package (default "pipapi")
#' @param branch valid branch name for the package
#' @param display (default TRUE) do you want to display the branches in the console?
#' @return a colorDF::colorDF output with an invisible dataframe
#'
#' @examples
#' \dontrun{
#' get_branch_info()
#' get_branch_info(branch = c("PROD", "QA"), display = FALSE)
#' get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
#' }
#' @export
get_branch_info <- function(package = "pipapi", branch = NULL, display = TRUE) {
  check_github_token()
  is_core(package)
  check_package_condition(package)

  branches <- get_branches(package, display = FALSE)
  if(!is.null(branch)) {
    if(!all(branch %in% branches)) {
      cli::cli_abort("{branch} is not a correct branch name. Please use one of {toString(branches)}.")
    }
    branches <- branch
  } else {
    branches <- get_default_branch(package)
  }
  out <- lapply(cli::cli_progress_along(branches), \(x) {
    dat <- latest_commit_for_branch(package, branches[x])
    data.frame(dat$commit$author)
  }) |> rowbind()

  res <- add_vars(out, package = rep(package, nrow(out)), branch_name = branches, pos = "front") |>
    frename(last_update_time = "date", last_commit_author_name = "name") |>
    fselect(-email)

  if(isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}


#' Get details of the branch which was last updated
#' @description
#' This function is useful to get latest branch name, author of latest commit and time it was last updated.
#'
#' @inheritParams get_branch_info
#'
#' @return colorDF::colorDF output along with an invisible single row dataframe
#'
#' @examples
#' \dontrun{
#' get_latest_branch_update()
#' get_latest_branch_update("wbpip", display = FALSE)
#' }
#' @export
#'
get_latest_branch_update <- function(package = "pipapi", display = TRUE) {
  check_github_token()
  is_core(package)
  check_package_condition(package)
  # Get info about all the branches
  out <- get_branch_info(package, display = FALSE)
  # Return only the latest information
  res <- out |>
    fsubset(branch_name != "gh-pages") |>
    fmutate(last_update_time = as.POSIXct(last_update_time, format = "%Y-%m-%dT%T")) |>
    # arrange data in descending order
    roworder(-last_update_time) |>
    # Get the 1st row (latest)
    ss(1L)

  if(isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}

